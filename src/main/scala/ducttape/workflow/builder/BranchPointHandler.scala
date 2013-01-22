package ducttape.workflow.builder

import ducttape.syntax.AbstractSyntaxTree.ASTType
import ducttape.syntax.AbstractSyntaxTree.BranchPointDef
import ducttape.syntax.AbstractSyntaxTree.ConfigVariable
import ducttape.syntax.AbstractSyntaxTree.Literal
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.AbstractSyntaxTree.Sequence
import ducttape.syntax.AbstractSyntaxTree.SequentialBranchPoint
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.AbstractSyntaxTree.Unbound
import ducttape.syntax.Namespace
import ducttape.syntax.FileFormatException
import ducttape.workflow.Branch
import ducttape.workflow.BranchFactory
import ducttape.workflow.BranchPointFactory
import ducttape.workflow.SpecTypes.SpecPair
import grizzled.slf4j.Logging

import collection.Map

class BranchPointHandler(
             confSpecs:          Map[String,Spec],
             branchPointFactory: BranchPointFactory,
             branchFactory:      BranchFactory
      ) extends Logging {

  private[builder] def resolveBranchPoint[SpecT <: Spec]
    (taskDef: TaskDef, origSpec: Spec, taskMap: Map[Namespace,TaskDef], isParam: Boolean)
    (prevTree: BranchTree, branchHistory: Seq[Branch], curTask: Option[TaskDef],
     curSpec: Spec, prevGrafts: Seq[Branch])
    (resolveVarFunc: (TaskDef, Map[Namespace,TaskDef], Spec, Option[TaskDef]) => SourceSpecInfo ) 
//      (implicit state: State[SpecT])
{
//    import state._   
    
    debug("Task=%s: Recursively resolving potential branch point: %s @ %s".format(taskDef, curSpec, curTask))

    implicit val state = new State(taskDef, origSpec, taskMap, isParam,
                                   prevTree, branchHistory, curTask, curSpec, prevGrafts,
                                   resolveVarFunc)
    
    // TODO: Un-nest these methods

    // note: you can skip these method definitions to the match-case below
    // to first see their usage

    curSpec.rval match {
      // we found another branch point -- keep recursing
      case bp @ BranchPointDef(branchPointNameOpt, branchSpecz) => {
        val branchSpecs = branchSpecz
        val branchPointName = getName(branchPointNameOpt, bp)
        handleBranchPoint(branchPointName, branchSpecs)
      }
      // we found another branch point -- keep recursing
      case bp @ SequentialBranchPoint(branchPointNameOpt: Option[_], sequence: Sequence) => {
        val branchPointName = getName(branchPointNameOpt, bp)
        val branchSpecs = generateBranchSpecs(branchPointName, sequence.start, sequence.end, sequence.increment)
        debug("Generated sequence branch specs: " + branchSpecs)
        handleBranchPoint(branchPointName, branchSpecs, isFromSeq=true)
      }
      
      // TODO: match a new GlobbedBranchPoint (or something like that) here
      //       The GlobbedBranchPoint will need to come from the parser that returns the AST
      //       We need to generate one SpecPair per branch of the globbed branch point
      //       In each of these SpecPairs, the input spec will be the same lhs.
      //       We will also need to modify TaskEnvironment.
            
      // we found a global config variable -- we first have to look inside
      // that variable to determine what to do next
      case ConfigVariable(varName) => {
        confSpecs.get(varName) match {
          case Some(confSpec) => {
            resolveBranchPoint(taskDef, origSpec, taskMap, isParam)(
              prevTree, branchHistory, curTask, confSpec, prevGrafts)(resolveVarFunc)
          }
          case None => throw new FileFormatException(
            "Config variable %s required by input %s at task %s not found in config file.".
              format(varName, curSpec.name, taskDef.name),
            curSpec)
        }
      }
      // ha! we finally found a spec that isn't a branch point
      // we're done traversing through nested branch points
      // see if we can resolve the output spec or literal spec associated with our task's spec
      case _ => handleNonBranchPoint()
    }
  } // handleBranchPoint -- TODO: This method is obese, split it up.
  
  
  // create an internal node in the branch tree
  private def handleBranchPoint(
          branchPointName: String,
          branchSpecs: Seq[Spec],
          isFromConfig: Boolean = false,
          isFromSeq: Boolean = false)(
              implicit state: State) {

    import state._
    
    val branchPoint = branchPointFactory.get(branchPointName)

        for ( (branchSpec, idx) <- branchSpecs.zipWithIndex) {

          debug("Task=%s; bp=%s; new branchSpec=%s".format(taskDef, branchPoint, branchSpec))
          val isBaseline = (idx == 0)
          val branch = branchFactory.get(branchSpec.name, branchPoint, isBaseline)

          // Statically enforce grafts that happen within a nested
          // branch point (occur in our branch tree)
          // This is especially important for grafted params, since these
          // grafts are enforced exclusively at compile-time (now)
          // and it just helps keep the generated graphs a bit cleaner
          val graftsOk = prevGrafts.find { graft: Branch => graft.branchPoint == branch.branchPoint } match {
          case Some(graft) => {
            debug("Task=%s; Enforcing static graft %s == %s".format(taskDef, graft, branch))
            graft == branch
          }
          case None => true
          }

          if (graftsOk) {
            val newHistory = branchHistory ++ Seq(branch)
                // check if this branch assignment spec's rvalue defines another branch point
                // Note that it might not even be an assignment, since we allow anonymous branch specs
                branchSpec.rval match {
              // we found a nested branch point -- recursively call resolveBranchPoint()
                case BranchPointDef(_,_) => {
                  // no grafts needed/possible here
                  val bpTreeData: BranchPointTreeData = prevTree.getOrAdd(branchPoint, prevGrafts)
                      val branchTree: BranchTree = bpTreeData.tree.getOrAdd(branch)

                      debug("Task=%s; branchPoint=%s; Spec %s contains a nested branch point. Branch tree is %s for branch spec.".format(taskDef, branchPoint, branchSpec, branchTree.branch))
                      resolveBranchPoint(taskDef, origSpec, taskMap, isParam)(
                          branchTree, newHistory, curTask, branchSpec, prevGrafts)(resolveVarFunc)
                }
                // no nested branch point, go ahead and resolve which spec we link to
                case _ => {
                  // if it's not a branch point def, we must immediately get its grafts
                  // to get them in the right place in the tree
                  val sourceSpecInfo = resolveVarFunc(taskDef, taskMap, branchSpec, curTask)
//                  val (srcSpec, srcTaskDefOpt, myGrafts) = resolveVarFunc(taskDef, taskMap, branchSpec, curTask)
                      // only use *prevGrafts* (the grafts needed to arrive at this BP)
                      // instead of prevGrafts++myGrafts
                      val bpTreeData: BranchPointTreeData = prevTree.getOrAdd(branchPoint, prevGrafts)
                      val branchTree: BranchTree = bpTreeData.tree.getOrAdd(branch)

                      debug("Task=%s; branchPoint=%s; Spec %s does not contain a nested branch point. Branch tree is %s for branch spec.".format(taskDef, branchPoint, branchSpec, branchTree.branch))
//                      handleNonBranchPointHelper(branchSpec, branchTree)(srcSpec, srcTaskDefOpt, myGrafts)
                      handleNonBranchPointHelper(branchSpec, branchTree)(sourceSpecInfo)
                }
            }
          }
        }
  }

    // create a leaf node in the branch tree
    private def handleNonBranchPoint()(
              implicit state: State) {
      
      import state._
      
      // the srcTaskDef is only specified if it implies a temporal dependency (i.e. not literals)
      val sourceSpecInfo = resolveVarFunc(taskDef, taskMap, curSpec, curTask)
//      val (srcSpec, srcTaskDefOpt, myGrafts) = resolveVarFunc(taskDef, taskMap, curSpec, curTask)
//      handleNonBranchPointHelper(curSpec, prevTree)(srcSpec, srcTaskDefOpt, myGrafts)
      handleNonBranchPointHelper(curSpec, prevTree)(sourceSpecInfo)      
    }  
  
    // we've gotten to the right place in the branch point tree
    // and we've resolved a potential spec we should maybe link to
    // first, we check if it is a branch point or not -- if not, we just add it
    private def handleNonBranchPointHelper(theSpec: Spec, myBranchTree: BranchTree)
                                          (sourceSpecInfo: SourceSpecInfo)(
              implicit state: State) {
//        (srcSpec: Spec, srcTaskDefOpt: Option[TaskDef], myGrafts: Seq[Branch])(
            


      import state._
      
      // use myGrafts instead of myGrafts ++ prevGrafts
      val allGrafts: Seq[Branch] = sourceSpecInfo.grafts
      debug("Resolved %s to potentially non-branch spec %s @ %s with new grafts %s (all grafts: %s)".format(
        origSpec, sourceSpecInfo.srcSpec, sourceSpecInfo.srcTask, sourceSpecInfo.grafts, allGrafts))
      
      // resolveVarFunc might have returned a branch point to us
      // if it traced back to a parent's parameter, which is itself a branch point
      // if that's the case, we should continue recursing. otherwise, just add.
      sourceSpecInfo.srcSpec.rval match {
        // phew, we got either a literal (filename or parameter value)
        // or an unbound output (ducttape will determine the output file path of this task
        // that will be run in the future)
        case Literal(_) | Unbound() => {

          // TODO: We need to remove this branch from the history/branch tree entirely...
          // this potentially removes phantoms even
          // i.e. don't modify the grafts, modify the tree...
          // TODO: Figure out what the above comment means.

          // store specs at this branch nesting along with its grafts
          // this is used by the MetaHyperDAG to determine structure and temporal dependencies
          val data: TerminalData = myBranchTree.getOrAdd(sourceSpecInfo.srcTask, allGrafts, isParam)
          data.specs += new SpecPair(origSpec, sourceSpecInfo.srcTask, sourceSpecInfo.srcSpec, isParam)
          
          trace("Task=%s; Resolved %s --> %s (%s); Grafts are: %s".format(taskDef, origSpec, sourceSpecInfo.srcSpec, sourceSpecInfo.srcTask, allGrafts))
        }
        case _ => {
          // not a literal -- keep tracing through branch points
          // note that we now recurse with a different curTask
          resolveBranchPoint(taskDef, origSpec, taskMap, isParam)(
            myBranchTree, branchHistory, sourceSpecInfo.srcTask, sourceSpecInfo.srcSpec, allGrafts)(resolveVarFunc)
        }
      }
    }
    
    // generate the literal specs implied by a sequence branch point
    private def generateBranchSpecs(bpName: String, start: BigDecimal, end: BigDecimal, inc: BigDecimal): Seq[LiteralSpec] = {
      for (value <- start to end by inc) yield {
        debug("Generating literal spec from sequential branch element: %s".format(value))
        new LiteralSpec(value.toString, new Literal(value.toString), dotVariable=false)
      }
    }
    
    private def getName(branchPointNameOpt: Option[String], astElem: ASTType) = branchPointNameOpt match {
      case Some(name) => name
      case None => throw new FileFormatException("Branch point name is required", astElem)
    }    
}