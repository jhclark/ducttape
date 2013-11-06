package ducttape.workflow.builder

import ducttape.syntax.Namespace
import ducttape.syntax.AbstractSyntaxTree.BranchGraft
import ducttape.syntax.AbstractSyntaxTree.BranchGraftElement
import ducttape.syntax.AbstractSyntaxTree.BranchPointDef
import ducttape.syntax.AbstractSyntaxTree.ConfigVariable
import ducttape.syntax.AbstractSyntaxTree.Literal
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.AbstractSyntaxTree.SequentialBranchPoint
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.AbstractSyntaxTree.TaskVariable
import ducttape.syntax.AbstractSyntaxTree.Unbound
import ducttape.syntax.FileFormatException
import ducttape.workflow.Branch
import ducttape.workflow.BranchFactory
import ducttape.workflow.BranchPoint
import ducttape.workflow.BranchPointFactory
import ducttape.workflow.NoSuchBranchException

import grizzled.slf4j.Logging

import collection.Map

object VariableHandler extends Logging {

  /**
   * the resolved Spec is guaranteed to be a literal for params
   */
  private[builder] def resolveParam(
                            taskDef: TaskDef, 
                            taskMap: Map[Namespace,TaskDef], 
                               spec: Spec, 
                            curTask: Option[TaskDef])(implicit taskTemplateBuilder: TaskTemplateBuilder) : Seq[SourceSpecInfo] = {
    
    resolveNonBranchVar(ParamMode())(taskDef, taskMap, spec)(src=curTask)
    
  }
   
  /**
   * 
   */
  private[builder] def resolveInput(
                            taskDef: TaskDef, 
                            taskMap: Map[Namespace,TaskDef], 
                               spec: Spec, 
                            curTask: Option[TaskDef])(implicit taskTemplateBuilder: TaskTemplateBuilder) : Seq[SourceSpecInfo]  = {
    
    resolveNonBranchVar(InputMode())(taskDef, taskMap, spec)(src=curTask)
    
  }

  
  // group parameters via currying by (1) initial state and (2) items that change recursively
  // srcTaskDefDependency is only non-None if it implies a temporal dependency
  // returns (srcSpec, srcTaskDefDependency, grafts)
  //
  // TODO: Since things are curried in this way, we could assign the result of the function
  // with the first two groups of argument applied to a val -- then that function could just
  // call itself, modifying only the last 3 arguments
  private def resolveNonBranchVar(mode: ResolveMode)
                                 (origTaskDef: TaskDef,
                                  taskMap: Map[Namespace,TaskDef], spec: Spec)
                                 (curSpec: Spec=spec,
                                  src: Option[TaskDef],
                                  grafts: Seq[Branch] = Nil)(implicit taskTemplateBuilder: TaskTemplateBuilder)
                                 : Seq[SourceSpecInfo] = {                                  
//                                 : (Spec, Option[TaskDef], Seq[Branch]) = {
    curSpec.rval match {
      // we might have traced back through a TaskVariable into a parent's parameters,
      // which can, in turn, define a branch point
      // just return what we have and let resolveBranchPoint figure out the rest
      // in handleNonBranchPointHelper()
      case BranchPointDef(_,_) | SequentialBranchPoint(_,_) => Seq(new SourceSpecInfo(curSpec, src, grafts))
      
      // literals will never have a use for grafts
      case Literal(litValue) => {
        val litSpec = curSpec.asInstanceOf[LiteralSpec]
        val litSrc = if (src != Some(origTaskDef) || spec != curSpec) src else None
        Seq(new SourceSpecInfo(litSpec, litSrc, Nil))
      }
      
      case ConfigVariable(varName) => resolveConfigVar(varName, origTaskDef, spec, src, grafts)
      
      case TaskVariable(srcTaskName, srcOutName) => resolveTaskVar(mode)(
        origTaskDef, taskMap, spec)(curSpec, src, grafts)(srcTaskName, srcOutName)
      
      case BranchGraft(srcOutName, srcTaskNameOpt, unexpandedBranchGraftElements) => {
        
        //TODO for Lane: Look through branchGraftElements for any branch globs "*"
        
        /*
         * Here's what Jon suggested:
        def expandGlob(accumulatedGrafts: Seq[Branch], remainingGlobbedBranchPoints: Seq[BranchPoint]) : Seq[SourceSpecInfo] = {
          remainingGlobbedBranchPoints.headOption match {
            case None => /* termination case */
            case Some(bp: BranchPoint) => branches(bp.flatMap{ branch =>
              expandGlob(accumulatedGrafts + branch, remainingGlobbedBranchPoints.tail)
            }) 
          }
        }
        */
        val expandedGlobs = BranchGraftGlob.expand(unexpandedBranchGraftElements, taskTemplateBuilder.branchPointFactory, taskTemplateBuilder.branchFactory)
        
        expandedGlobs.flatMap( (branchGraftElements: Seq[BranchGraftElement]) => {
        
//        val (srcSpec, srcTask, prevGrafts) = srcTaskNameOpt match {
          val srcSpecInfos: Seq[SourceSpecInfo] = srcTaskNameOpt match {        
            case Some(srcTaskName) => {
              resolveTaskVar(mode)(origTaskDef, taskMap, spec)(curSpec, src, grafts)(srcTaskName, srcOutName)
            }
            case None => resolveConfigVar(srcOutName, origTaskDef, spec, src, grafts)
          }
        
          srcSpecInfos.map( srcSpecInfo => {       
            val resultGrafts = srcSpecInfo.grafts ++ branchGraftElements.map{ e => try {
                taskTemplateBuilder.branchFactory(e.branchName, e.branchPointName)
              } catch {
                case ex: NoSuchBranchException => throw new FileFormatException(ex.getMessage, e)
              }
            }
            srcSpecInfo.withUpdatedGrafts(resultGrafts)
          })
        })
//        new SourceSpecInfo(srcSpec, srcTask, resultGrafts)
      }
      
      case Unbound() => {
        mode match {
          case InputMode() => {
            // make sure we didn't just refer to ourselves -- 
            // referring to an unbound output of a parent task is fine though (and usual)
            if (src != Some(origTaskDef) || spec != curSpec) {
              Seq(new SourceSpecInfo(curSpec, src, grafts))
            } else {
              debug("Original task was %s and src is %s".format(origTaskDef, src))
              throw new FileFormatException("Unbound input variable: %s".format(curSpec.name),
                                            List(origTaskDef, curSpec))
            }
          }
          case _ => throw new RuntimeException("Unsupported unbound variable: %s".format(curSpec.name))
        }
      }
      
    }
  }
   
  // helper for resolveNonBranchVar
  private def resolveTaskVar(mode: ResolveMode)
                    (origTaskDef: TaskDef, taskMap: Map[Namespace,TaskDef], spec: Spec)
                    (curSpec: Spec, prevTask: Option[TaskDef], grafts: Seq[Branch])
                    (srcTaskName: String, srcOutName: String)(implicit taskTemplateBuilder: TaskTemplateBuilder)
                    : Seq[SourceSpecInfo] = {
//                    : (Spec, Option[TaskDef], Seq[Branch]) = {
     
    // TODO: XXX: Lane: This may not handle namespaces properly
    val srcTaskNamespace = Namespace.fromString(srcTaskName)
    taskMap.get(srcTaskNamespace) match {
      case Some(srcDef: TaskDef) => {
        // determine where to search when resolving this variable's parent spec
        val specSet = mode match {
          case InputMode() => srcDef.outputs
          case ParamMode() => srcDef.params
        }
        // search for the parent spec
        specSet.find(outSpec => outSpec.name == srcOutName) match {
          case Some(srcSpec) => {
            debug("Found parent of %s @ %s => %s @ %s".format(spec, srcTaskName, srcSpec, srcDef))
            resolveNonBranchVar(mode)(origTaskDef, taskMap, spec)(srcSpec, Some(srcDef), grafts)
          }
          case None => {
            // give a very specific error if it was defined in the task but just not the correct output/param set
            srcDef.allSpecs.find(outSpec => outSpec.name == srcOutName) match {
              case Some(_) => throw new FileFormatException(
              "Output %s at source task %s for required by input %s at task %s not found. It was declared at the source task, but has the wrong type. Candidates are: %s".
                format(srcOutName, srcTaskName, spec.name, origTaskDef.name, specSet.map(_.name).mkString(" ")),
              List(spec, srcDef))
              case None => throw new FileFormatException(
              "Output %s at source task %s for required by input %s at task %s not found. Candidates are: %s".
                format(srcOutName, srcTaskName, spec.name, origTaskDef.name, specSet.map(_.name).mkString(" ")),
              List(spec, srcDef))
            }
          }
        }
      }
      case None => {
        throw new FileFormatException(
          "Source task %s for input %s at task %s not found".
            format(srcTaskName, spec.name, origTaskDef.name), spec)
      }
    }
  }
   
  private def resolveConfigVar(varName: String, taskDef: TaskDef, spec: Spec, src: Option[TaskDef], grafts: Seq[Branch])
                              (implicit taskTemplateBuilder: TaskTemplateBuilder) 
                              : Seq[SourceSpecInfo] = {
    
    return taskTemplateBuilder.confSpecs.get(varName) match {
      case Some(confSpec) => Seq(new SourceSpecInfo(confSpec, None, grafts))
      case None => throw new FileFormatException(
          "Config variable %s required by input %s at task %s not found in config file.".
            format(varName, spec.name, taskDef.name),
          List(spec))
    }
    
  }
  
}
