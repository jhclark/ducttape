package ducttape.syntax

import collection._
import ducttape.syntax.AbstractSyntaxTree._
import annotation.tailrec

object ErrorBehavior extends Enumeration {
  type ErrorBehavior = Value
  val Ignore, Warn, Error = Value

  def parse(str: Option[String], default: ErrorBehavior): ErrorBehavior = str match {
    case None => default
    case Some(s) => s.toLowerCase match {
      case "error" => Error
      case "warn" => Warn
      case "ignore" => Ignore
      case _ => throw new RuntimeException("Invalid value for error behavior '%s'. Please use one of: error, warn, ignore".format(s))
    }
  }
}
import ErrorBehavior._

class StaticChecker(undeclaredBehavior: ErrorBehavior,
                    unusedBehavior: ErrorBehavior) {
  
  /**
   * Returns a tuple of (warnings, errors)
   */
  def check(wd: WorkflowDefinition): (Seq[FileFormatException], Seq[FileFormatException]) = {
    
    val warnings = new mutable.ArrayBuffer[FileFormatException]
    val errors = new mutable.ArrayBuffer[FileFormatException]
    
    // make sure that branch points are coherent throughout the workflow
    def findBranchPoints(node: ASTType): Seq[BranchPointDef] = {
      val myBranchPoints: Seq[BranchPointDef] = node match {
        case bp: BranchPointDef => Seq(bp)
        case _ => Nil
      }
      myBranchPoints ++ node.children.flatMap(findBranchPoints(_))
    }
    
    // map from branch point name to the first example branch point of that name
    val branchPoints = new mutable.HashMap[String, BranchPointDef]
    for (branchPoint: BranchPointDef <- findBranchPoints(wd)) {
      val name = branchPoint.name match {
        case None => errors += new FileFormatException("Anonymous branch points are not yet supported", branchPoint)
        case Some(name) => {
          branchPoints.get(name) match {
            case None => branchPoints += name -> branchPoint
            case Some(prevBranchPoint) => {
              if (branchPoint.specs.isEmpty) {
                errors += new FileFormatException("Illegal branch point with zero branches", branchPoint)
              } else {
              
                // 1) make sure baseline branch has the same name as the last time we saw it
                val baseline = branchPoint.specs.head
                val prevBaseline = prevBranchPoint.specs.head
                if (baseline.name != prevBaseline.name) {
                  errors += new FileFormatException("All occurrences of a branch point must have the same baseline branch " +
                      "(The baseline branch is the first branch): '%s' != '%s'".format(baseline.name, prevBaseline.name),
                    List(branchPoint, prevBranchPoint))
                }
                
                
                // 2) make sure the branch point has the same set of branches as the last time we saw it
                val branchNames = branchPoint.specs.map(_.name).toSet
                val prevBranchNames = prevBranchPoint.specs.map(_.name).toSet
                if (branchNames != prevBranchNames) {
                  errors += new FileFormatException("All occurrences of a branch point must have the same set of branch names: " +
                      "%s != %s".format(branchNames, prevBranchNames),
                    List(branchPoint, prevBranchPoint))
                }
              }
            }
          }
        }
      }
    }
    
    
    // check for unsupported block types that are in the grammar, but
    // not yet implemented
    for (block <- wd.blocks) block match {
      case funcCall: CallDefinition => ;
  
      case groupLike: GroupDefinition => {
        groupLike.keyword match {
          case "group" => errors += new FileFormatException("Group blocks are not supported yet", groupLike)
          case "branchpoint" => errors += new FileFormatException("Branchpoint blocks are not supported yet", groupLike)
          case "summary" => ; // TODO: More checking for these...
          case "submitter" => ;
          case "versioner" => ;
        }
      }

      case taskLike: TaskDef => {
        taskLike.keyword match {
          case "baseline" => errors += new FileFormatException("Baseline blocks are not supported yet", taskLike)
          case "branch" => errors += new FileFormatException("Branch blocks are not supported yet", taskLike)
          case "action" => errors += new FileFormatException("Action blocks are not supported yet", taskLike)
          case "of" => errors += new FileFormatException("'of' blocks are not supported yet", taskLike)
          case "func" => ;
          case "package" => ;
          case "task" => ;
          case _ => throw new RuntimeException("Unrecognized task-like block type: " + taskLike.keyword)
        }
      }
      
      case configLike: ConfigDefinition => {
        configLike.keyword match {
          case "global" => ;
          case "config" => ;
        }
      }
      
      case plan: PlanDefinition => ;
    }
    
    val seenTasks = new mutable.HashMap[String,TaskDef]
    for (task: TaskDef <- wd.tasks) {
      seenTasks.get(task.name) match {
        case None => seenTasks += task.name -> task
        case Some(prevTask) => errors += new FileFormatException("Redeclaration of task: %s".format(prevTask.name), List(task, prevTask))
      }
      val (w, e) = checkTaskDef(task)
      warnings ++= w
      errors ++= e
    }
    
    (warnings, errors)
  }
  
  /**
   * Returns a tuple of (warnings, errors)
   */
  def checkTaskDef(taskDef: TaskDef): (Seq[FileFormatException], Seq[FileFormatException]) = {
    val warnings = new mutable.ArrayBuffer[FileFormatException]
    val errors = new mutable.ArrayBuffer[FileFormatException]
        
    // first check for things not allowed in ducttape
    for (spec <- taskDef.inputs) spec.rval match {
      case Literal(path) => {
        if (path == "") {
          errors += new FileFormatException("Empty input name not allowed", spec)
        }
      }
      case _ => ;
    }
    for (spec <- taskDef.outputs) spec.rval match {
      case Literal(path) => {
        if (path == "") {
          errors += new FileFormatException("Empty output name not allowed", spec)
        }
      }
      case _ => ;
    }
    
    // now check for bash issues
    val commands: BashCode = taskDef.commands
    val usedVars: Set[String] = commands.vars
    
    val definedVars: Set[String] = taskDef.header.specsList.flatMap(_.specs.filterNot(_.dotVariable).map(_.name)).toSet
    
    // check for use of undeclared bash variables
    for (usedVar <- usedVars) {
      if (!definedVars.contains(usedVar)) {
        // TODO: Get position information from bash
        val msg = "Potential use of undefined variable: %s at task %s".format(usedVar, taskDef.name)
        undeclaredBehavior match {
          case Ignore => ;
          case Warn => warnings += new FileFormatException(msg, taskDef)
          case Error => errors += new FileFormatException(msg, taskDef)
        }
      }
    }
    
    // check for not using declared variables
    for(definedVar <- definedVars) {
      if(!usedVars.contains(definedVar)) {
        // TODO: Get position information from bash
        val msg = "Unused variable: %s at task %s".format(definedVar, taskDef.name)
        unusedBehavior match {
          case Ignore => ;
          case Warn => warnings += new FileFormatException(msg, taskDef)
          case Error => errors += new FileFormatException(msg, taskDef)
        }
      }
    }
    
    (warnings, errors)
  }
}
