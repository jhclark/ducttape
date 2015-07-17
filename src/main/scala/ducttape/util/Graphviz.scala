package ducttape.util

import ducttape.syntax.AbstractSyntaxTree.Block
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.Specs
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.AbstractSyntaxTree.TaskInputs
import ducttape.syntax.AbstractSyntaxTree.TaskOutputs
import ducttape.syntax.AbstractSyntaxTree.TaskPackageNames
import ducttape.syntax.AbstractSyntaxTree.TaskParams
import ducttape.syntax.AbstractSyntaxTree.WorkflowDefinition

private class IdentifierTracker(private val name:String) {
  
  var counter = 0

  def getNextID(item:Object) : String = {
    counter += 1
    val id = s"${name}${counter}"
    
    if (IdentifierTracker.map.containsKey(item)) {
      throw new RuntimeException(s"""Cannot assign Item ${item.toString} the ID ${id} because it already has been assigned ID ${IdentifierTracker.map.get(item)}""")
    } else {
      IdentifierTracker.map.put(item,id)
    }
    
    return id
  }
  
}

private object IdentifierTracker {
  
  val map = new java.util.IdentityHashMap[Object,String]
  
  def retrieveID(item:Object) : String = map.get(item)
  
}


class Graphviz(wd: WorkflowDefinition) {

  private val s = new StringBuilder()
  
  
  private var taskIDs = new IdentifierTracker("task")
  private var branchIDs = new IdentifierTracker("branch")
  private var branchPointIDs = new IdentifierTracker("branchPoint")
    /*
  private def constructNumberedName(name:String, number:Int) : String = {
    return s"${name}${number}"
  }
  */
  //private val taskIDmap : Map[TaskDef,String] = {
    
  //  val map = new scala.collection.mutable.HashMap[TaskDef,String]()
    /*
    wd.blocks.foreach( (block:Block) => {
    
      block match { 
    
        case task:TaskDef => { taskIDs.getNextID(task) } //constructNumberedName("task", map.size) }
      
        case _ => {}
      
      }
    
    })
    */
    //map.toMap
 // }
  
  
  
  
  
  
  s.append("digraph G {\n\n") // Start directed graph G
  
  s.append("\tgraph [nodesep=\"1\", ranksep=\"1\"];\n\n")

  /*
  taskToNumberedName.foreach( (mapEntry:(TaskDef,String)) => {
    val task = mapEntry._1
    val string = mapEntry._2
    s.append('\t')
    s.append(string)
    s.append('\n')
  })
  */
  
  // Iterate over defined blocks
  wd.blocks.foreach( (block:Block) => {

     block match { 
    
        case task:TaskDef => { 
          
          val taskID = taskIDs.getNextID(task) //IdentifierTracker.retrieveID(task) //taskIDmap(task)
          
          // Output task name
          s.append(s"""\tsubgraph cluster_${taskID} {\n""")
          s.append(s"""\t\t${taskID} [style="task" label="${task.name}"]\n""") 
          
          var outputIDs = new IdentifierTracker(s"${taskID}_out")
          var inputIDs = new IdentifierTracker(s"${taskID}_in")
          var paramIDs = new IdentifierTracker(s"${taskID}_param")
          
          def process(specs:Specs, counter:IdentifierTracker, specType:String) {
            specs.specs.foreach( (spec:Spec) => {
              s.append(s"""\t\t${counter.getNextID(spec)} [style="${specType}" label="${spec.name}"]\n""")
            })            
          }
          
          task.header.children.foreach( (specs:Specs) => {
            specs match {
              
              case inputs:TaskInputs   => process(inputs,  inputIDs,  "input")
              
              case outputs:TaskOutputs => process(outputs, outputIDs, "output")
              
              case params:TaskParams   => process(params,  paramIDs,  "param") 
              
              case packageNames:TaskPackageNames => {}
              
            }
          })
           
          s.append("\t}\n\n")
        }
      
        case _ => {}
      
      }    
  })
  
  s.append("}\n") // End directed graph G
  
  System.out.println(s.toString)
}
