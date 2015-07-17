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




class Graphviz(wd: WorkflowDefinition) {

	private class IdentifierTracker(private val name:String, private val map:java.util.IdentityHashMap[Object,String]) {

		var counter = 0

		def getID(item:Object) : String = {

		  if (map.containsKey(item)) {

			  return map.get(item)

		  } else {

			  counter += 1

				val id = s"${name}${counter}"

				map.put(item,id)

				return id

			}

		}

	}

  private val map = new java.util.IdentityHashMap[Object,String]
  
  
	

	private var taskIDs = new IdentifierTracker("task", map)
	private var branchIDs = new IdentifierTracker("branch", map)
	private var branchPointIDs = new IdentifierTracker("branchPoint", map)


	private def drawTaskSubgraph(task:TaskDef, s:StringBuilder) : Unit = {

    val taskID = taskIDs.getID(task)

		s.append(s"""\tsubgraph cluster_${taskID} {\n""")
		s.append(s"""\t\t${taskID} [style="task" label="${task.name}"]\n""") 

		var outputIDs = new IdentifierTracker(s"${taskID}_out", map)
		var inputIDs = new IdentifierTracker(s"${taskID}_in", map)
		var paramIDs = new IdentifierTracker(s"${taskID}_param", map)

		def process(specs:Specs, counter:IdentifierTracker, specType:String) {
			specs.specs.foreach( (spec:Spec) => {
				s.append(s"""\t\t${counter.getID(spec)} [style="${specType}" label="${spec.name}"];\n""")
			})            
		}

		task.header.children.foreach( (specs:Specs) => {
			specs match {          
			case inputs:      TaskInputs       => process(inputs,  inputIDs,  "input")              
			case outputs:     TaskOutputs      => process(outputs, outputIDs, "output")             
			case params:      TaskParams       => process(params,  paramIDs,  "param")            
			case packageNames:TaskPackageNames => {}          
			}
		})

		s.append("\t}\n\n") 
    
	}

	private def drawTaskArrows(task:TaskDef, s:StringBuilder) : Unit = {

    val taskID = taskIDs.getID(task)

    var outputIDs = new IdentifierTracker(s"${taskID}_out", map)
    var inputIDs = new IdentifierTracker(s"${taskID}_in", map)
    var paramIDs = new IdentifierTracker(s"${taskID}_param", map)

    def process(specs:Specs, counter:IdentifierTracker, inputOrParam:Boolean) {
      specs.specs.foreach( (spec:Spec) => {
        if (inputOrParam) {
        	s.append(s"""\t${counter.getID(spec)} -> ${taskID};\n""")
        } else {
          s.append(s"""\t${taskID} -> ${counter.getID(spec)};\n""")
        }
      })            
    }

    task.header.children.foreach( (specs:Specs) => {
      specs match {          
      case inputs:      TaskInputs       => process(inputs,  inputIDs,  true)              
      case outputs:     TaskOutputs      => process(outputs, outputIDs, false)             
      case params:      TaskParams       => process(params,  paramIDs,  true)            
      case packageNames:TaskPackageNames => {}          
      }
    })
    
    s.append("\n")
  }

  
  def graph() : String = {
    
    val s = new StringBuilder()
    
    s.append("digraph G {\n\n") // Start directed graph G

    s.append("\tgraph [nodesep=\"1\", ranksep=\"1\"];\n\n")


    // Iterate over defined blocks
    wd.blocks.foreach( (block:Block) => {

      block match { 

      case task:TaskDef => { 

        drawTaskSubgraph(task, s)
        drawTaskArrows(task, s)

      }

      case _ => {}

      }    
    })

    s.append("}\n\n") // End directed graph G    
    
    return s.toString()
    
  }
  
  
  def dot2tex() : String = {
    
    val s = new StringBuilder()
    
    s.append("""\begin{center}""").append('\n')
    s.append("""\begin{tikzpicture}[>=latex, scale=0.85, transform shape]""").append('\n')
    s.append("""\begin{dot2tex}[dot,scale=0.85,tikzedgelabels,codeonly]""").append("\n\n")    
    
    s.append(graph())
    
    s.append("""\end{dot2tex}""").append('\n')
    s.append("""\end{tikzpicture}""").append('\n')
    s.append("""\end{center}""").append('\n')    
    
    return s.toString()
    
  }

}
