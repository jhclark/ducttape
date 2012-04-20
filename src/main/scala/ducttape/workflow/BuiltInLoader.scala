package ducttape.workflow
import ducttape.syntax.AbstractSyntaxTree.Block
import ducttape.syntax.GrammarParser
import java.io.File
import ducttape.syntax.AbstractSyntaxTree.WorkflowDefinition
import ducttape.util.Files

object BuiltInLoader {
  
  def load(builtInsDir: File): Seq[WorkflowDefinition] = {
    for(file <- findBuiltIns(builtInsDir)) yield {
      GrammarParser.readWorkflow(file)
    }
  }
  
  def findBuiltIns(builtInsDir: File): Seq[File] = {
    Files.ls(builtInsDir).filter{file => file.getName.endsWith(".tape")}
  }
}