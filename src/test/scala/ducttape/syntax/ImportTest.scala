package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.util.Files
import ducttape.syntax.GrammarParser.Parser
import java.io.File
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable.Set
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class ImportTest extends AbstractTest("import",Grammar.importStatement(new java.io.File("./"))) {
 
  def successCases = {

    val tutorialDir = new File("tutorial")
    val set: Set[String] = Set.empty[String]
    Files.ls(tutorialDir).filter(_.getName.endsWith(".tape")).foreach(tapeFile => {       
      set.add(s"import ${tapeFile}")
      set.add(s"import ${tapeFile}\n")
      set.add("# A comment\n" + s"import ${tapeFile}")
      set.add(s"import ${tapeFile}" + "\n# A comment\n")
    })
    
    if (set.isEmpty)
      fail("No tutorial files found in " + tutorialDir.getAbsolutePath)
    
    set
  }
  
  def failureCases = Set(
    "",
    " ",
    "A-variable_Name__"
  ) 
  
  def errorCases = Set(
    "/* Forgot to close ",
    "/* Here we go /* another problem",
    "/* Here we go /* another problem */"
  )
  
  override def exceptionCases = Set(
    "import 01-01-hello-world.tap"
  )
  
}