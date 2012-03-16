package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PackageNamesTest extends AbstractTest("package naames",Grammar.packageNames) {
 
  def successCases = Set(
    "",
    "moses tokenizer giza",
    """# Package comments
      moses tokenizerr giza"""     
  ) 
  
  def failureCases = Set(
      " "
  ) 
  
  def errorCases = Set(
    "A-variable_Name__"
  )
  
}