package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BashCodeTest extends AbstractTest("bash code",BashGrammar.bashBlock) {
 
  def successCases = Set(
    "",  
    " ",
    """A_variable_Name__="hello"""",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_=5",
    "$x",
    "${x}",
    "${x:+foo}",
    "${x:+$x}",
    """echo "export CLASSPATH=${classpath_dir}/antlr-3.1.1.jar:${CLASSPATH:+:$CLASSPATH}" > ${environment}""",
    "echo ${variable##*/}",
    """echo "it's ok"""",
    """echo 'my favorite is the " mark'""",
    """awk -F$'\n' '{print "This is a test of string expansion for tabs"}'"""
  ) 
  
  def failureCases = Set(
    "${x",
    """echo "hello,
    world"""
  ) 
  
  def errorCases = Set(

  )
  
}
