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
class BlocksTest extends AbstractTest("blocks", Grammar.blocks) {

  def successCases = {

    val tutorialDir = new File("tutorial")
    val set: Set[String] = Set.empty[String]
    Files.ls(tutorialDir).filter(_.getName.endsWith(".tape")).foreach(tapeFile => {       
      val source = Source.fromFile(tapeFile)
      set.add(source.mkString)
      source.close()
    })
    
    if (set.isEmpty)
      fail("No tutorial files found in " + tutorialDir.getAbsolutePath)
    
    set
  }
  
  def failureCases = Set(

  ) 
  
  def errorCases = Set(   
//    " ",            
//"""[funky] < in=foo > out  
// bar {
//  function die () {
//    echo "$@" >&2
//    exit 1
//  }
//  
//  # Now do it!
//  die()
//}""" ,      
//      """[hello] {
//        echo "hello, world!"
//      }""",      
//"""[hello] {
//      echo "hello, world!"
//} yay""",
//"""[hello] { // Comments are not allowed after opening { braces
//      echo "hello, world!"
//}""",
//"""[hello] { # Comments are not allowed after opening { braces
//      echo "hello, world!"
//}""",
//    "A-variable_Name__",
//    "",      
//    "> x y_txt",
//    "< a=/etc/passwd b=/etc/hosts",
//    "> x",
//    "< a=$x@first > x",
//    "tokenizer < in=(DataSet: train=a.txt tune=b.txt test=c.txt) > out",
//    "< in=$out@tokenize[DataSet:train] > model",
//    "< in=$out@tokenize[DataSet:tune] > weights",
//    "moses tokenizerr giza < in=$out@tokenize[DataSet:test] > hyps",
//    """moses tokenizerr giza
//    < in=$out@tokenize[DataSet:test] > hyps""",
//    """moses tokenizerr giza
//    // Do some inputs
//    < in=$out@tokenize[DataSet:test] 
//    // Here's the result
//    > hyps""" ,
//    """// Package comments
//      moses tokenizerr giza
//    // Do some inputs
//    < in=$out@tokenize[DataSet:test] 
//    // Here's the result
//    > hyps"""    
  )
  
}