package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TaskBlockTest extends AbstractTest("task header",Grammar.taskBlock) {
 
  def successCases = Set(
"""[hello] {
      echo "hello, world!"
}""",
"""[hello] 
{
      echo "hello, world!"
}""",
"""[funky] < in=foo > out {
  function die () {
    echo "$@" >&2
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""[funky] < in=foo > out
{
  function die () {
    echo "$@" >&2
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""[funky] < in=foo > out :: p=param
{
  function die () {
    echo "$@" >&2
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""[funky] 
     < in=foo > out :: p=param
{
  function die () {
    echo "$@" >&2
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""[funky] 
     < in=foo 
     > out 
     :: p=param
{
  function die () {
    echo "$@" >&2
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""[funky]
    # input
    < in=foo x=73
    # out
    > out
    # parameters
    :: p=param
{
  function die () {
    echo "$@" >&2
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""[funky] < in=foo x=73
    # out
    > out
    # parameters
    :: p=param
{
  function die () {
    echo "$@" >&2
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""[funky] moses < in=foo x=73
    # out
    > out
    # parameters
    :: p=param
{
  function die () {
    echo "$@" >&2
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""[funky] # moses
    moses < in=foo x=73
    # out
    > out
    # parameters
    :: p=param
{
  function die () {
    echo "$@" >&2
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""[funky] # moses
moses < in=foo x=73
    # out
    > out
    # parameters
    :: p=param
{
  function die () {
    echo "$@" >&2
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""[funky] 
# moses
moses < in=foo x=73
    # out
    > out
    # parameters
    :: p=param
{
  function die () {
    echo "$@" >&2
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""[funky]
  # moses
  moses 
  # input
  < in=foo x=73
  # out
  > out
  # parameters
  :: p=param
{
  function die () {
    echo "$@" >&2
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""[funky]
  # moses
  moses 
  # input
  < in=foo x=73
  # out
  > out
  # parameters
  :: p=param {
  function die () {
    echo "$@" >&2
    exit 1
  }
  
  # Now do it!
  die()
}"""

  ) 
  
  def failureCases = Set(

  ) 
  
  def errorCases = Set(
"""[funky] < in=foo > out  
 bar {
  function die () {
    echo "$@" >&2
    exit 1
  }
  
  # Now do it!
  die()
}""" ,      
      """[hello] {
        echo "hello, world!"
      }""",      
"""[hello] {
      echo "hello, world!"
} yay""",      
    "A-variable_Name__",
    "",
    " ",      
    "> x y_txt",
    "< a=/etc/passwd b=/etc/hosts",
    "> x",
    "< a=$x@first > x",
    "tokenizer < in=(DataSet: train=a.txt tune=b.txt test=c.txt) > out",
    "< in=$out@tokenize[DataSet:train] > model",
    "< in=$out@tokenize[DataSet:tune] > weights",
    "moses tokenizerr giza < in=$out@tokenize[DataSet:test] > hyps",
    """moses tokenizerr giza
    < in=$out@tokenize[DataSet:test] > hyps""",
    """moses tokenizerr giza
    # Do some inputs
    < in=$out@tokenize[DataSet:test] 
    # Here's the result
    > hyps""",
    """# Package comments
      moses tokenizerr giza
    # Do some inputs
    < in=$out@tokenize[DataSet:test] 
    # Here's the result
    > hyps"""     
  )
  
}