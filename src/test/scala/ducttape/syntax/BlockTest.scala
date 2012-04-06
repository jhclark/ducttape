package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BlockTest extends AbstractTest("block",Grammar.block) {
 
  def successCases = Set(
    """plan {
      reach goal via (branchPointName: a)
    // some comments
    reach g1,g2,g3 via (branchPointName: a)
    reach a via (branchPointName: a b)  
    reach b , b2 via (greeting: y z)
    reach tastyMeal via (sauce: a1 ketchup wasabi)
    reach config via (flags: a b )
    reach h_1, h_2 via (flags: a b)
    }""",

    // Bare rvalues  
    """plan basic {
    reach alphabet via (branchPointName: a b c)
    reach a via (a: a1 a4)
    reach z,y, w via (a: a1)
    reach here, there via (a: a1 a2 a3 a4)
    }""",
    
    // With comments
    """plan {
    reach goal via {(
      // Here is a comment
    a: a1 a2 a3)}}""",
    """plan {
    reach goal via {(
      // Here is a comment
    a: 
    // More comments
    a1 a2 a3)}
    } """,
    """plan {
    reach another, goal, too via {
    (
      // Here is a comment
    a: 
    // More comments
    a1 
    // And more
    // And more
    a2 a3)
    }
    } """,    
    """plan {
    reach goal via {(
      // Here is a comment
    a: 
    // More comments
    a1 
    // And more
    // And more
    a2 
    // And more
    a3)}}""",        
    """plan {
    reach goal via 
    {
    (
      // Here is a comment
    a: 
    // More comments
    a1 
    // And more
    // And more
    a2 
    // And more
    a3
    // And more
    // and more
    // and More
    )
    }}""",
    """plan myPlan {
    reach goal via {
    (
      // Here is a comment
    a: 
    // More comments
    a1 // here too!
    // And more
    // And more
    a2 
    // And more
    a3
    // And more
    // and more
    // and More
    )
    }
    }""",    
    """plan {
      reach goal via { (
      // Here is a comment
    a: 
    // More comments
    a1 // here too!
    // And more
    // And more
    a2 // and here
    // And more
    a3  // and here
    // And more
    // and more
    // and More
    )}
      }""",    
    
    // Cross products
    """plan it {  
          reach goal via (a: a1 a2 a3) * (b: b1 b2)
          reach goal via (a: a1) * (b: b1)
          reach goal via (a: a1 a2 a3) * (b: b1 b2) * (c: c_one c_two c_three c_four)
          reach goal via (a: a1 a2 a3) * (b: b1 b2) * (c: c_one c_two c_three c_four) * (d: *)
      }""",
    
    // Multi-line
    """plan something {
      // Comment
      reach somewhere, else via {
          (a: a1 a2 a3) 
        * (b: b1 b2)
       }
      
      }""",
    """plan it {
      reach goal via { (a: a1) * (b: b1) }}""",
    """plan {
      reach countdown via { (a: a1 a2 a3) * (b: b1 b2)
    * (c: c_one c_two c_three c_four)
    }
      }""",
    """plan {
      reach super_hero via { // Here I come to save the day
    
    // Mighty mouse
    // is on the way
    (a: a1 a2 a3) // Fighting evil
    * 
    // in the breeze
    (b: b1 b2) * (c: c_one c_two c_three c_four) * 
    
    // Looking for a piece
    (d: *) // of cheese
    }}""",       
"""task x = filter < in=x.txt > out""",
"""group g {
}""",
"""group g {
     task hello {
        echo "hello, world!"
     }
}""",      
"""task hello {
      echo "hello, world!"
}""",
"""// Hello, world
task hello {
      echo "hello, world!"
}""",
""" // Hello, world
task hello {
      echo "hello, world!"
}""",
""" // Hello, world
// some more
task hello {
      echo "hello, world!"
}""",
"""task hello 
{
      echo "hello, world!"
}""",
"""task funky < in=foo > out {
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky < in=foo > out
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky < in=foo > out :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky 
     < in=foo > out :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky 
     < in=foo 
     > out 
     :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky
    // input
    < in=foo x=73
    // out
    > out
    // parameters
    :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky < in=foo x=73
    // out
    > out
    // parameters
    :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky : moses < in=foo x=73
    // out
    > out
    // parameters
    :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky // moses
    : moses < in=foo x=73
    // out
    > out
    // parameters
    :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky // moses
: moses < in=foo x=73
    // out
    > out
    // parameters
    :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky 
// moses
: moses < in=foo x=73
    // out
    > out
    // parameters
    :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky
  // moses
  : moses 
  // input
  < in=foo x=73
  // out
  > out
  // parameters
  :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky
  // moses
  : moses 
  // input
  < in=foo x=73
  // out
  > out
  // parameters
  :: p=param {
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
""" // Hello, world

task hello {
      echo "hello, world!"
}""",
""" // Hello, world
// some more

task hello {
      echo "hello, world!"
}""",
""" // Hello, world

// some more
task hello {
      echo "hello, world!"
}""",
""" // Hello, world

// some more

task hello {
      echo "hello, world!"
}"""  ,
      """task hello {
        echo "hello, world!"
      }""" ,
"""# To define a "task function" in the same file
func filter < in > out {
  cat < $in > $out
}      
""",
"""summary fileSizes {
# Inside summary block
of pt > PTFileSize {
  du -sh $pt > $PTFileSize
}
}
""",
"""submitter sge :: CMDS vmem walltime q {
  // stuff
  action wrap > wrapper {
    echo '#$ -S /bin/bash' >> $wrapper
  }

  // more stuff
  // and more
  action run < wrapper > jobid {
    qsub $wrapper > $jobid
  }

  action poll < jobid > done exit_code {
    # some code
  }

}
""",
"""config myStuff {
test_src_=( Test: baseline=/phase1/jhclark/experiments/data/zh/fbis/mt08.src.txt.ready mt02=/phase1/jhclark/experiments/data/zh/nistmt/mt02/mt02.src.norm mt03=/phase1/jhclark/experiments/data/zh/nistmt/mt03/mt03.src.norm mt04=/phase1/jhclark/experiments/data/zh/nistmt/mt04/mt04.src.norm mt05=/phase1/jhclark/experiments/data/zh/nistmt/mt05/mt05.src.norm mt06=/phase1/jhclark/experiments/data/zh/nistmt/mt06/mt06.src.norm mt08=/phase1/jhclark/experiments/data/zh/nistmt/mt08/mt08.src.norm )
test_refs_=( Test: baseline=/phase1/jhclark/experiments/data/zh/fbis/mt08.ref.?.tok.norm mt02=/phase1/jhclark/experiments/data/zh/nistmt/mt02/ref.?.lc.tok.norm mt03=/phase1/jhclark/experiments/data/zh/nistmt/mt03/ref.?.lc.tok.norm mt04=/phase1/jhclark/experiments/data/zh/nistmt/mt04/ref.?.lc.tok.norm mt05=/phase1/jhclark/experiments/data/zh/nistmt/mt05/ref.?.lc.tok.norm mt06=/phase1/jhclark/experiments/data/zh/nistmt/mt06/ref.?.lc.tok.norm mt08=/phase1/jhclark/experiments/data/zh/nistmt/mt08/ref.?.lc.tok.norm )
fCorpus_=/phase1/jhclark/experiments/data/zh/fbis/corpus.en.ready.gz
eCorpus_=/phase1/jhclark/experiments/data/zh/fbis/corpus.zh.ready.gz
align_=/phase1/jhclark/experiments/data/zh/fbis/corpus.zh-en.aln.gz
arpa_=/phase1/jhclark/experiments/data/zh/fbis/c2e.3gram.lm.gz
saIni_=/phase1/jhclark/experiments/binopt/sa_full.ini
}
""",
"""config {
test_src_=( Test: baseline=/phase1/jhclark/experiments/data/zh/fbis/mt08.src.txt.ready mt02=/phase1/jhclark/experiments/data/zh/nistmt/mt02/mt02.src.norm mt03=/phase1/jhclark/experiments/data/zh/nistmt/mt03/mt03.src.norm mt04=/phase1/jhclark/experiments/data/zh/nistmt/mt04/mt04.src.norm mt05=/phase1/jhclark/experiments/data/zh/nistmt/mt05/mt05.src.norm mt06=/phase1/jhclark/experiments/data/zh/nistmt/mt06/mt06.src.norm mt08=/phase1/jhclark/experiments/data/zh/nistmt/mt08/mt08.src.norm )
test_refs_=( Test: baseline=/phase1/jhclark/experiments/data/zh/fbis/mt08.ref.?.tok.norm mt02=/phase1/jhclark/experiments/data/zh/nistmt/mt02/ref.?.lc.tok.norm mt03=/phase1/jhclark/experiments/data/zh/nistmt/mt03/ref.?.lc.tok.norm mt04=/phase1/jhclark/experiments/data/zh/nistmt/mt04/ref.?.lc.tok.norm mt05=/phase1/jhclark/experiments/data/zh/nistmt/mt05/ref.?.lc.tok.norm mt06=/phase1/jhclark/experiments/data/zh/nistmt/mt06/ref.?.lc.tok.norm mt08=/phase1/jhclark/experiments/data/zh/nistmt/mt08/ref.?.lc.tok.norm )
fCorpus_=/phase1/jhclark/experiments/data/zh/fbis/corpus.en.ready.gz
eCorpus_=/phase1/jhclark/experiments/data/zh/fbis/corpus.zh.ready.gz
align_=/phase1/jhclark/experiments/data/zh/fbis/corpus.zh-en.aln.gz
arpa_=/phase1/jhclark/experiments/data/zh/fbis/c2e.3gram.lm.gz
saIni_=/phase1/jhclark/experiments/binopt/sa_full.ini
}
""",
"""global myStuff {
test_src_=( Test: baseline=/phase1/jhclark/experiments/data/zh/fbis/mt08.src.txt.ready mt02=/phase1/jhclark/experiments/data/zh/nistmt/mt02/mt02.src.norm mt03=/phase1/jhclark/experiments/data/zh/nistmt/mt03/mt03.src.norm mt04=/phase1/jhclark/experiments/data/zh/nistmt/mt04/mt04.src.norm mt05=/phase1/jhclark/experiments/data/zh/nistmt/mt05/mt05.src.norm mt06=/phase1/jhclark/experiments/data/zh/nistmt/mt06/mt06.src.norm mt08=/phase1/jhclark/experiments/data/zh/nistmt/mt08/mt08.src.norm )
test_refs_=( Test: baseline=/phase1/jhclark/experiments/data/zh/fbis/mt08.ref.?.tok.norm mt02=/phase1/jhclark/experiments/data/zh/nistmt/mt02/ref.?.lc.tok.norm mt03=/phase1/jhclark/experiments/data/zh/nistmt/mt03/ref.?.lc.tok.norm mt04=/phase1/jhclark/experiments/data/zh/nistmt/mt04/ref.?.lc.tok.norm mt05=/phase1/jhclark/experiments/data/zh/nistmt/mt05/ref.?.lc.tok.norm mt06=/phase1/jhclark/experiments/data/zh/nistmt/mt06/ref.?.lc.tok.norm mt08=/phase1/jhclark/experiments/data/zh/nistmt/mt08/ref.?.lc.tok.norm )
fCorpus_=/phase1/jhclark/experiments/data/zh/fbis/corpus.en.ready.gz
eCorpus_=/phase1/jhclark/experiments/data/zh/fbis/corpus.zh.ready.gz
align_=/phase1/jhclark/experiments/data/zh/fbis/corpus.zh-en.aln.gz
arpa_=/phase1/jhclark/experiments/data/zh/fbis/c2e.3gram.lm.gz
saIni_=/phase1/jhclark/experiments/binopt/sa_full.ini
}
""",
"""global {
test_src_=( Test: baseline=/phase1/jhclark/experiments/data/zh/fbis/mt08.src.txt.ready mt02=/phase1/jhclark/experiments/data/zh/nistmt/mt02/mt02.src.norm mt03=/phase1/jhclark/experiments/data/zh/nistmt/mt03/mt03.src.norm mt04=/phase1/jhclark/experiments/data/zh/nistmt/mt04/mt04.src.norm mt05=/phase1/jhclark/experiments/data/zh/nistmt/mt05/mt05.src.norm mt06=/phase1/jhclark/experiments/data/zh/nistmt/mt06/mt06.src.norm mt08=/phase1/jhclark/experiments/data/zh/nistmt/mt08/mt08.src.norm )
test_refs_=( Test: baseline=/phase1/jhclark/experiments/data/zh/fbis/mt08.ref.?.tok.norm mt02=/phase1/jhclark/experiments/data/zh/nistmt/mt02/ref.?.lc.tok.norm mt03=/phase1/jhclark/experiments/data/zh/nistmt/mt03/ref.?.lc.tok.norm mt04=/phase1/jhclark/experiments/data/zh/nistmt/mt04/ref.?.lc.tok.norm mt05=/phase1/jhclark/experiments/data/zh/nistmt/mt05/ref.?.lc.tok.norm mt06=/phase1/jhclark/experiments/data/zh/nistmt/mt06/ref.?.lc.tok.norm mt08=/phase1/jhclark/experiments/data/zh/nistmt/mt08/ref.?.lc.tok.norm )
fCorpus_=/phase1/jhclark/experiments/data/zh/fbis/corpus.en.ready.gz
eCorpus_=/phase1/jhclark/experiments/data/zh/fbis/corpus.zh.ready.gz
align_=/phase1/jhclark/experiments/data/zh/fbis/corpus.zh-en.aln.gz
arpa_=/phase1/jhclark/experiments/data/zh/fbis/c2e.3gram.lm.gz
saIni_=/phase1/jhclark/experiments/binopt/sa_full.ini
}
"""
  ) 
  
  def failureCases = Set(
"""group fileSizes {
  # Inside summary block
  of pt > PTFileSize {
    du -sh $pt > $PTFileSize
  }
}
""",      
"""task funky < in=foo > out  
 bar {
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",      
    " ",
    "A-variable_Name__",
    "",      
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
    // Do some inputs
    < in=$out@tokenize[DataSet:test] 
    // Here's the result
    > hyps""" ,
    """// Package comments
      moses tokenizerr giza
    // Do some inputs
    < in=$out@tokenize[DataSet:test] 
    // Here's the result
    > hyps"""       
  ) 
  
  def errorCases = Set(   
          
"""task hello {
      echo "hello, world!"
} yay""",
"""task hello { // Comments are not allowed after opening { braces
      echo "hello, world!"
}""",
"""task hello { # Comments are not allowed after opening { braces
      echo "hello, world!"
}""" 
  )
  
}