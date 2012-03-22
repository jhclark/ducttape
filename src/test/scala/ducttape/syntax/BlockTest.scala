package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BlockTest extends AbstractTest("block",Grammar.block) {
 
  def successCases = Set(
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
    echo "$@" >&2
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky < in=foo > out
{
  function die () {
    echo "$@" >&2
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky < in=foo > out :: p=param
{
  function die () {
    echo "$@" >&2
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky 
     < in=foo > out :: p=param
{
  function die () {
    echo "$@" >&2
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
    echo "$@" >&2
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
    echo "$@" >&2
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
    echo "$@" >&2
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
    echo "$@" >&2
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
    echo "$@" >&2
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
    echo "$@" >&2
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
    echo "$@" >&2
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
    echo "$@" >&2
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
    echo "$@" >&2
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
    echo "#$ -S /bin/bash" >> $wrapper
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
    echo "$@" >&2
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