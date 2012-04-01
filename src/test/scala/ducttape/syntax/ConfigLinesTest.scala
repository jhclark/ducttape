package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ConfigLinesTest extends AbstractTest("config lines",Grammar.configLines) {
 
  def successCases = Set(
    "",      
    " ",    
    "A_variable_Name__=foo",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_=bar",
"""
test_src_=( Test: baseline=/phase1/jhclark/experiments/data/zh/fbis/mt08.src.txt.ready mt02=/phase1/jhclark/experiments/data/zh/nistmt/mt02/mt02.src.norm mt03=/phase1/jhclark/experiments/data/zh/nistmt/mt03/mt03.src.norm mt04=/phase1/jhclark/experiments/data/zh/nistmt/mt04/mt04.src.norm mt05=/phase1/jhclark/experiments/data/zh/nistmt/mt05/mt05.src.norm mt06=/phase1/jhclark/experiments/data/zh/nistmt/mt06/mt06.src.norm mt08=/phase1/jhclark/experiments/data/zh/nistmt/mt08/mt08.src.norm )
test_refs_=( Test: baseline=/phase1/jhclark/experiments/data/zh/fbis/mt08.ref.?.tok.norm mt02=/phase1/jhclark/experiments/data/zh/nistmt/mt02/ref.?.lc.tok.norm mt03=/phase1/jhclark/experiments/data/zh/nistmt/mt03/ref.?.lc.tok.norm mt04=/phase1/jhclark/experiments/data/zh/nistmt/mt04/ref.?.lc.tok.norm mt05=/phase1/jhclark/experiments/data/zh/nistmt/mt05/ref.?.lc.tok.norm mt06=/phase1/jhclark/experiments/data/zh/nistmt/mt06/ref.?.lc.tok.norm mt08=/phase1/jhclark/experiments/data/zh/nistmt/mt08/ref.?.lc.tok.norm )
fCorpus_=/phase1/jhclark/experiments/data/zh/fbis/corpus.en.ready.gz
eCorpus_=/phase1/jhclark/experiments/data/zh/fbis/corpus.zh.ready.gz
align_=/phase1/jhclark/experiments/data/zh/fbis/corpus.zh-en.aln.gz
arpa_=/phase1/jhclark/experiments/data/zh/fbis/c2e.3gram.lm.gz
saIni_=/phase1/jhclark/experiments/binopt/sa_full.ini    
""",
"""
// Comment this
test_src_=( Test: baseline=/phase1/jhclark/experiments/data/zh/fbis/mt08.src.txt.ready mt02=/phase1/jhclark/experiments/data/zh/nistmt/mt02/mt02.src.norm mt03=/phase1/jhclark/experiments/data/zh/nistmt/mt03/mt03.src.norm mt04=/phase1/jhclark/experiments/data/zh/nistmt/mt04/mt04.src.norm mt05=/phase1/jhclark/experiments/data/zh/nistmt/mt05/mt05.src.norm mt06=/phase1/jhclark/experiments/data/zh/nistmt/mt06/mt06.src.norm mt08=/phase1/jhclark/experiments/data/zh/nistmt/mt08/mt08.src.norm )

// And comment that
test_refs_=( Test: baseline=/phase1/jhclark/experiments/data/zh/fbis/mt08.ref.?.tok.norm mt02=/phase1/jhclark/experiments/data/zh/nistmt/mt02/ref.?.lc.tok.norm mt03=/phase1/jhclark/experiments/data/zh/nistmt/mt03/ref.?.lc.tok.norm mt04=/phase1/jhclark/experiments/data/zh/nistmt/mt04/ref.?.lc.tok.norm mt05=/phase1/jhclark/experiments/data/zh/nistmt/mt05/ref.?.lc.tok.norm mt06=/phase1/jhclark/experiments/data/zh/nistmt/mt06/ref.?.lc.tok.norm mt08=/phase1/jhclark/experiments/data/zh/nistmt/mt08/ref.?.lc.tok.norm )


fCorpus_=/phase1/jhclark/experiments/data/zh/fbis/corpus.en.ready.gz
// wow
// yikes
eCorpus_=/phase1/jhclark/experiments/data/zh/fbis/corpus.zh.ready.gz # Yeah!
align_=/phase1/jhclark/experiments/data/zh/fbis/corpus.zh-en.aln.gz
arpa_=/phase1/jhclark/experiments/data/zh/fbis/c2e.3gram.lm.gz
saIni_=/phase1/jhclark/experiments/binopt/sa_full.ini // And comment here, too!    
"""
  ) 
  
  def failureCases = Set(

  ) 
  
  def errorCases = Set(
    "A-variable_Name__"
  )
  
}