def tune: (cdec, fCorpus, eCorpus, N, metric) -> (weights, dev_bleu) {
  $cdec/decoder/cdec $fCorpus $eCorpus $N --metric $metric > $weights
}

# In scala:
def tune(cdec, fCorpus, eCorpus, N, metric): (weights, dev_bleu) = {
  
}

[tune] cdec f_dev e_dev N metric > weights dev_bleu {
  $cdec/decoder/cdec $f_dev $e_dev $N $metric > $weights
}

package cdec {
  
}



cdec = Package(url="blah")
multeval = Package(url="blah")
f_dev = File("/some/file")
e_dev = File("/some/file")
metric = BranchPoint("Metric", Map("bleu" -> "BLEU", "meteor" -> "METEOR"))
metric = (Metric: bleu="BLEU" meteor="METEOR")
(weights, dev_bleu) = tune(cdec, f_dev, e_dev, 10, metric) // returns packed references...
e_test = decode(cdec, f_test, weights)
score = eval(multeval, e_test, refs_test)
score.exec(select="branches")

