global {
       ducttape_output=tutorial/acid/output
       ducttape_experimental_packages=true
}

package sametime :: .versioner=git .repo="git://github.com/jhclark/sametime.git" .ref=HEAD {
  ./build.sh
}

task extract_dictionary
	:: param=(DataSet: train=(TrainCorpus: small="Small training corpus" 
	                                       large="Large training corpus") 
	                    test=(TestSplit: standard="Standard test set" 
	                                     random="Randomized test set"))
	: sametime
	> out
{
	echo "Hello, Dictionary. I'm using parameter ${param}" > ${out}
}

task preproc
	< dict_in=$out@extract_dictionary
	< in=(Side: src=(DataSet: train=(TrainCorpus: small=sm.src large=lg.src)
	                    test=(TestSplit: standard=test.src random=rand.src))
	            tgt=(DataSet: train=(TrainCorpus: small=sm.tgt large=lg.tgt) 
	                    test=(TestSplit: standard=test.tgt random=rand.tgt)))
	> out
{
	echo "I'm using dictionary ${dict_in}"
	cat ${dict_in}
	
	tr '[:upper:]' '[:lower:]' < ${in} > ${out}
}

task process_dict
	< in=(OnlyOne: one=$out@extract_dictionary)
	> out
{
	echo "Let's calculate a dictionary from ${in}" > ${out}
}

task build_model
	< src=$out@preproc[DataSet:train,Side:src]
	< tgt=$out@preproc[DataSet:train,Side:tgt]
	< dict=(UseDict: no=/dev/null yes=$out@process_dict)
	> out
{
	echo "Dictionary = ${dict}" > ${out}
	echo >> ${out}
	paste ${src} ${tgt} >> ${out}
}

func sample_function
	:: x
	> out
{
	# Do nothing in particular
	echo ${x} > ${out}
}

task nothing calls sample_function
	:: x=(Foo: bar)
	> out 


task corpus_counts
	< dummy=(Foo: bar=$out@nothing[Foo:bar])
	< src=$out@preproc[Side:src]
	< tgt=$out@preproc[Side:tgt]
	> out
{
	wc ${dummy} ${src} ${tgt} > ${out}
}

task optimize
	< in=$out@build_model
	:: seed=(OptimizerSeed: 1..3)
	> out
{
	echo "Seed = ${seed}" > ${out}
	echo "Model = ${in}" >> ${out}
}

task evaluate_one
	< counts=$out@corpus_counts
	< weights=$out@optimize
	< model=$out@build_model
	> out
{
	echo "Counts = ${counts}"    > ${out}
	echo "Weights = ${weights}" >> ${out}
}

task evaluate_all
	< counts=$out@corpus_counts[OptimizerSeed:*]
	< weights=$out@optimize
	< model=$out@build_model
	> out
{
	echo "Counts = ${counts}"    > ${out}
	echo "Weights = ${weights}" >> ${out}
}


plan {
	reach evaluate_one,evaluate_all via (DataSet: *)     * 
	                                    (TrainCorpus: *) * 
	                                    (TestSplit: *)   * 
	                                    (UseDict: *)     * 
	                                    (OnlyOne: *)     * 
	                                    (Foo: *)         * 
	                                    (OptimizerSeed: *)
}
