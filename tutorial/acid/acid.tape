global {
       ducttape_output=tutorial/acid/output
}


task nothing 
	< in=(Dummy: dummy=dummy.txt)
	> out 
{
	# Do nothing in particular
	wc -l ${in} > ${out}
}



task extract_dictionary
	:: param=(DataSet: train=(TrainCorpus: small="Small training corpus" large="Large training corpus") test=(TestSplit: standard="Standard test set" random="Randomized test set"))
	> out
{
	echo "Hello, Dictionary. I'm using parameter ${param}" > ${out}
}


task preproc
	< dict_in=$out@extract_dictionary
	< src_in=(DataSet: train=(TrainCorpus: small=small.src large=large.src) test=(TestSplit: standard=test.src random=test.random.src))
	< tgt_in=(DataSet: train=(TrainCorpus: small=small.tgt large=large.tgt) test=(TestSplit: standard=test.tgt random=test.random.tgt))
	> src_out
	> tgt_out
{
	echo "I'm using dictionary ${dict_in}"
	cat ${dict_in}
	
	tr '[:upper:]' '[:lower:]' < ${src_in} > ${src_out}
	tr '[:upper:]' '[:lower:]' < ${tgt_in} > ${tgt_out}
}


task corpus_counts
	< dummy_in=$out@nothing[Dummy:dummy]
	< src_in=$src_out@preproc
	< tgt_in=$tgt_out@preproc
	> out
{
	wc ${dummy_in} ${src_in} ${tgt_in} > ${out}
}

task process_dict
	< in=(OnlyOne: one=$src_out@preproc)
	> out
{
	echo "Let's calculate a dictionary from ${in}" > ${out}
}

task build_model
	< training_src=$src_out@preproc[DataSet:train]
	< training_tgt=$tgt_out@preproc[DataSet:train]
	< dictionary=(UseDict: no=/dev/null yes=$out@process_dict)
	> out
{
	echo "Dictionary = ${dictionary}" > ${out}
	echo >> ${out}
	paste ${training_src} ${training_tgt} >> ${out}
}

task optimize
	< in=$out@build_model
	:: seed=(OptimizerSeed: 1 2 3)
	> out
{
	echo "Seed = ${seed}" > ${out}
	echo "Model = ${in}" >> ${out}
}

task evaluate_one
	< counts=$out@corpus_counts
	< weights=$out@optimize
	> out
{
	echo "Counts = ${counts}"    > ${out}
	echo "Weights = ${weights}" >> ${out}
}

task evaluate_all
	< counts=$out@corpus_counts[OptimizerSeed:*]
	< weights=$out@optimize
	> out
{
	echo "Counts = ${counts}"    > ${out}
	echo "Weights = ${weights}" >> ${out}
}


plan {
	reach evaluate_one,evaluate_all via (DataSet: *) * (TrainCorpus: *) * (TestSplit: *) * (UseDict: *) * (OnlyOne: *) * (Dummy: *) * (OptimizerSeed: *)
}
