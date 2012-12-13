#!/usr/bin/env ducttape

# This ducttape workflow implements the cdec tutorial
# at http://www.cdec-decoder.org/guide/compiling.html
#
# Usage: ducttape tutorial.tape -C tutorial.tconf
#
# Unlike most MT toolkit pipelines, this tutorial workflow is *meant* to be
# modified in complex ways so that you can easily insert your new ideas
# with minimal effort.

# BUILDING:
# You will likely have to edit the 'package' blocks such that the paths match your local environment.
# You MUST download and extract the SRILM code yourself due to licensing restrictions.
#
# Some dependencies you should be aware of:
# * curl
# * python 2.7+
# * cython 0.14.1+
# * boost
#
# For help compiling cdec see http://www.cdec-decoder.org/guide/compiling.html

# In practice, you'll probably want to clone these git repos locally and point these repo URLs
# at your local machine: 1) so that you can pull just when you want and 2) so that you can edit the repos locally
# For example, "git://github.com/redpony/cdec.git" might become something like "/home/jhclark/cdec"

# Tutorial step #0: Build and compile software
# Estimated time: ~15 minutes
package cdec :: .versioner=git .repo="git://github.com/redpony/cdec.git" .ref=HEAD
{
  autoreconf -ifv
  ./configure # --with-boost=/path/to/boost-install
  make
  ./tests/run-system-tests.pl

  # Build the python extensions
  cd python
  python setup.py build
}

package multeval :: .versioner=git .repo="git://github.com/jhclark/multeval.git" .ref=HEAD {
  ./get_deps.sh
  ant
}

# Sorry folks, due to the nasty license, you'll have to download the software and update this path yourselves
package srilm :: cores=16 .versioner=disk .path="/home/jhclark/workspace/srilm" {
  make -j $cores SRILM=$PWD MACHINE_TYPE=i686-m64 MAKE_PIC=yes World
}

# Tutorial Step #1
# Estimated time: ~2 minutes
# Note: We use a "branch point" called DataSection to run this task once for each data section
task preproc_corpus : cdec
    < in=(DataSection: train=$train_data dev=$dev_data devtest=$devtest_data)
    > out {
  $cdec/corpus/tokenize-anything.sh < $in > $out
}

# Tutorial Step #2
# Estimated time: 20 sec
# Note: We use a "branch graft" to grab only the training data section so that this task
#       gets run only for the "train" branch of the "DataSection" branch point
task filter_training : cdec
    < in=$out@preproc_corpus[DataSection:train]
    > out {
  $cdec/corpus/filter-length.pl $in > $out
}

# Tutorial Step #3
# Estimated time: ~10 minutes
# Note: We use a branch point "AlignDirection" to run this step for two branches: forward and reverse
#
# For more information on word alignment, see http://www.cdec-decoder.org/concepts/alignment.html
# For more information on fast align, see http://www.cdec-decoder.org/guide/fast_align.html
task align_bidir : cdec
    < train=$out@filter_training
    > alignments
    :: direction_flag=(AlignDirection: forward="" reverse="-r") {

  $cdec/training/fast_align -i $train -d -v $direction_flag > $alignments
}

# Tutorial Step #4
# Estimated time: 5 seconds
# Note: Here, we use both branches of the "Direction" branch point at the same time during symmetrization
task align_sym : cdec
    < align_fwd=$alignments@align_bidir[AlignDirection:forward]
    < align_rev=$alignments@align_bidir[AlignDirection:reverse]
    > alignments
    :: sym_heuristic=$sym_heuristic {

  $cdec/utils/atools -i $align_fwd -j $align_rev -c $sym_heuristic > $alignments
}

# Tutorial Step #5
# Estimated time: ~1 minute
# Compile a suffix array over the training data for use in grammar extraction
# Note: The "dot parameters" below are for use with ducttape's scheduler integration.
#       If you're interested, go check out the ducttape tutorial
task index_training : cdec
    < train=$out@filter_training
    < align=$alignments@align_sym
    > ini
    > sa
    :: .cpus=1 .walltime="0:30:00" .vmem=10g .submitter=shell .q=shared {

  export PYTHONPATH=`echo $cdec/python/build/lib.*`
  python -m cdec.sa.compile -b $train -a $align -c $ini -o $sa
}

# Tutorial Step #6
# Extract grammars for dev and devtest
# Estimated time: 15 minutes
# For more information on the SCFG's extracted in this step, see http://www.cdec-decoder.org/concepts/scfgs.html
# See also the cdec grammar format at http://www.cdec-decoder.org/documentation/rule-format.html
task extract_gra : cdec
    < corpus=(ExtractSection: dev=$out@preproc_corpus[DataSection:dev] devtest=$out@preproc_corpus[DataSection:devtest])
    < ini=$ini@index_training
    > sgm
    :: cores=$extract_gra_cores
    :: .cpus=1 .walltime="3:00:00" .vmem=10g .submitter=shell .q=shared {

  # SGM file contains input sentences surrounded by sgml markup
  # that tells the decoder where to find the grammar for each sentence
  export PYTHONPATH=`echo $cdec/python/build/lib.*`
  python -m cdec.sa.extract -c $ini -g grammars -j $cores < $corpus > $sgm
}

# Tutorial step #7
# Build the target language model
# (The task "grab_parallel_tgt" in tutorial.tconf is also part of tutorial step #7)
# More on language models at http://www.cdec-decoder.org/concepts/language-models.html
task build_lm : srilm
    < corpus=$lm_data
    > arpa
    :: lm_order=$lm_order {

  cat $corpus | $srilm/bin/*/ngram-count -unk -text - -interpolate -kndiscount -order $lm_order -lm $arpa
}

# Tutorial step #8
# Compile the target language model into an efficient binary format for KenLM
task compile_lm : cdec
    < arpa=$arpa@build_lm
    > klm {
  $cdec/klm/lm/build_binary $arpa $klm
}

# Tutorial step #9
# Create cdec ini file (the init weights file is specified in tutorial.tconf)
task make_cdec_ini
    < klm=$klm@compile_lm
    > ini {
  echo "formalism=scfg" >> $ini
  echo "add_pass_through_rules=true" >> $ini
  echo "density_prune=80" >> $ini
  echo "feature_function=WordPenalty" >> $ini
  echo "feature_function=KLanguageModel $klm" >> $ini
}

# Tutorial step #10
# Tune system using development data with MERT
# Esimated time: 20 - 40 minutes
#
# More resources:
# * Linear models: http://www.cdec-decoder.org/concepts/linear-models.html
# * Discriminative training: http://www.cdec-decoder.org/concepts/training.html
# * Minimum error rate training: http://www.cdec-decoder.org/documentation/mert.html 
task tune_mert : cdec
    < weights_init=$weights_init
    < dev_sgm=$sgm@extract_gra[ExtractSection:dev]
    < cdec_ini=$ini@make_cdec_ini
    > weights=dpmert/weights.final
    :: cores=$tune_cores {
  $cdec/dpmert/dpmert.pl -w $weights_init -d $dev_sgm -c $cdec_ini -j $cores
}

# Tutorial step #11
# Evaluate test set using trained weights
# Estimated time: 5 minutes
task decode : cdec
    < cdec_ini=$ini@make_cdec_ini
    < weights=$weights@tune_mert
    < devtest_sgm=$sgm@extract_gra[ExtractSection:devtest]
    > scores
    :: cores=$decode_cores {
  $cdec/dpmert/decode-and-evaluate.pl -c $cdec_ini -w $weights -i $devtest_sgm -j $cores &> $scores
}

# Grab the primary field that we care about from the scores
summary scores {
  # 'bleu' will become a field name in our summary
  of decode > bleu {
    awk -F= '/BLEU=/{print $2}' < $scores > $bleu
  }
}

# Nuts and bolts:
global {
  ducttape_experimental_packages=true
  ducttape_experimental_submitters=true
}

#task score : multeval
#  < hyps=$hyps@decode
#  < e_test=@download
#  > scores
#  :: tgt_lang=en
#  :: score_cores=4
#  :: .submitter=shell .q=shared .cpus=4 .walltime="0:15:00" .vmem=6g {
#  $multeval/multeval.sh eval --refs $e_test --meteor.language $tgt_lang --threads $score_cores --hyps-baseline $hyps > $scores
#}
