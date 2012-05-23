#!/usr/bin/env ducttape

# An example using cdec to build a statistical machine translation system
# for the Kyoto Free Translation Task (KFTT)

# You will likely have to edit the 'package' blocks such that the paths match your local environment

package cdec :: cores=16 .versioner=git .repo="git://github.com/redpony/cdec.git" .ref=HEAD {
  autoreconf -ifv
  ./configure
  make

  # Build Adam Lopez' sa-extract
  (
    cd sa-extract
    make \
      PYVER=python2.7 \
      PYDIR=/home/jhclark/prefix \
      CYTHON=/home/jhclark/software/Cython-0.15.1/bin/cython
  )
}

package sametime :: .versioner=git .repo="git://github.com/jhclark/sametime.git" .ref=HEAD {
  ./build.sh
}

package multeval :: .versioner=git .repo="git://github.com/jhclark/multeval.git" .ref=HEAD {
  ./build.sh
}

# Sorry folks, due to the nasty license, you'll have to download the software yourselves
package srilm :: .versioner=disk .path="/home/jhclark/software/srilm" {
  make -j $cores SRILM=$PWD MACHINE_TYPE=i686-m64 MAKE_PIC=yes World
}
# TODO: Lowercasing and other such data munging

task download > f_train=kftt-data-1.0/data/tok/kyoto-train.cln.ja
              > e_train=kftt-data-1.0/data/tok/kyoto-train.cln.en
              > f_dev=kftt-data-1.0/data/tok/kyoto-dev.ja
              > e_dev=kftt-data-1.0/data/tok/kyoto-dev.en
              > f_test=kftt-data-1.0/data/tok/kyoto-test.ja
              > e_test=kftt-data-1.0/data/tok/kyoto-test.en {
  wget http://www.phontron.com/kftt/download/kftt-data-1.0.tar.gz
  tar -xvzf kftt-data-1.0.tar.gz
}

# Use Chris' blazing fast Model2-like aligner
# Has no length limits and is very fast, though quality isn't quite as good as HMM or M4
task align_fast : cdec < f_train=@download e_train=@download > align=align.final.gz {

  # Create the cdec "corpus" format
  paste <(zcat -f $f_train) <(zcat -f $e_train) | sed 's/\t/ ||| /g' > corpus.fe
  paste <(zcat -f $e_train) <(zcat -f $f_train) | sed 's/\t/ ||| /g' > corpus.ef

  # -A: Write alignments, not params
  # -d: favor the diagonal
  # -v: Use variational Bayes
  $cdec/training/model1 -A -d -v corpus.fe | gzip > align.fe.gz
  $cdec/training/model1 -A -d -v corpus.ef | gzip > align.ef.gz
  $cdec/util/atools -c invert -i align.ef.gz | gzip > align.fe.inv.gz
  $cdec/util/atools -c grow-diag-final-and \
      -i align.fe.gz -j align.fe.inv.gz \
    | gzip > align.final.gz
}

task build_sa : cdec
              < f_train=@download e_train=@download align=@align_fast
              > ini
              :: .cpus=1 .walltime="0:30:00" .vmem=10g .submitter=shell .q=shared {
  # Can use -i my_ini for custom features here
  $cdec/sa-extract/sa-compile.pl -o $PWD/sa-compiled -b nc=$f_corpus,$e_corpus -a gdfa=$align > $ini
}

task extract_gra : cdec
                 < f=(DataSection: tune=$f_dev@download test=$f_test@download)
                 < ini=@build_sa
                 > gra_dir
                 :: .cpus=1 .walltime="3:00:00" .vmem=10g .submitter=shell .q=shared {

  cat $f | $cdec/sa-extract/escape-testset.pl | $cdec/sa-extract/extractor.py -c $ini

  # Assign feature names and gzip
  mkdir -p $gra_dir
  feat_names=$cdec/sa-extract/sa_feat_names.txt
  for file in (cd $raw; ls grammar.out.*); do
    $cdec/sa-extract/sa2cdec.py $feat_names < $raw/$file | gzip > $gra_dir/$file.gz
  done
}

task build_lm : srilm < mono_corpus=$e_train@download > arpa=arpa.gz :: N=3
              :: .submitter="shell" {
  zcat -f $mono_corpus \
    $srilm/lm/bin/i686-m64/ngram-count \
      -kndiscount -order $N -interpolate -unk -text /dev/stdin -lm arpa
  gzip arpa
}

task binarize_lm : cdec < arpa=@build_lm > klm :: .cpus=1 .walltime="0:30:00" .vmem=16g .submitter=shell {
  $cdec/klm/lm/build_binary $arpa $klm
}

task tune_pro : cdec
              < f_dev=@download e_dev=@download gra_dir=@extract_gra[DataSection:tune] klm=@binarize_lm
              > weights ini
              :: num_refs=1 pro_iterations=30
              :: cores=32 .cpus=32 .walltime="24:00:00" .vmem=64g .submitter=shell .q=normal {

  # sent-level grammar provided by input sents via markup
  echo "formalism=scfg" >> $ini
  echo "feature_function=KLanguageModel $klm" >> $ini
  echo "feature_function=WordPenalty" >> $ini
  echo "add_pass_through_rules=true" >> $ini

  # Some reasonable starting weights...
  echo "Glue -0.1" >> weights.init
  echo "WordPenalty -1.0" >> weights.init
  echo "EGivenFCoherent -0.25" >> weights.init
  echo "PassThrough -0.35" >> weights.init
  echo "LanguageModel 0.30" >> weights.init
  echo "LanguageModel_OOV -1.25" >> weights.init
  
  $cdec/sa-extract/wrap_input.py $gra_dir/grammar.out. < $f_dev > dev.src.wrapped
  $cdec/dpmert/divide_refs.py $num_refs dev.refs. < $e_dev
  $cdec/pro-train/dist-pro.pl \
        --ref-files $PWD/dev.refs.'*' \
        --source-file $PWD/dev.src.wrapped \
        --weights $PWD/weights.init \
        --use-make 1 \
        --jobs $cores \
        --workdir $PWD \
        --max-iterations $N \
        --metric ibm_bleu \
        $ini
  cp $(ls -t weights.* | head -n1) $weights
  echo "weights=$weights" >> $ini
}

task decode : cdec sametime
            < f_test=@download
            < gra_dir=@extract_gra[DataSection:test]
            < weights=@tune_pro
            < ini=@tune_pro
            > hyps
            :: .submitter=shell .cpus=32 .walltime="1:00:00" .vmem=60g .q=shared {
  $cdec/sa-extract/wrap_input.py $gra_dir/grammar.out. < $f_test > test.src.wrapped
  $sametime/sametime "$cdec/decoder/cdec -c $ini" < test.src.wrapped > $hyps
}

task score : multeval < hyps=$hyps@decode e_test=@download > scores
           :: tgt_lang=en
           :: .submitter=shell .q=shared .cpus=4 .walltime="0:15:00" .vmem=6g {
  $multeval/multeval.sh eval --refs $e_test --meteor.language $tgt_lang --threads 4 --hyps-baseline $hyps > $scores
}
