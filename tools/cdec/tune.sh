#!/usr/bin/env bash
set -auxeo pipefail
echo $PATH
source libducttape.bash

# Use case insensitive comparisons
shopt -s nocasematch
dir=$PWD

# Inputs
require I_initWeights
require I_lm
require I_grammar
require I_fTune
require I_eTune
require I_glcSrcVocab iff="useGlc == True"
require I_glcTgtVocab iff="useGlc == True"
require I_glcSrcStopwords iff="useGlc == True"
require I_glcTgtStopwords iff="useGlc == True"
require I_glcCooc iff="useGlc == True"
require I_glcFeatWeights iff="useGlc == True"
require I_glcAllowableFeats iff="useGlc == True"
require I_glcFeatConfig iff="useGlc == True"

# Params
require P_densityPrune default=150
require P_lmOrder 
require P_useMake required=false
require P_useGlc default=false

# Tool paths
require T_cdec

# Outputs
require O_1best
require O_weights

rm -f cdec.ini
echo "formalism=scfg" >> cdec.ini
echo "grammar=$I_grammar" >> cdec.ini
echo "feature_function=KLanguageModel $I_lm" >> cdec.ini
echo "feature_function=WordPenalty" >> cdec.ini
echo "add_pass_through_rules=true" >> cdec.ini
#echo "cubepruning_pop_limit=200" >> cdec.ini
echo "density_prune=$P_densityPrune" >> cdec.ini

if [[ $P_useGlc == "True" ]]; then
    echo "feature_function=ContextCRF --srcVocabFile $I_glcSrcVocab --tgtVocabFile $I_glcTgtVocab --srcStopwordFile $I_glcSrcStopwords --tgtStopwordFile $I_glcTgtStopwords --weightsFile $I_glcFeatWeights --coocFile $I_glcCooc --allowableFeatsFile $I_glcAllowableFeats --config $I_glcFeatConfig" >> cdec.ini
    echo "feature_function=WordSet -N crf.ContentWordCount -v $I_glcTgtVocab" >> cdec.ini
    echo "feature_function=WordSet -N crf.NonContentWordCount -v $I_glcTgtVocab --oov" >> cdec.ini
    echo "feature_function=WordSet -N crf.StopWordCount -v $I_glcTgtStopwords" >> cdec.ini
    echo "feature_function=WordSet -N crf.NonStopWordCount -v $I_glcTgtStopwords --oov" >> cdec.ini
fi

mkdir separate-reference-files
numRefs=$(( $( wc -l < $I_eTune ) / $( wc -l < $I_fTune ) ))
awk "BEGIN{i=0} {print>>(\"separate-reference-files/ref\"i);i+=1;i%=$numRefs}" < $I_eTune

makeFlag=""
if [ -n ${P_useMake} ]; then
    makeFlag="--use-make ${P_useMake} --use-fork"
fi

#(trap "find $dir/vest-work | fgrep sentserver.log | xargs zcat -f; find $dir/vest-work/logs.1/ -iname 'cdec.*.ER' | xargs tail;" EXIT;
$T_cdec/vest/dist-vest.pl --workdir vest-work $makeFlag --source-file $I_fTune --ref-files $dir'/separate-reference-files/ref*' --weights $I_initWeights cdec.ini 2>&1 | tee vest.log
#; trap - EXIT) 

finalTopbestGz=$(ls -t vest-work/run.raw.*.gz | head -1)
zcat $finalTopbestGz > 1best.txt
finalWeights=$(ls -t vest-work/weights.* | head -1)
cp $finalWeights optWeights.txt

ln -s 1best.txt $O_1best
ln -s optWeights.txt $O_weights
