#!/usr/bin/env bash
set -auxeo pipefail
source libducttape.bash

# Inputs
require I_grammar
require I_lm
require I_fTest
require I_weights

# Parameters

# Outputs
require O_1best

# Tool paths
require T_cdec

echo "formalism=scfg" >> cdec.ini
echo "grammar=$I_grammar" >> cdec.ini
echo "feature_function=KLanguageModel $I_lm" >> cdec.ini
echo "feature_function=WordPenalty" >> cdec.ini
echo "add_pass_through_rules=true" >> cdec.ini
# TODO: Remove these file checks in favor of having duct tape do it for us
[ -e $I_fTest ] || (echo >&2 "Input sents not found: $I_fTest"; exit 1)
[ -e $I_weights ] || (echo <&2 "Optmized weights not found: $I_weights"; exit 1)
$T_cdec/decoder/cdec -c cdec.ini -i $I_fTest -w $I_weights -r -P > $O_1best
