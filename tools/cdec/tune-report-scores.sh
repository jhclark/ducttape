#!/usr/bin/env bash
set -eo pipefail

bleu=$(egrep -ho '^DECODER SCORE: [0-9.]+' vest.log | tail -n 1 | egrep -ho '[0-9.]+')
echo BLEU$'\t'$bleu

