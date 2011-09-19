# Notice how using parameters as literals allows us to easily handle multiple languages, test sets, genres, data sizes, etc.
# TODO: Specify which things are always cross products and which things default to baseline (name starts with "&" ?) 
# NOTE: If a step ever uses the unpack operator to reference a step, it might mean that realization must always get run
# TODO: How messy does this start looking if we integrate scheduler submission?
# GRAMMAR CHANGE: Overload $ to mean both task dependence and variable resolution to allow using parameters as variables in literal filenames
# GRAMMAR CHANGE: Use * to glob and match branches (used for deciding config settings without introducing new realizations)
# GRAMMAR CHANGE: We will use @ instead of ! to mean branch grabbing
# GRAMMAR CHANGE: Use square brackets instead of parens for branch grabbing so that the grammar doesn't explode

# If we wanted to specify different amounts of memory per language (i.e. even more power than in align's .vmem),
# there should be a separate "input def" block
# for inputting files along with their settings (similar to a loonybin FILESYSTEM node)

[tok_src] < src=(sets: train=train.$lang.$sz tune=tune.$lang.$sz test=test.$lang.$sz)
          > tok
          :: lang=de sz=(size: small=small large=large)
          :: .vmem=2gb .cpus=1
  /home/jhclark/workspace/moses/scripts/tokenizer/tokenizer.pl -l $lang < $src > $tok

[tok_tgt] < tgt=(sets: train=train.$lang.$sz tune=tune.$lang.$sz test=test.$lang.$sz)
          > tok
          :: lang=en size=(size: small=small large=large)
          :: .vmem=2gb .cpus=1
  /home/jhclark/workspace/moses/scripts/tokenizer/tokenizer.pl -l $lang < $tgt > $tok

# TODO: Split so that it can be parallelized/resumed
[align] < src=$tok_src/tok@sets[train] tgt=$tok_tgt/tok@sets[train] > $symmetrized
        :: .vmem=(size: small=2gb large=16gb) .cpus=8
  TODO > $symmetrized

# Setup Adam Lopez's extractor.
# Extract a grammar for both the tuning set and test set with the magic of realizations
[extract_gra] < src=$tok_src/tok@sets[train] tgt=$tok_tgt/tok@sets[train] align=$align/symmetrized
              < filteredTo=(filterSet: tune=$tok_src/tok@sets[tune] test=$tok_src/tok@sets[test])
              > gra
              :: .vmem=16gb .cpus=1
  /home/jhclark/workspace/sa-extract TODO $filteredTo > $gra

[tune] < src=$tok_src/tok@sets[tune] refs=$tok_tgt/tok@sets[tune] gra=$extract_gra/gra@filterSet[tune]
       > hyps weights
       :: .submitter=localhost # submits its own jobs
  /home/jhclark/workspace/cdec/pro-trainer/dist-pro.pl TODO

[decode] < src=$tok_src/tok@sets[test] gra=$extract_gra/gra@filterSet[test] weights=$tune/weights
         > hyps
         :: .vmem=16gb .cpus=8
  /home/jhclark/workspace/cdec/decoder/cdec TODO -w $weights > $hyps

[eval] < hyps=(scoreSet: tune=$tune/hyps test=$decode/hyps)
       < refs=(scoreSet: tune=$tok_tgt/tok@sets[tune] test=$tok_tgt/tok@sets[test])
       > scores
       :: .vmem=4gb .cpus=1
  /home/jhclark/workspace/multeval/multeval.sh TODO $hyps $refs > $scores

# NOTE: We could have multiple meanings of $ in the same input:
# e.g. weights=$tune/weights/$lang
# We will resolve this based on type of the name found (if there's a collision, we throw an error)