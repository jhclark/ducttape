task uses_nested < file=$ref_test :: N=$num_refs {
  echo "I will be reading file $file with $N refs"
  head -n $N $file
}

global {
  # Adding multiple branches from a config file
  # TODO: Another example with multiple language pairs
  #       (multiple config files, each which introduces a realization?)
  num_refs=(
    Test:
    baseline=(
      NumRefs:
      oneRef=1
      multiRef=4
    )
    mt02=(
      NumRefs:
      oneRef=1
      multiRef=16
    )
  )

  ref_test=(
    Test:
    baseline=(
      NumRefs:
      oneRef=small.txt
      multiRef=small.txt
    )
    mt02=(
      NumRefs:
      oneRef=big.txt
      multiRef=big.txt
    )
  )
}