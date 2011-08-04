# TODO: Unfinished tutorial step

# - This does lead to confusion in reports:
#   A report defaults to running for all hyperedges, unless some specializations are specified
[whichThing] a > aOut :: paramA
  # TODO: Need a better way of specifying individual inputs to packed realizations
  +baseline [choiceA] b :: param=$paramA
    cat $a $b $aOut
  +alt [choiceB] param=$x
    ln -s $a $aOut
  +alt [choiceC] param=$x
    ln -s $a $aOut
  +report reportA # Basic report, run for all non-specialized hyperedges (B/C), since only choiceA is specialized below
    echo "Line1=$(head -n1 $a)"
  +report reportA!(choiceA) # Specializations listed after ! inside () -- node not necessary, since it it always this one
    echo "Line1=$(head -n1 $a)"
    echo "Line1B=$(head -n1 $b)"


