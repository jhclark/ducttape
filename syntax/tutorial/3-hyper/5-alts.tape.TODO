# TODO: Finish this example

# Using inline alternatives

[whichThing] a > aOut :: paramA
  # TODO: Need a better way of specifying individual inputs to packed realizations
  +baseline [choiceA] b :: param=$paramA
    cat $a $b $aOut
  +alt [choiceB] param=$x
    ln -s $a $aOut
  +alt [choiceC] param=$x
    ln -s $a $aOut
