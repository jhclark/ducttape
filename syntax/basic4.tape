# Now let's string 2 commands together
# First a step we already know about...
[first] > a
  seq 10 > $a

# We use first's output "a" as the input "x"
# by using the = operator
[andthen] b > x=$first.a
  cat < $b > $x
