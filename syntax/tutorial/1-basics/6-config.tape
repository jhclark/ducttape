# Sometimes you might want your workflow to just be a template
# and specify different sets of input files later as a config
# file

# Given a step like this:
[g1.step2] a=$g1.step1/a b=$g1.step1/b > x y :: @@big
   cat < $a > $x
   cat < $b > $y


# You can specify inputs and parameters in a separate config file, like this:
g1.step2.a = /etc/passwd
