# The named output does not match the actual file produced
#
# This tape should die after running this task (note: my_dir vs mydir)
task foo > out=my_dir/file.txt {
     mkdir mydir
     echo "hello" > mydir/file.txt
}
