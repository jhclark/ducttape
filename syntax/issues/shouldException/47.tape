# The named output does not match the actual file produced
#
# This tape should die after running this task
task foo > out=my_dir/file.txt {
     mkdir mydir
     echo "hello" > mydir/file.txt
}
