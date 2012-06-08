# Create a temporary dummy script for running qsub
export PATH=$PWD:$PATH
echo 'echo "$@" >> qsub.log' > qsub
trap "rm qsub" EXIT
chmod a+x qsub

ducttape 06-02-sge-simple.tape "$@"
