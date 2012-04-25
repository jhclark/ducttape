# Create a temporary dummy script for running qsub
export PATH=$PWD:$PATH
echo 'echo "$@" >> qsub.log' > qsub
trap "rm qsub" EXIT
chmod a+x qsub

ducttape 2-sge-simple.tape "$@"
