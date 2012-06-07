dir=$(cd $(dirname $0); pwd)
ducttape $dir/01-08-config-extern.tape -C $dir/01-08-config-extern.conf "$@"
