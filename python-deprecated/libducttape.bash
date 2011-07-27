# To be sourced by calling bash scripts

function require {
    name=$1
    (
	conditional=0
	for arg in $@; do
	    if [[ "${arg:0:3}" == 'iff' ]]; then
		conditional=1
	    fi
	done
	
	set +u
	value=${!name}
	if [[ $conditional == 0 && -z $value ]]; then
	    echo >&2 "ERROR: Variable not defined: ${name}"
	    exit 1
	fi
    )
}