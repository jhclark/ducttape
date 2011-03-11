# To be sourced by calling bash scripts

function require {
    name=$1
    (
	set +u
	value=${!name}
	if [ -z $value ]; then
	    echo >&2 "ERROR: Variable not defined: ${name}"
	    exit 1
	fi
    )
}