task foo < undefined_dir="" {
	if [[ -n "${undefined_dir}" && -d "${undefined_dir}" ]]; then
		echo "Actual value is ${undefined_dir}"
		exit 1
	else
		echo "Desired behavior"
	fi
}
