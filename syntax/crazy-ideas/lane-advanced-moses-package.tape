# Download and compile moses
task moses 
   # Binaries
   > decoder=dist/bin/moses
   > mert=dist/bin/mert
   > pro=dist/bin/pro

   # Dependencies
   :: modelblocks_dir=/free/lane/modelblocks
   :: giza_dir=/tools/giza/v1.0.6

   # Compiler options
  :: compiler_threads=10
  :: git_repo=www:/repos/git/Decoders/mosesdecoder.git
{
     git clone ${git_repo} .

     # Returns true iff the first parameter 
     # is the name of a defined variable
     # whose value is the path to a directory that actually exists
     function dir_defined {
         eval '[[ ${!'$1'[@]} && -d ${'$1'} ]]'
     }

     # Returns true iff the first parameter 
     # is the name of a defined variable
     function var_defined {
         eval '[[ ${!'$1'[@]} ]]'	     
     }


     # Tell moses where modelblocks lives
     if (dir_defined modelblocks); then
     	mkdir -p synlm/hhmm
	ln -s ${modelblocks_dir}/rvtl synlm/hhmm/
	ln -s ${modelblocks_dir}/wsjparse synlm/hhmm/
	synlm_flag="--with-synlm"
     else
	synlm_flag=""
     fi

     # Tell moses where GIZA++ lives
     if (dir_defined giza_dir); then
     	giza_flag="--with-giza=${giza_dir}"
     else
	giza_flag=""
     fi

     # Tell moses where IRSTLM lives
     if (dir_defined irstlm_dir); then
     	irstlm_flag="--with-irstlm=${irstlm_dir}"
     else
	irstlm_flag=""
     fi
     
     # Tell moses where SRILM lives
     if (dir_defined srilm_dir); then
     	srilm_flag="--with-srilm=${srilm_dir}"
     else
	srilm_flag=""
     fi

     # Tell moses where RandLM lives
     if (dir_defined randlm_dir); then
     	randlm_flag="--with-randlm=${randlm_dir}"
     else
	randlm_flag=""
     fi

     # Enable flag specifying number of compiler threads
     if (var_defined compiler_threads); then
     	compiler_threads_flag="-j${compiler_threads}"
     else
	compiler_threads_flag="-j1"
     fi

     ./bjam ${compiler_threads_flag} ${giza_flag} ${synlm_flag} ${irstlm_flag} ${srilm_flag} ${randlm_flag}
}
