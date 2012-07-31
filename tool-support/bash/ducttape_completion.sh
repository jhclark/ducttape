# Bash programmable tab completion routines for ducttape

# For bash, see http://tldp.org/LDP/abs/html/tabexpansion.html
# see also http://fahdshariff.blogspot.com/2011/04/writing-your-own-bash-completion.html
# see also http://stackoverflow.com/questions/511683/bash-get-list-of-commands-starting-with-a-given-string
# on using -S ' ' -- see http://www.selenic.com/pipermail/mercurial/2006-January/006285.html

# This script may be installed in /etc/bash_completion

# Apparently, zsh can emulate bash compinit with "bashcompinit"
# see http://www.zshwiki.org/home/convert/bash
if [ -z "$BASH_VERSION" ]; then # Assume zsh if not bash
    #if [[ "$SHELL" == *"zsh" ]]; then
    # Load bash-to-zsh compatibility layer
    echo >&2 "Using bash to zsh compatibility layer for completion"
    autoload -Uz bashcompinit
    bashcompinit
fi

_CompleteDucttape() {
  COMPREPLY=() # Array of valid completions
  curPartial=${COMP_WORDS[COMP_CWORD]} # What the user typed so far

  if [[ $COMP_CWORD == 1 ]]; then
      case "$curPartial" in
	  -*) COMPREPLY=( $(compgen -W '--help') ) ;;
	  *)
	      completeMatches="$(ls --color=n -1 $curPartial*.tape 2>/dev/null)"
	      partialMatches=$(ls -d --color=n -1 $curPartial*/ 2>/dev/null)
              # Since we're using -o nospace, we have to append spaces using -S' ' to indicate
	      # when a match is complete (as opposed to partial)
	      COMPREPLY=( ${COMPREPLY[@]} $(compgen -W "$completeMatches" -S ' ') )
              echo >&2 "$completeMatches"
              echo >&2 "${COMPREPLY[0]}"
	      COMPREPLY=( ${COMPREPLY[@]} $(compgen -W "$partialMatches") )
	      if [[ "$curPartial" == *".tape" ]]; then
	      # Make sure proper files complete this fully (not as a partial match)
		  COMPREPLY=( ${COMPREPLY[@]} "$curPartial " )
	      fi
	      ;;
      esac
  else
      COMPREPLY=( "--purge" "--viz" "--env" "--markDone" )
  fi

  echo >&2 ${COMPREPLY[0]}

  return 0
}

function _CompleteCdd {
  COMPREPLY=() # Array of valid completions
  curPartial=${COMP_WORDS[COMP_CWORD]} # What the user typed so far

  if [[ $COMP_CWORD == 1 ]]; then
      # special case for trailing slash (dirname hacks off last directory)
      if [[ "$curPartial" == *"/" ]]; then
	  baseDir="$curPartial"
	  partialRealization=""
      else
	  baseDir=$(dirname "$curPartial")
	  partialRealization=$(basename "$curPartial")
      fi
      # Strip any / suffix
      baseDir=${baseDir%/}

      #echo >&2 "Base: $baseDir $partialRealization"
      if [ -e $baseDir/.ducttape_autocomplete ]; then
          #echo >&2 "Using special completion"
          # Escape +'s in partial realization using bash string manipulation
          escapedRealization=${partialRealization/+/\\+}
	  result=$(awk "/^${escapedRealization}/{print \"${baseDir}/\"\$1}" < $baseDir/.ducttape_autocomplete)
	  #echo >&2 "XXX: $curPartial --> $result"
	  COMPREPLY=( $result )
          if (( ${#COMPREPLY[@]} == 1 )); then
              # There's exactly one answer, so recommend adding a slash
	      COMPREPLY=( $result"/" )
          fi
      else 

          # If the penultimate directory doesn't exist, it might be b/c we autocompleted it to a human-readable
          # realization name, which actually doesn't exist
          if [[ "$curPartial" == *"/" && ! -e $curPartial ]]; then
              baseDir=$(dirname "$curPartial")
              dir=$(basename "$curPartial") # human-readable realization
              escapedRealization=${dir/+/\\+}
              if [ -e $baseDir/.ducttape_autocomplete ]; then
                  result=$(awk "/^${escapedRealization}/{print \"${baseDir}/\"\$2}" < $baseDir/.ducttape_autocomplete)
#                  echo >&2 "result: $result"
                  COMPREPLY=( $result"/" )
              fi
          else
              # just complete as normal
	      #partialMatches=$(ls -d --color=n -1 $curPartial*/ 2>/dev/null)
              COMPREPLY=( $(compgen -f $curPartial ) )

              # For zsh -- recommend /'s for directories
              if [[ ${#COMPREPLY[@]} == 1 && -d ${COMPREPLY[0]} ]]; then
                  # There's exactly one answer, so recommend adding a slash
	          COMPREPLY=( ${COMPREPLY[0]}"/" )
              fi


                  # Since we're using -o nospace, we have to append spaces using -S' ' to indicate
	          # when a match is complete (as opposed to partial)
	  #COMPREPLY=( ${COMPREPLY[@]} $(compgen -W "$completeMatches" -S ' ') )
                  #echo >&2 "$completeMatches"
                  #echo >&2 "${COMPREPLY[0]}"
	  #COMPREPLY=( ${COMPREPLY[@]} $(compgen -W "$partialMatches") )
              
          fi
      fi
  fi

  #echo >&2 "YYY: ${COMPREPLY[0]}"

  return 0
}

# -F use above function
# Disabled -o default (fallback on default completion)
# Last arg: What commands to invoke this for
# TODO: See also "COMP_WORDBREAKS"
complete -r ducttape 2>/dev/null
complete -F _CompleteDucttape -o nospace ducttape

complete -r cd 2>/dev/null
complete -F _CompleteCdd -o nospace cd