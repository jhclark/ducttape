# Bash programmable tab completion routines for ducttape

# For bash, see http://tldp.org/LDP/abs/html/tabexpansion.html
# see also http://fahdshariff.blogspot.com/2011/04/writing-your-own-bash-completion.html
# see also http://stackoverflow.com/questions/511683/bash-get-list-of-commands-starting-with-a-given-string
# on using -S ' ' -- see http://www.selenic.com/pipermail/mercurial/2006-January/006285.html

# Apparently, zsh can emulate bash compinit with "bashcompinit"
# see http://www.zshwiki.org/home/convert/bash
if [ -z "$BASH_VERSION" ]; then # Assume zsh if not bash
    #if [[ "$SHELL" == *"zsh" ]]; then
    # Load bash-to-zsh compatibility layer
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
	      completeMatches=$(ls --color=n -1 $curPartial*.tape 2>/dev/null)
	      partialMatches=$(ls -d --color=n -1 $curPartial*/ 2>/dev/null)
	  # Since we're using -o nospace, we have to append spaces using -S' ' to indicate
	  # when a match is complete (as opposed to partial)
	      COMPREPLY=( $(compgen -W "$partialMatches") )
	      COMPREPLY=( ${COMPREPLY[@]} $(compgen -W "$completeMatches" -S ' ') )
	      if [[ "$curPartial" == *".tape" ]]; then
	      # Make sure proper files complete this fully (not as a partial match)
		  COMPREPLY=( ${COMPREPLY[@]} "$curPartial " )
	      fi
	      ;;
      esac
  else
      COMPREPLY=( "--purge" "--viz" )
  fi

  return 0
}

# -F use above function
# Disabled -o default (fallback on default completion)
# Last arg: What commands to invoke this for
complete -F _CompleteDucttape -o nospace ducttape
