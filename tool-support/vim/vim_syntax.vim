" Vim syntax file
" Language: Ducctape Workflow
" Maintainer: Jonathan Clark
" Latest Revision: Feb 6, 2012
"
" For help modifying this file, see http://vim.wikia.com/wiki/Creating_your_own_syntax_files

if exists("b:current_syntax")
  finish
endif

syn match dtComment "^#.*"
syn match dtHeader "^\[[^]]+\]"
syn match dtBash "^\s.*"

let b:current_syntax = "ducttape"

hi def link dtComment     Comment
hi def link dtHeader     Type
hi def link dtBash     Constant
