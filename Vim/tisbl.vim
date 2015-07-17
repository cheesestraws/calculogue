" Vim syntax file
" Language:	TISBL
" Maintainer:	Camilla Löwy <dreda@dreda.org>
" Last Change:	Sep 22, 2013
" File Types:   .t
" Version:      0.1

if exists("b:current_syntax")
  finish
endif

setlocal iskeyword=a-z,A-Z,48-57,_,.,-,\\,+,\:,;,#,\,,',?

syn match tisblNumber         display /\<[,.:;]\?#\d\+\>/ contains=tisblNounStack
syn match tisblNumber         display /\<[,.:;]\?#\d\+.\d\+\>/ contains=tisblNounStack
syn match tisblWord           display /[,.:;]\?'[^ ]\+/ contains=tisblNounStack

syn match tisblVerb           display /\<\\[^ ]\+\>/ contains=tisblVerbStack

syn match tisblVerbStack      display contained /\<\\[,.:;]\?/
syn match tisblVerbStack      display contained /[,.:;]\>/

syn match tisblNounStack      display contained /\<[,.:;]/

syn match tisblComment        display /%.*$/ contains=tisblCommentTodo,@Spell
syn keyword tisblCommentTodo  contained TODO FIXME XXX

hi def link tisblNumber       Number
hi def link tisblWord         String
hi def link tisblVerb         Identifier
hi def link tisblVerbStack    StorageClass
hi def link tisblNounStack    StorageClass
hi def link tisblComment      Comment
hi def link tisblCommentTodo  Todo

