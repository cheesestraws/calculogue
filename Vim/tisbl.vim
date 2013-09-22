" Vim syntax file
" Language:	TISBL
" Maintainer:	Camilla Löwy <dreda@dreda.org>
" Last Change:	Sep 22, 2013
" File Types:   .t
" Version:      0.1

if exists("b:current_syntax")
  finish
endif

setlocal iskeyword=a-z,A-Z,48-57,_,.,-,\\,+,\:,;,#,\,,?

syn match tisblNumber         display /\<[,.:;]\?#\d\+\>/
syn match tisblNumber         display /\<[,.:;]\?#\d\+.\d\+\>/
syn match tisblWord           display /[,.:;]\?'[^ ]\+/

syn match tisblVerb           display /\<\\[^ ]\+\>/ contains=tisblStack

syn match tisblStack          contained /[,.:;]/

syn match tisblComment        /%.*$/ contains=tisblCommentTodo,@Spell
syn keyword tisblCommentTodo  contained TODO FIXME XXX

hi def link tisblNumber       Number
hi def link tisblWord         String
hi def link tisblVerb         Identifier
hi def link tisblStack        StorageClass
hi def link tisblLiteralStack StorageClass
hi def link tisblComment      Comment
hi def link tisblCommentTodo  Todo

