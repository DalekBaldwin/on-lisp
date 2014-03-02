(in-package :cl-user)

(defpackage :on-lisp
  (:use :cl)
  (:export
   #:last1
   #:single
   #:append1
   #:conc1
   #:mklist
   #:longer
   #:filter
   #:group
   #:flatten
   #:prune
   #:find2
   #:before
   #:after
   #:duplicate
   #:split-if
   #:most
   #:best
   #:mostn
   #:map0-n
   #:map1-n
   #:mapa-b
   #:map->
   #:mappend
   #:mapcars
   #:rmapcar
   #:readlist
   #:prompt
   #:break-loop
   #:mkstr
   #:symb
   #:reread
   #:explode
   #:!
   #:def!
   #:memoize
   #:compose
   #:fif
   #:fint
   #:fun
   #:lrec
   #:ttrav
   #:trec
   #:defnode
   #:run-node
   #:compile-net
   #:while
   #:mac
   #:our-expander
   #:our-defmacro
   #:our-macroexpand-1
   #:our-do
   #:make-initforms
   #:make-stepforms
   #:our-and
   #:our-andb
   #:for
   #:ntha
   #:nthb
   #:nthd
   #:nth-fn
   #:nthe
   #:ora
   #:or-expand
   #:orb
   #:our-let
   #:when-bind
   #:when-bind*
   #:with-gensyms
   #:condlet
   #:condlet-clause
   #:condlet-binds
   #:if3
   #:nif
   #:in
   #:inq
   #:in-if
   #:>case
   #:>casex
   #:till
   #:do-tuples/o
   #:do-tuples/c
   #:dt-args
   #:mvdo*
   #:mvdo-gen
   #:mvdo-rebind-gen
   #:mvpsetq
   #:shuffle
   #:mvdo
   #:allf
   #:nilf
   #:tf
   #:toggle
   #:toggle2
   #:concf
   #:conc1f
   #:concnew
   #:_f
   #:pull
   #:pull-if
   #:popn
   #:sortf
   #:most-of
   #:nthmost
   #:gen-start
   #:nthmost-gen
   #:genbez
   #:aif
   #:awhen
   #:awhile
   #:aand
   #:acond
   #:alambda
   #:ablock
   #:aif2
   #:when2
   #:awhile2
   #:acond2
   #:read2
   #:do-file
   #:fn
   #:rbuild
   #:build-call
   #:build-compose
   #:alrec
   #:on-cdrs
   #:our-copy-list
   #:our-remove-duplicates
   #:our-find-if
   #:our-some
   #:unions
   #:intersections
   #:differences
   #:maxmin
   #:atrec
   #:on-trees
   #:our-copy-tree
   #:count-leaves
   #:rfind-if
   #:delay
   #:force
   #:abbrev
   #:abbrevs
   #:propmacro
   #:propmacros
   #:a+
   #:a+expand
   #:alist
   #:alist-expand
   #:anaphex
   #:pop-symbol
   #:defanaph
   #:anaphex1
   #:anaphex2
   #:anaphex3
   #:defdelim
   #:ddfn
   #:dbind
   #:destruc
   #:dbind-ex
   #:with-matrix
   #:with-array
   #:with-struct
   #:with-places
   #:wplac-ex
   #:match
   #:varsym?
   #:binding
   #:vars-in
   #:var?
   #:if-match
   #:pat-match
   #:simple?
   #:gen-match
   #:match1
   #:_
   #:gensym?
   #:length-test
   #:make-db
   #:clear-db
   #:db-query
   #:db-push
   #:fact
   #:with-answer%
   #:interpret-query
   #:interpret-and
   #:interpret-or
   #:interpret-not
   #:lookup
   #:with-answer
   #:compile-query
   #:compile-simple
   #:compile-and
   #:compile-or
   #:compile-not
   #:=defun
   #:=bind
   #:=values
   #:=funcall
   #:=apply
   #:dft
   #:dft-node
   ;;#:=dft-node
   #:call-restart
   ;;#:=call-restart
   #:dft2
   ;;#:=dft2
   #:fork
   #:program
   #:wait
   #:yield
   #:setpri
   #:choose
   #:choose-bind
   #:fail
   #:def-atn-node
   #:down
   #:cat
   #:jump
   #:compile-cmds
   #:up
   #:getr
   #:set-register
   #:setr
   #:pushr
   
   )
  )

(defpackage :on-lisp.02
  (:use :cl)
  (:export
   ;;#:make-dbms
   ;;#:fact
   ;;#:recurser
   ;;#:compall
   ))

(defpackage :on-lisp.03
  (:use :cl)
  (:export
   ;;#:bad-reverse
   ;;#:good-reverse
   ;;#:our-nreverse
   ;;#:nr2
   ))

(defpackage :on-lisp.04
  (:use :cl)
  (:export
   #:last1
   #:single
   #:append1
   #:conc1
   #:mklist
   #:longer
   #:filter
   #:group
   #:flatten
   #:prune
   #:find2
   #:before
   #:after
   #:duplicate
   #:split-if
   #:most
   #:best
   #:mostn
   #:map0-n
   #:map1-n
   #:mapa-b
   #:map->
   #:mappend
   #:mapcars
   #:rmapcar
   #:readlist
   #:prompt
   #:break-loop
   #:mkstr
   #:symb
   #:reread
   #:explode
   ))

(defpackage :on-lisp.05
  (:use :cl)
  (:export
   ;;#:*!equivs*
   #:!
   #:def!
   #:memoize
   #:compose
   #:fif
   #:fint
   #:fun
   #:lrec
   ;;#:rfind-if
   #:ttrav
   #:trec
   ))

(defpackage :on-lisp.06
  (:use :cl)
  (:export
   ;;#:node
   ;;#:*nodes*
   #:defnode
   #:run-node
   #:complie-net
   ))

(defpackage :on-lisp.07
  (:use :cl)
  (:export
   ;;#:nif
   #:while
   #:mac
   #:our-dolist
   ;;#:when-bind
   #:our-expander
   #:our-defmacro
   #:our-macroexpand-1
   #:our-do
   #:make-initforms
   #:make-stepforms
   #:our-and
   #:our-andb
   ))

(defpackage :on-lisp.08
  (:use :cl)
  (:export
   ;;#:move-objs
   ;;#:scale-objs
   ;;#:with-redraw
   ))

(defpackage :on-lisp.09
  (:use :cl)
  (:export
   #:for
   ))

(defpackage :on-lisp.10
  (:use :cl)
  (:export
   #:ntha
   #:nthb
   #:nthd
   #:nth-fn
   #:nthe
   #:ora
   #:or-expand
   #:orb
   ))

(defpackage :on-lisp.11
  (:use :cl)
  (:import-from :on-lisp.04
                :mklist
                :map0-n
                :map1-n
                :mappend
                :group)
  (:export
   #:our-let
   #:when-bind
   #:when-bind*
   #:with-gensyms
   #:condlet
   #:condlet-clause
   #:condlet-binds
   ;;#:with-db
   ;;#:with-db-fn
   #:if3
   #:nif
   #:in
   #:inq
   #:in-if
   #:>case
   #:>casex
   #:till
   #:do-tuples/o
   #:do-tuples/c
   #:dt-args
   #:mvdo*
   #:mvdo-gen
   #:mvdo-rebind-gen
   #:mvpsetq
   #:shuffle
   #:mvdo
   ))

(defpackage :on-lisp.12
  (:use :cl)
  (:import-from :on-lisp.11
                :with-gensyms)
  (:export
   #:allf
   #:nilf
   #:tf
   #:toggle
   #:toggle2
   #:concf
   #:conc1f
   #:concnew
   #:_f
   #:pull
   #:pull-if
   #:popn
   #:sortf
   ;;#:*cache*
   ;;#:retrieve
   ))

(defpackage :on-lisp.13
  (:use :cl)
  (:import-from :on-lisp.04
                :map0-n
                :map1-n)
  (:import-from :on-lisp.11
                :with-gensyms)
  (:export
   #:most-of
   #:nthmost
   #:gen-start
   #:nthmost-gen
   ;;#:*segs*
   ;;#:*du*
   ;;#:*pts*
   #:genbez
   ))

(defpackage :on-lisp.14
  (:use :cl)
  (:export
   #:aif
   #:it
   #:awhen
   #:awhile
   #:aand
   #:acond
   #:alambda
   #:ablock
   #:aif2
   #:when2
   #:awhile2
   #:acond2
   #:read2
   #:do-file
   ))

(defpackage :on-lisp.15
  (:use :cl)
  (:import-from :on-lisp.05
                :lrec
                :trec)
  (:export
   #:fn
   #:rbuild
   #:build-call
   #:build-compose
   #:alrec
   #:on-cdrs
   #:our-copy-list
   #:our-remove-duplicates
   #:our-find-if
   #:our-some
   #:unions
   #:intersections
   #:differences
   #:maxmin
   #:atrec
   #:on-trees
   #:our-copy-tree
   #:count-leaves
   ;;#:flatten
   #:rfind-if
   ;;#:unforced
   ;;#:*unforced*
   #:delay
   ;;#:forced
   ;;#:closure
   #:force
   ))

(defpackage :on-lisp.16
  (:use :cl)
  (:import-from :on-lisp.04
                :group)
  (:import-from :on-lisp.12
                :_f)
  (:import-from :on-lisp.14
                :it)
  (:export
   #:abbrev
   #:abbrevs
   #:propmacro
   #:propmacros
   #:a+
   ;;#:it
   #:a+expand
   #:alist
   #:alist-expand
   #:defanaph
   #:anaphex
   #:pop-symbol
   #:anaphex1
   #:anaphex2
   #:anaphex3
   #:asetf
   ))

(defpackage :on-lisp.17
  (:use :cl)
  (:export
   #:defdelim
   #:ddfn
   ))

(defpackage :on-lisp.18
  (:use :cl)
  (:import-from :on-lisp.04
                :symb)
  (:import-from :on-lisp.11
                :with-gensyms)
  (:import-from :on-lisp.14
                :aif
                :it
                :acond2
                :aif2)
  (:export
   #:dbind
   #:destruc
   #:dbind-ex
   #:with-matrix
   #:with-array
   #:with-struct
   #:with-places
   #:wplac-ex
   #:match
   ;;#:it
   #:varsym?
   #:binding
   #:if-match
   #:vars-in
   #:var?
   #:pat-match
   #:simple?
   #:gen-match
   #:match1
   #:_ ;; !!!
   #:gensym?
   #:length-test
   ))

(defpackage :on-lisp.19
  (:use :cl)
  (:import-from :on-lisp.11
                :with-gensyms)
  (:import-from :on-lisp.14
                :it
                :aif2)
  (:import-from :on-lisp.18
                :match
                :binding
                :vars-in
                :pat-match
                :simple?
                :_)
  (:export
   #:make-db
   ;;#:*default-db*
   #:clear-db
   #:db-query
   #:db-push
   #:fact
   #:with-answer%
   #:interpret-query
   #:interpret-and
   #:interpret-or
   #:interpret-not
   #:lookup
   #:with-answer
   #:compile-query
   #:lisp
   #:compile-simple
   #:compile-and
   #:compile-or
   #:compile-not
   ))

(defpackage :on-lisp.20
  (:use :cl)
  (:export
   ;;#:cont
   #:=lambda
   #:=defun
   #:=defuns
   #:=bind
   #:=values
   #:=funcall
   #:=apply
   #:dft
   ;;#:*saved*
   ;;#:=call-restart
   #:call-restart
   ;;#:=dft-node
   #:dft-node
   ;;#:=dft2
   #:dft2
   ))

(defpackage :on-lisp.21
  (:use :cl)
  (:import-from :on-lisp.20
                :=defun)
  (:export
   ;;#:proc
   ;;#:pri
   ;;#:state
   #:wait
   ;;#:*halt*
   ;;#:*default-proc*
   #:fork
   #:program
   ;;#:pick-process
   ;;#:most-urgent-process
   ;;#:arbitrator
   #:yield
   #:setpri
   #:halt
   #:kill
   ))

(defpackage :on-lisp.22
  (:use :cl)
  (:export
   #:*paths*
   ;;#:failsym
   #:choose
   #:choose-bind
   ;;#:cb
   #:fail
   ))

(defpackage :on-lisp.23
  (:use :cl)
  (:import-from :on-lisp.11
                :with-gensyms)
  (:import-from :on-lisp.20
                :=defun
                :=defuns
                :=bind
                :=values)
  (:import-from :on-lisp.22
                :*paths*
                :choose
                :fail)
  (:export
   #:def-atn-node
   #:def-atn-nodes
   #:down
   #:cat
   #:jump
   #:compile-cmds
   #:up
   #:getr
   #:set-register
   #:setr
   #:pushr
   #:*types*
   #:with-parses
   #:parse))

(defpackage :on-lisp.24
  (:use :cl)
  (:import-from :on-lisp.04
                :symb)
  (:import-from :on-lisp.11
                :with-gensyms)
  (:import-from :on-lisp.12
                :conc1f)
  (:import-from :on-lisp.14
                :it
                :acond2
                :aif2)
  (:import-from :on-lisp.18
                :match
                :varsym?
                :binding
                :vars-in
                :simple?
                :_
                :gensym?
                )
  (:import-from :on-lisp.19
                :lisp)
  (:import-from :on-lisp.20
                :=lambda
                :=defun
                :=defuns
                :=bind
                :=values
                :=funcall
                )
  (:import-from :on-lisp.22
                :*paths*
                :choose-bind
                :fail
                )
  (:export
   #:with-inference%
   #:rep_
   #:fullbind
   #:prove-query
   #:prove-and
   #:prove-or
   #:prove-not
   #:prove-simple
   ;;#:*rlist*
   #:<-%
   #:with-inference%%
   ;;#:*rules*
   #:<-
   #:with-inference
   #:cut
   ))

(defpackage :on-lisp.25
  (:use :cl)
  (:export))

(in-package :on-lisp)
