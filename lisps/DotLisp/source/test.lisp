;Copyright (c) 2003, Rich Hickey
;licensed under the BSD license - see license.txt

true
false
(if true true false)
(if false true false)

nil
()

(eql? nil ())	;true

;nil is a constant
(eql? nil 'nil)	;true

(type-of 1)
(type-of 2.0)
(type-of "fred")
(type-of 'fred)
(type-of '(a b c))


;dynamic variables
(set *fred 5)
(set fred 5)
(def (foo) (prn fred) (prn *fred))
(foo)
(def (bar) (let (fred 10) (dynamic-let (*fred 10) (foo))))
(bar)
(foo)	;*fred is restored

:a-keyword	 ;evals to itself

'(a b c)

(type-of ["1" 2 3])		;heterogenous - Object[]
(type-of [1 2 3])		;homogenous - Int32[]

Int32.
(set ht (Hashtable. 1000))
(.Count ht)
(ht.Add "fred" "ethel")
(ht.Add "ricky" "lucy")
ht.Count
(String:Concat "Hello " "World")
(set e (.IEnumerable:GetEnumerator ht))
(while e.MoveNext
	(prn e.Current))
	
(type-of _)

$
$$$

!
!

(eql? "fred" "fred")
(eqv? "fred" "fred")

(if nil false)
(if () false)
(if false false)

(when true true)
(when false false)

(not true)
(not false)
(not nil)

(when-not false false)

(cond 
	(> 2 3) false
	(> 3 2) true
	:else	"else")

(cond 
	(> 2 3) false
	(< 3 2) true
	:else	"else")

(cond 
	(> 2 3) false
	(< 3 2) true )

(def (foo x)
	(case x
		(a b c) "symbol"
		(1 2 3)	"number"
		:else	"else"))

(foo 'c)
(foo 2)
(foo "none")

(and 1 "two" 'three 4 5)
(and 1 "two" nil 'three 4 5)

(or 1 "two" 'three 4 5)
(or false 1 "two" nil 'three 4 5)
(or false nil)


(def (foo &opt (x true)) x)
(foo)	;gets default
(foo _)	;this does too
(foo false)
(def (bar &opt x) (foo x))
(bar)	;here too, missing flows through
(bar false)

(def (prob x) 
	(cond
		(symbol? x) farkle))
		
;(def (frob x) 
;	(case x
;		(a b c) true
;		(b c d) (prob x)))

(def (frob x) 
	(cond
		(member x '(a b c)) true
		(member x '(b c d)) (prob x)))
