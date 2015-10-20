;Copyright (c) 2003, Rich Hickey
;licensed under the BSD license - see license.txt

(Trace:Listeners.Add (TextWriterTraceListener. Console:Error))

(__set list (fn (&rest args) args))	;hmmm, presumes implementation detail

(__set def (macro (var &rest body)
	(Trace:WriteLine (String:Format "defined: {0}" 
						(if (cons? var) (first var) var)))
	(if (cons? var)
		(list '__set (first var) (cons 'fn (cons (rest var) body)))
		(cons '__set (cons var body)))))

;todo - capture test as 'it for use in result?
(def cond (macro (&rest clauses)
	(if (nil? clauses)
		nil
		(if (eql? (first clauses) :else)
			(second clauses)
			(list 'if (first clauses)
				(second clauses)
				(cons 'cond (rest (rest clauses))))))))
					
(def and (macro (&rest args)
	(cond	(nil? args) true
			(nil? (rest args)) (first args)
			:else (list 'if (first args) (cons 'and (rest args)) nil))))

(def (constant? exp) 
	(if (cons? exp) 
			(eql? (first exp) 'quote) 
			(not (symbol? exp)))) 

(def (__starts-with? lst x)
	(and (cons? lst) (eql? (first lst) x)))
	
;from PAIP
(def backquote (macro (x) (__backquote-expand x)))
	
(def (__backquote-expand x)
	(cond
		(atom? x) (if (constant? x) x (list 'quote x))
		(__starts-with? x 'unquote) (second x)
		(__starts-with? x 'backquote) 
			(__backquote-expand (__backquote-expand (second x)))
		(__starts-with? (first x) 'unquote-splicing)
			(if (nil? (rest x))
				(second (first x))
				(list 'append (second (first x)) (__backquote-expand (rest x))))
		:else (__backquote-combine 
						(__backquote-expand (first x)) 
						(__backquote-expand (rest x)) x)))

(def (__backquote-combine	left right x)
	(cond 
		(and (constant? left) (constant? right))
			(if (and (eql? (eval left) (first x)) (eql? (eval right) (rest x)))
				(list 'quote x)
				(list 'quote (cons (eval left) (eval right))))
		(nil? right) (list 'list left)
		(__starts-with? right 'list) (cons 'list (cons left (rest right)))
		:else (list 'cons left right)))

(def def-macro (macro (spec &rest body)
	`(def ~(first spec) (macro ~(rest spec) ~@body))))

(def-macro (nand &rest args)
	`(not (and ~@args)))
	
(def-macro (xor x y)
	`(if ~x (not ~y) ~y))

(def (odd? x)
	(not (even? x)))

(def-macro (error msg)
	`(throw (Exception. ~msg)))

(def (__pairize lst)
     (cond 
		(nil? lst)			nil
		(odd? (len lst))	(error "Expecting even number of arguments")
		:else				(cons (list (first lst) (second lst))
								(__pairize (nth-rest 2 lst)))))

(def-macro (let params &rest body)
	`(__let ~(__pairize params) ~@body))

#|			
(def-macro (__let bindings &rest body)
	`((fn ~(map->list (fn (x)
					(if (atom? (first x))
						(first x)
						(first (first x))))
				bindings)
			~@body)
		~@(map->list (fn (x)
					(if (atom? (first x))
						(second x)
						(list 'fn (rest (first x)) (second x))))
				bindings)))
|#

(def-macro (__let bindings &rest body)
	`((fn ~(map->list first	bindings)
			~@body)
		~@(map->list second	bindings)))
						
(def-macro (lets params &rest body)
	`(__lets ~(__pairize params) ~@body))
			
(def-macro (__lets bindings &rest body) 
	(if (nil? bindings) 
		`((fn () ~@body)) 
      `(let (~@(first bindings)) (__lets ~(rest bindings) ~@body))))

(def-macro (letfn params &rest body)
	`(__letr ~(__pairize params) ~body))

(def-macro (__letr bindings &rest body)
	`(__let ~(map->list (fn (x) (list (first (first x)) 'nil)) bindings)
		~@(concat! (map->list (fn (x) 
						(list '__set (first (first x)) 
								(list 'fn (rest (first x)) (second x))))
					bindings))
				~@body))

;set as a macro
(def (def-setter placefn setfn)
	(placefn.Setter setfn))

(def (setter placefn)
	(if (not (symbol? placefn))
		placefn
		(let (setfn placefn.Setter)
			(if setfn
				setfn
				placefn))))
			
#| gens nested blocks			
(def-macro (__set1 place value &rest args)
              `(block
                ~(if (not (cons? place))
                     `(__set ~place ~value)
                   (let (setfn (setter (first place)))
                     `(~setfn ~@(rest place) ~value)))
                ~@(if args `((__set1 ~@args)) nil)))

(def-macro (set &rest args)
    (if (nil? args)
        nil
      `(__set1 ~@args)))
|#

(def-macro (when arg &rest body)
   `(if ~arg (block ~@body)))

;better, suggested by MH

(def-macro (__set1 place value)
	(if (not (cons? place))
		`(__set ~place ~value)
		(let (setfn (setter (first place)))
			`(~setfn ~@(rest place) ~value))))

(def (__gen-pairwise-calls cmd lst)
	(when lst
		(cons (list cmd (first lst) (second lst))
			(__gen-pairwise-calls cmd (nth-rest 2 lst)))))

(def-macro (set &rest args)
	(when args
		`(block ~@(__gen-pairwise-calls '__set1 args))))

;similar to CL mapcan
(def (mapcat! f &rest lists)
	(apply concat! (apply map->list f lists)))
	
(def (member obj lst &key (test eql?))
	(cond
		(nil? lst) nil
		(test obj (first lst)) lst
		:else (member obj (rest lst) :test test)))

(def (member-if pred lst)
	(cond
		(nil? lst) nil
		(pred (first lst)) lst
		:else (member-if pred (rest lst))))
	
(def-macro (case arg &rest clauses)
	(let (g (gensym))
		`(let (~g ~arg)
			(cond ~@(mapcat! (fn (cl)
							(let (key (first cl))
								`(~(cond 
										(eql? key :else) :else
										(cons? key) `(member ~g '~key)
										:else `(eql? ~g '~key))
									~(second cl))))
							(__pairize clauses))))))

(def (__destructure params args bindings)
	(cond 
		(nil? params) bindings
		(atom? params) (cons `(~params ~args) bindings)
		(cons? params)
			(case (first params)
				(&rest)
					 (cons `(~(second params) ~args) bindings)
				:else
					(__destructure (first params) `(first ~args)
						(__destructure (rest params) `(rest ~args) bindings)))))

(def-macro (destructuring-bind params args &rest body)
	(let (gargs (gensym))
	`(let (~gargs ~args)
		(__let ~(__destructure params gargs nil) ~@body))))

;now redefine def-macro with destructuring
(def (_make_macro params body)
	(let (gargs (gensym))
		`(macro (&rest ~gargs)
			(destructuring-bind ~params ~gargs ~@body)))) 
		
(def (to-bool x)
	(if x true false))
	
(def (tree? lst)
	(to-bool (member-if cons? lst)))
	
(def def-macro (macro (spec &rest body)
	(if (member-if (fn (x) (member x '(&opt &key)))	spec)
		`(def ~(first spec) (macro ~(rest spec) ~@body))
		`(def ~(first spec) ~(_make_macro (rest spec) body)))))

(def-macro (when-not arg &rest body)
   `(if (not ~arg)
       (block ~@body)))

(def-macro (until test &rest body) 
	`(while (not ~test) 
		~@body))

(def-macro (for inits test update &rest body)
	`(lets ~inits
		(while ~test
			(block
				~@body
				~update))))

(def-macro (next! lst)
	`(set ~lst (rest ~lst)))
	
(def-macro (with-gensyms syms &rest body)
    `(__let ~(map->list (fn (s)
                     `(~s (gensym)))
             syms)
       ~@body))
	
(def-macro (dolist var lst &rest body)
	(let (g (gensym))
		`(for (~g ~lst) ~g (next! ~g)
			(let (~var (first ~g)) 
				~@body))))
				
(def-macro (dotails var lst &rest body)
	(let (g (gensym))
		`(for (~g ~lst) ~g (next! ~g)
			(let (~var ~g) 
				~@body))))


;(def-macro (dotimes var n &rest body)
;	(let (gn (gensym))
;		`(for (~gn ~n ~var 0) (< ~var ~gn) ((++ ~var))
;			~@body)))

(def (keyword str)
	(intern (+ ":" str)))
	
(def (__params-to-args params &opt (mode :base))
	(cond 
		(nil? params) nil
		(eqv? (first params) '&opt) (__params-to-args (rest params) :opt)
		(eqv? (first params) '&key) (__params-to-args (rest params) :key)
		(eqv? (first params) '&rest) nil
		:else (case mode
				:base (cons (first params) (__params-to-args (rest params) :base))
				:opt  (cons (if (cons? (first params)) 
								(first (first params))
								(first params))
						(__params-to-args (rest params) :opt))
				:key (cons (if (cons? (first params)) 
								(first (first params))
								(first params))
						(__params-to-args (rest params) :key)))))

(def (__rest-param params)
	(cond 
		(nil? params) nil
		(eqv? (first params) '&rest) (second params)
		:else (__rest-param (rest params))))

(def-macro (def-method (name (p1 dispatch-type-or-value) &rest params) &rest body)
	(when-not name.isDefined
		(name.setGlobalValue (GenericFunction.)))
	`(.AddMethod ~name ~dispatch-type-or-value 
			(fn ~(cons p1 params) 
				(let (call-base-method 
						(fn () 
							(apply (.FindBaseMethod ~name ~dispatch-type-or-value) 
								~p1 ~@(__params-to-args params) ~(__rest-param params))))	
						;((.FindBaseMethod ~name ~dispatch-type-or-value) ~p1 ~@params))	
					~@body))))

(def-macro (def-binop (name (p1 dispatch1) (p2 dispatch2)) &rest body)
	(when-not name.isDefined
		(name.setGlobalValue (BinOp.)))
	`(.AddMethod ~name ~dispatch1 ~dispatch2 
			(fn ~(list p1 p2) 
					~@body)))

;handled in primitives so available for boot
;(def-method (str (obj Object.)) 
;	obj.ToString)

;(def-method (str (obj nil)) 
;	"nil")

(def-method (str (obj true)) 
	"true")

(def-method (str (obj false)) 
	"false")

(def-method (str (obj String.)) 
	(String:Concat "\"" obj "\""))

	
(def *pr-writer Console:Out)
(def *pr-sep " ")

(def (pr &rest x)
	(while (and x (rest x))
		(*pr-writer.Write (str (first x)))
		(*pr-writer.Write *pr-sep)
		(next! x))
	(when x
		(*pr-writer.Write (str (first x)))))
	
(def (prn &rest x)
	(apply pr x)
	(*pr-writer.Write "\n"))

(def (prs &rest x)
	(while (and x (rest x))
		(*pr-writer.Write  (first x))
		(*pr-writer.Write *pr-sep)
		(next! x))
	(when x
		(*pr-writer.Write (first x))))
	
(def (prns &rest x)
	(apply prs x)
	(*pr-writer.Write "\n"))

(def-macro (+= x n)
	`(set ~x (add ~x ~n)))

(def one 1)

(def-macro (++ x)
	`(+= ~x one))

(def-macro (__accum op args)
	(with-gensyms (x result)
		`(let (~result (first ~args))
			(dolist ~x (rest ~args)
				(~op ~result ~x))
			~result)))
	
(def (+ &rest args)
	(__accum += args))
	
(def-macro (-= x n)
	`(set ~x (subtract ~x ~n)))

(def-macro (-- x)
	`(-= ~x 1))
	
(def (- &rest args)
	(__accum -= args))

(def-macro (*= x n)
	`(set ~x (multiply ~x ~n)))

(def (* &rest args)
	(__accum *= args))

(def-macro (/= x n)
	`(set ~x (divide ~x ~n)))

(def (/ &rest args)
	(__accum /= args))

(def-binop (add (x String.) (y Object.))
	(String:Concat x y.ToString))

(def-binop (add (x Object.) (y String.))
	(String:Concat x.ToString y))

(def-method (str (obj Type.))
	(+ obj.Name "."))
	
(def (now) DateTime:Now)

(def-macro (dotimes var n &rest body)
	(with-gensyms (gn)
		`(for (~var 0 ~gn ~n) (< ~var ~gn) (++ ~var)
			~@body)))

(def (zero? x)
	(== x 0))
(def (positive? x)
	(> x 0))
(def (negative? x)
	(< x 0))
	
(def (__min x y)
	(if (< x y) x y))

(def (min &rest args)
	(let (result (first args))
		(for (args (rest args)) args (next! args)
			(set result (__min result (first args))))
		result))

(def (__max x y)
	(if (> x  y) x y))

(def (max &rest args)
	(let (result (first args))
		(while (rest args)
			(set args (rest args))
			(set result (__max result (first args))))
		result))

(def-method (str-delim (x Cons.) &key (start true))
	(if start "(" ")"))

(def-method (str-delim (x Array.) &key (start true))
	(if start "[" "]"))

(def-method (str-delim (x IEnumerable.) &key (start true))
	(if start 
		(+ "{" (str (type-of x)) " ")
		"}"))

(def-method (str-delim (x IEnumerator.) &key (start true))
	(if start 
		(+ "{" (str (type-of x)) " ")
		"}"))

(set *str-limit 20)

(def (__strseq seq)
	(let (	s (StringBuilder. (str-delim seq))
			limit *str-limit)
		(let (c 0 e (get-enum seq))
			(while (and (< c limit) (.IEnumerator:MoveNext e))
				(s.Append (str (.IEnumerator:Current e)))
				(s.Append " ")
				(++ c)
				(when (and (== c limit) e.MoveNext)
					(s.Append "... ")))
			;trim the trailing space
			(when (> c 0)
				(s.Remove (- s.Length 1) 1)))
		(s.Append (str-delim seq :start false))))

;these might be a bad idea now with lazy enumeration
;(def-method (str (seq IEnumerable.))
;	(__strseq seq))

;(def-method (str (seq IEnumerator.))
;	(__strseq seq))

(def-method (str (a Array.))
	(__strseq a))

(def-method (str (a Cons.))
	(__strseq a))
	
(def-method (str (a ICollection.))
	(__strseq a))
		
#|	
(def-method (str (obj ICollection.))
	(let (	s (StringBuilder.) 
			limit 20 
			isarray (is? obj Array.))
		(s.Append (if isarray "[" (+ "{" (str (type-of obj)) " ")))
		(for (c 0 e (.IEnumerable:GetEnumerator obj)) 
				(< c (min limit (.ICollection:Count obj))) 
				(++ c) 
			e.MoveNext
			(s.Append (str e.Current))
			(when (< c (- (min limit (.ICollection:Count obj)) 1))
				(s.Append " ")))
		(when (>= (.ICollection:Count obj) limit)
			(s.Append " ..."))
		(.ToString (s.Append (if isarray "]" "}")))))
|#

(def-method (str (obj DictionaryEntry.))
	(+ "{" (str obj.Key) " " (str obj.Value) "}"))

(def-binop (subtract (d1 DateTime.) (d2 DateTime.))
	(d1.Subtract d2))
	
(def-macro (time &rest body)
	(with-gensyms (start result)
		`(let (~start (now)) 
			(set ~result 
				(block ~@body))
			(prn (- (now) ~start))
			~result)))

(def (__memberize sym)
	(intern (+ "." sym)))

(def (__typeize s)
	(intern (+ s.ToString ".")))

;strip the trailing . and return as string
(def (__untypeize sym)
	(let (s sym.ToString)
		(s.Substring 0 (- s.Length 1))))
				
(def (make-record type &rest args)
	(let (this (type))
		(apply init-rec this args)))
		
(def-macro (def-record type &rest fields)
  (let (typesym (if (cons? type) 
						(first type)
						type))
	(with-gensyms (this newtype)
		`(let (~newtype	(Record:CreateRecordType	
									;~(__untypeize typesym)
									~(.ToString typesym)
									~(if (cons? type)
										(second type)
										'Record.)))
			(interpreter.InternType ~newtype)
			;(def-method (init-rec (~this ~typesym) &key ~@fields)
			(def-method (init-rec (~this ~newtype) &key ~@fields)
				(call-base-method)
				~@(map->list (fn (x) (if (cons? x)
									`(~(__memberize (first x)) ~this ~(first x))
									`(~(__memberize x) ~this ~x)))
							fields)
					~this)
			~newtype))))

(set _ Missing:Value)

(def (missing? x)
	(eql? x _))
	
(def-method (str (m Missing.)) "_")

(def-setter 'first 'set-first)
(def-setter 'rest 'set-rest)

(def-macro (parallel-set &rest args)
	(when-not (even? (len args)) 
      (error "odd number of arguments"))
	(lets (pairs (__pairize args)
         syms (map->list (fn (x) (gensym)) 
                 pairs))
    `(let ~(mapcat! list
             syms
             (map->list second pairs))
       (set ~@(mapcat! list
                (map->list first pairs)
                syms)))))

(def-macro (push! obj place)
    `(set ~place (cons ~obj ~place)))

(def-macro (pop! place)
    `(set ~place (rest ~place)))

(def-macro (rotate-set &rest args)
    `(parallel-set ~@(mapcat! list
               args
               (append (rest args) 
                       (list (first args))))))

;todo reverse and reverse! need to be gfuncs
(def (reverse! lst)
     (let (prev nil)
       (while (cons? lst)
         (parallel-set (rest lst) prev
               prev      lst
               lst       (rest lst)))
		prev))

(def (butlast lst &opt (n 1))
     (reverse! (nth-rest n (reverse lst))))

(def (last lst &opt (n 1))
	(let (l (len lst))
		(if	(<= l n) 
			lst
			(nth-rest (- l n) lst))))
		
(def-macro (shift-set &rest args)
    (let (places (butlast args))
      `(parallel-set ~@(mapcat! list
                 places
                 (append (rest places) 
                         (last args))))))

(def-macro (for-each var coll &rest body)
	(let (enum (gensym))
		`(let (~enum (get-enum ~coll))
			(while (.IEnumerator:MoveNext ~enum)
				(let (~var (.IEnumerator:Current ~enum))
					~@body)))))

(def-macro (make-enum inits get &rest move)
	(with-gensyms (movefn getfn)
		`(lets ~inits
			(let (	~getfn (fn () ~get)
					~movefn (fn () ~@move))
				(FnEnumerator. ~getfn ~movefn)))))
		
(def (range start end &opt (step 1))
	(make-enum
		(x start curr start)
		curr
		(set curr x)
		(+= x step)
		(< curr end)))

(def (map1 key seq)
	(make-enum
		(e (get-enum seq))
		(key (.IEnumerator:Current e))
		(.IEnumerator:MoveNext e)))

(def (filter pred seq)
	(make-enum
		(e (get-enum seq))
		(.IEnumerator:Current e)
		(let (seeking true)
			(while (and seeking (.IEnumerator:MoveNext e))
				(when (pred (.IEnumerator:Current e))
					(set seeking false)))
			(not seeking))))

(def (find val seq &key (test eqv?))
	(filter (fn (x)
				(test x val))
			seq)) 
				
(def (concat &rest seqs)
	(make-enum
		(next-e (fn () (get-enum (if (nil? seqs)
									nil 
									(first seqs))))
		 e  (next-e))
		(.IEnumerator:Current e)
		(if (.IEnumerator:MoveNext e)
			true
			(block 
				(while (and (next! seqs) 
							(set e (next-e)) 
							(not (.IEnumerator:MoveNext e))))
					;no body
				(not (nil? seqs))))))

(def-method (into (coll IList.) seq)
	(for-each x seq
		(coll.Add x))
	coll)

(def-method (into (coll Cons.) seq)
	(let (tail (last coll))
		(for-each x seq
			(block
				(set (rest tail) (Cons. x))
				(set tail (rest tail))))
		coll))

(def-method (into (coll nil) seq)
	(let (tail nil)
		(for-each x seq
			(if (nil? tail)
				(set coll (set tail (Cons. x)))
				(block
					(set (rest tail) (Cons. x))
					(set tail (rest tail)))))
		coll))

(def (reduce f seq &key init)
	(lets (	e (get-enum seq)
			has-items (.IEnumerator:MoveNext e)
			result (cond 
						(not has-items) (if (missing? init) (f) init)
						(missing? init) (.IEnumerator:Current e)
						:else (f init (.IEnumerator:Current e))))
		(when has-items
			(while (.IEnumerator:MoveNext e)
				(set result (f result (.IEnumerator:Current e)))))
		result))
	
(def (map f &rest seqs)
	(if (== 1 (len seqs))
		(map1 f (first seqs))
		(make-enum
			(es (into () (map1 get-enum seqs)))
			(apply f (map1 .IEnumerator:Current es))
			(let (ret true)
				(for-each e es
					(when-not (.IEnumerator:MoveNext e)
						(set ret false)))
				ret))))

(def (any pred &rest seqs)
	(let (m (apply map pred seqs) found false)
		(while (and (not found) m.MoveNext)
			(set found m.Current))
		found))

(def (every pred &rest seqs)
	(let (m (apply map pred seqs) found true)
		(while (and found m.MoveNext)
			(set found m.Current))
		found))
	
;todo clarify binding of values vs. expressions
(def (bind1 func val)
	(fn (&rest args)
		(apply func val args)))

(def-macro (bind func &rest pattern)
	(lets (	args (map->list (fn (x) (if (eql? x '_)
								(gensym)
								x)) 
					pattern)
			params (mapcat! (fn (patt arg) 
								(if (eql? patt '_)
									(list arg)
									()))
						pattern args))
		`(fn ~params (~func ~@args))))
		 
(def-macro (bind* func &rest pattern)
	(lets (	args (append (	map->list (fn (x) 
									(if (eql? x '_)
										(gensym)
										x)) 
								pattern)
							(list (gensym)))
			params (append (mapcat! (fn (patt arg) 
								(if (eql? patt '_)
									(list arg)
									()))
								pattern args)
							(cons '&rest (last args))))
		`(fn ~params (apply ~func ~@args))))

(def (compose &rest fns)
	(destructuring-bind (f1 &rest flist) (reverse fns)
		(fn (&rest args)
			(let (result (apply f1 args))
				(dolist f flist
					(set result (f result)))
				result))))

;n.b. by default catch binds exception to 'ex
(def-macro (try body &key catch finally (catch-name 'ex))
	`(try-catch-finally
		(fn () ~body)
		~(if (missing? catch)
			nil
			`(fn (~catch-name) ~catch))
		~(if (missing? finally)
			nil 
			`(fn () ~finally))))

(def-macro (with-dispose inits &rest body)
	(let (disposal (map->list 
						(fn (x) `(when ~(first x) (.IDisposable:Dispose ~(first x))))
					(__pairize inits)))
		`(lets ~inits
			(try 
				(block ~@body)
				:finally
					(block ~@disposal)))))
				
(def-macro (trace &rest fnames)
	(when (cons? fnames)
		(for-each fname fnames
			(interpreter.Trace fname)))
	'interpreter.TraceList)

(def-macro (untrace &rest fnames)
	(if (nil? fnames)
		interpreter.UnTraceAll
		(for-each fname fnames
			(interpreter.UnTrace fname)))
	'interpreter.TraceList)

(def (load-assembly name)
	(interpreter.InternTypesFrom (Assembly:LoadWithPartialName name)))

(def (load-assembly-from filename)
	(interpreter.InternTypesFrom (Assembly:LoadFrom filename)))
	
(def-macro (get-or-add dictionary key expr)
	(let (g (gensym))
		`(let (~g (~key ~dictionary))
			(when (nil? ~g)
				(set ~g ~expr)
				(.Add ~dictionary ~key ~g))
			~g)))

;delegators are objects which implement an Invoke matching the signature
;of some delegate type
;they are constructed with an IFunction, and implement Invoke by calling it

;a map of delegate type -> delegator type
(set __delegator-types (Hashtable.))

(set __delegator-assembly 
	(let (assembly-name (AssemblyName.))
		(set assembly-name.Name "DelegatorAssembly")
		(AppDomain:CurrentDomain.DefineDynamicAssembly assembly-name 
														AssemblyBuilderAccess:Run)))
														
(set __delegator-module (__delegator-assembly.DefineDynamicModule "DelegatorModule"))

;make a type with a ctor taking an IFunction, 
;with an Invoke function matching the delegate type's, 
;implementing the delegate Invoke on the IFunction's Invoke
(def (__make-delegator-type type)
	(lets (	invoke-sig (type.GetMethod "Invoke")
			invoke-arg-types (apply vector-of Type. 
								(map .ParameterType invoke-sig.GetParameters)) 
			tb (__delegator-module.DefineType (+ type.Name "Delegator")
					(bit-or TypeAttributes:Class TypeAttributes:Public) Object.)
			fn-field (tb.DefineField "fn" IFunction. FieldAttributes:Private)
			ctor-arg-types [IFunction.]
			cb (tb.DefineConstructor MethodAttributes:Public
								CallingConventions:Standard
								ctor-arg-types)
			mb (tb.DefineMethod "Invoke" MethodAttributes:Public 
					invoke-sig.ReturnType
					invoke-arg-types))
		(let (cil cb.GetILGenerator)
			(cil.Emit OpCodes:Ldarg_0)
			(cil.Emit OpCodes:Call (.GetConstructor Object. Type:EmptyTypes))
			(cil.Emit OpCodes:Ldarg_0)
			(cil.Emit OpCodes:Ldarg_1)
			(cil.Emit OpCodes:Stfld fn-field)
			(cil.Emit OpCodes:Ret))
		(let (mil mb.GetILGenerator)
			(mil.DeclareLocal (type-of []))
			;create an Object array the size of numargs
			(mil.Emit OpCodes:Ldc_I4 invoke-arg-types.Length)
			(mil.Emit OpCodes:Newarr Object.)
			(mil.Emit OpCodes:Stloc_0)
			;turn the args into objects and place in array
			(for (i 0) (< i invoke-arg-types.Length) (++ i)
				(mil.Emit OpCodes:Ldloc_0)
				(mil.Emit OpCodes:Ldc_I4 i)
				(mil.Emit OpCodes:Ldarg (+ i 1))
				(when (.IsValueType (i invoke-arg-types))
					(mil.Emit OpCodes:Box))
				(mil.Emit OpCodes:Stelem_Ref))
			;call Invoke on fn member
			(mil.Emit OpCodes:Ldarg_0)
			(mil.Emit OpCodes:Ldfld fn-field)
			(mil.Emit OpCodes:Ldloc_0)
			(mil.Emit OpCodes:Callvirt (.GetMethod IFunction. "Invoke"))
			;above will leave an Object on the stack
			(cond	(eql? invoke-sig.ReturnType Void.)
						(block
							(mil.Emit OpCodes:Pop)
							(mil.Emit OpCodes:Ret))
					(.IsValueType invoke-sig.ReturnType)
						(block
							(mil.Emit OpCodes:Unbox invoke-sig.ReturnType)
							(mil.Emit OpCodes:Ldobj invoke-sig.ReturnType)
							(mil.Emit OpCodes:Ret))
					:else
						(block
							(mil.Emit OpCodes:Castclass invoke-sig.ReturnType)
							(mil.Emit OpCodes:Ret)))
		tb.CreateType)))
			
	
;get the delegator type for the delegate type if in cache, else create and cache one
(def (__make-delegator type f)
	(let (delegator-type (get-or-add __delegator-types type.FullName
							(__make-delegator-type type)))
		(delegator-type f)))

;make an instance of the delegate bound to the closure	
(def-macro (make-delegate type (&rest args) &rest body)
	`(let (f (fn ~args ~@body))
		(Delegate:CreateDelegate 
			~type 
			(__make-delegator ~type f)
			"Invoke")))
