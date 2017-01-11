;; some useful (or at least amusing) functions and macros

(provide 'stuff.scm)

(when (provided? 'pure-s7)
  (define (let->list e) (reverse! (map values e))))
  

;;; ----------------
(define empty? 
  (let ((documentation "(empty? obj) returns #t if obj is an empty sequence"))
    (lambda (obj) 
      (and (not (pair? obj))
	   (if (hash-table? obj)
	       (zero? (hash-table-entries obj)) ; length here is table size
	       (eqv? (length obj) 0))))))

(define applicable? arity)

;(define sequence? length) -- now built-in for procedure-signatures

(define indexable? 
  (let ((documentation "(indexable? obj) returns #t if obj can be applied to an index: (obj 0)"))
    (lambda (obj)
      (and (sequence? obj)
	   (applicable? obj)))))

(define (ow!)
  (call-with-output-string
   (lambda (p)
     (let ((ow (owlet))
	   (elist (list (rootlet))))
       
       ;; show current error data
       (format p "error: ~A" (ow 'error-type))
       (let ((info (ow 'error-data)))
	 (if (and (pair? info)
		  (string? (car info)))
	     (format p ": ~A" (apply format #f info))
	     (if (not (null? info))
		 (format p ": ~A" info))))

       (format p "~%error-code: ~S~%" (ow 'error-code))
       (when (ow 'error-line)
	 (format p "~%error-file/line: ~S[~A]~%" (ow 'error-file) (ow 'error-line)))
	   
       ;; show history, if available
       (when (pair? (ow 'error-history)) ; a circular list, starts at error-code, entries stored backwards
	 (let ((history ())
	       (lines ())
	       (files ())
	       (start (ow 'error-history)))
	   (do ((x (cdr start) (cdr x))
		(i 0 (+ i 1)))
	       ((or (eq? x start)
		    (null? (car x))
		    (= i (*s7* 'history-size)))
		(format p "~%error-history:~%    ~S" (car start))
		(do ((x history (cdr x))
		     (line lines (cdr line))
		     (f files (cdr f)))
		    ((null? x))
		  (format p (if (and (integer? (car line))
				     (string? (car f))
				     (not (string=? (car f) "*stdout*")))
				(values "~%    ~S~40T;~A[~A]" (car x) (car f) (car line))
				(values "~%    ~S" (car x)))))
		(format p "~%"))
	     (set! history (cons (car x) history))
	     (set! lines (cons (pair-line-number (car x)) lines))
	     (set! files (cons (pair-filename (car x)) files)))))
       
       ;; show the enclosing contexts
       (let ((old-print-length (*s7* 'print-length)))
	 (set! (*s7* 'print-length) 8)
	 (do ((e (outlet ow) (outlet e))) 
	     ((memq e elist)
	      (set! (*s7* 'print-length) old-print-length))
	   (if (> (length e) 0)
	       (format p "~%~{~A~| ~}~%" e))
	   (set! elist (cons e elist))))))))

#|
(set! (hook-functions *error-hook*) 
      (list (lambda (hook) 
	      (apply format *stderr* (hook 'data))
	      (newline *stderr*)
	      (when ((owlet) 'error-line)
		(format *stderr* "~S line ~A~%" ((owlet) 'error-file) ((owlet) 'error-line)))
	      (do ((e (outlet (owlet)) (outlet e))) 
		  ((eq? e (rootlet))) 
		(format *stderr* "~{  ~A~%~}~%" e))
	      (format *stderr* "~%~A~%" (stacktrace)))))
|#


;;; ----------------
(define (first obj)  (if (sequence? obj) (obj 0) (error 'wrong-type-arg "first argument, ~S, is not a sequence" obj)))
(define (second obj) (if (sequence? obj) (obj 1) (error 'wrong-type-arg "second argument, ~S, is not a sequence" obj)))
(define (third obj)  (if (sequence? obj) (obj 2) (error 'wrong-type-arg "third argument, ~S, is not a sequence" obj)))
(define (fourth obj) (if (sequence? obj) (obj 3) (error 'wrong-type-arg "fourth argument, ~S, is not a sequence" obj)))
(define (fifth obj)  (if (sequence? obj) (obj 4) (error 'wrong-type-arg "fifth argument, ~S, is not a sequence" obj)))
(define (sixth obj)  (if (sequence? obj) (obj 5) (error 'wrong-type-arg "sixth argument, ~S, is not a sequence" obj)))
(define (seventh obj)(if (sequence? obj) (obj 6) (error 'wrong-type-arg "seventh argument, ~S, is not a sequence" obj)))
(define (eighth obj) (if (sequence? obj) (obj 7) (error 'wrong-type-arg "eighthment, ~S, is not a sequence" obj)))
(define (ninth obj)  (if (sequence? obj) (obj 8) (error 'wrong-type-arg "ninth argument, ~S, is not a sequence" obj)))
(define (tenth obj)  (if (sequence? obj) (obj 9) (error 'wrong-type-arg "tenth argument, ~S, is not a sequence" obj)))


(define iota 
  (let ((documentation "(iota n (start 0) (incr 1)) returns a list counting from start for n:\n\
    (iota 3) -> '(0 1 2)"))
    (lambda* (n (start 0) (incr 1))
      (if (or (not (integer? n))
	      (< n 0))
	  (error 'wrong-type-arg "iota length ~A should be a non-negative integer" n))
      (let ((lst (make-list n)))
	(do ((p lst (cdr p))
	     (i start (+ i incr)))
	    ((null? p) lst)
	  (set! (car p) i))))))

(define (cdr* lst n)
  (do ((i n (- i 1))
       (result lst (cdr result))) 
      ((or (null? result)
	   (zero? i)) 
       result)))

(define make-circular-list 
  (let ((documentation "(make-circular-list n init) returns a circular list with n entries initialized to init:\n\
    (make-circular-list 3 #f) -> #1=(#f #f #f . #1#)"))
    (lambda* (n init)
      (let ((lst (make-list n init)))
	(set-cdr! (list-tail lst (- n 1)) lst)))))

(define circular-list 
  (let ((documentation "(circular-list . objs) returns a circular list with objs:\n\
    (circular-list 1 2) -> #1=(1 2 . #1#)"))
    (lambda objs
      (let ((lst (copy objs)))
	(set-cdr! (list-tail lst (- (length lst) 1)) lst)))))

(define circular-list? 
  (let ((documentation "(circular-list? obj) returns #t if obj is a circular list"))
    (lambda (obj)
      (catch #t
	(lambda () (infinite? (length obj)))
	(lambda args #f)))))

(define linearize 
  (let ((documentation " (linearize lst) turns a circular list into normal list:\n\
    (linearize (circular-list 1 2)) -> '(1 2)"))
    (lambda (lst) 
      (let lin-1 ((lst lst)
                  (result ())
                  (sofar ()))
        (if (or (not (pair? lst)) (memq lst sofar))
            (reverse! result)
            (lin-1 (cdr lst) (cons (car lst) result) (cons lst sofar)))))))

(define cyclic? 
  (let ((documentation "(cyclic obj) returns #t if the sequence obj contains any cycles"))
    (lambda (obj)
      (pair? (cyclic-sequences obj)))))


(define copy-tree 
  (let ((documentation "(copy-tree lst) returns a full copy of lst"))
    (lambda (lis)
      (if (pair? lis)
	  (cons (copy-tree (car lis))
		(copy-tree (cdr lis)))
	  lis))))

(define tree-member 
  (let ((documentation "(tree-member sym tree) returns #t if sym is found anywhere in tree:\n\
    (tree-member 'a '(1 (2 a))) -> #t"))
    (lambda (sym tree)
      (and (pair? tree)
	   (or (eq? (car tree) sym)
	       (and (pair? (car tree))
		    (tree-member sym (car tree)))
	       (tree-member sym (cdr tree)))))))

(define adjoin 
  (let ((documentation "(adjoin obj lst) adds obj to lst if it is not already in lst, returning the new list"))
    (lambda (obj lst)
      (if (member obj lst) lst (cons obj lst)))))

(define (cdr-assoc obj lst)
  (cond ((assoc obj lst) => cdr)
	(else #f)))


;;; this used to be built into s7.c, but no one uses it.
(define-macro (multiple-value-set! vars expr . body)
  (if (pair? vars)
      (let ((local-vars (map (lambda (n) (gensym)) vars)))
	`((lambda ,local-vars
	    ,@(map (lambda (n ln) `(set! ,n ,ln)) vars local-vars)
	    ,@body)
	  ,expr))
      (if (and (null? vars) (null? expr))
	  (cons 'begin body)
	  (error 'syntax-error "multiple-value-set! vars/exprs messed up"))))



;;; ----------------
(define-macro (fully-macroexpand form)
  (list 'quote 
	(let expand ((form form))
	  (cond ((not (pair? form)) form)
		((and (symbol? (car form))
		      (macro? (symbol->value (car form))))
		 (expand (apply macroexpand (list form))))
		((and (eq? (car form) 'set!)    ; look for (set! (mac ...) ...) and use mac's procedure-setter
		      (pair? (cdr form))
		      (pair? (cadr form))
		      (macro? (symbol->value (caadr form))))
		 (expand (apply (eval (procedure-source (procedure-setter (symbol->value (caadr form)))))
				(append (cdadr form) (cddr form)))))
		(else (cons (expand (car form)) 
			    (expand (cdr form))))))))

(define-macro (define-with-macros name&args . body)
  `(apply define ',name&args (list (fully-macroexpand `(begin ,,@body)))))

(define setf
  (let ((args (gensym))
	(name (gensym)))
    (apply define-bacro `((,name . ,args)        
			  (unless (null? ,args)
			    (apply set! (car ,args) (cadr ,args) ())
			    (apply setf (cddr ,args)))))))


(define-macro* (incf sym (inc 1))
  `(set! ,sym (+ ,sym ,inc))) ; or (list-values set! sym (list-values + sym inc))

;; (define-bacro* (incf-1 sym (inc 1)) (apply set! sym (list + sym inc) ()))

(define-macro* (decf sym (dec 1))
  `(set! ,sym (- ,sym ,dec)))

(define-macro (shiftf . places)
  (let ((tmp (gensym)))
    `(let ((,tmp ,(car places)))
       ,@(map (lambda (a b)
		`(set! ,a ,b))
	      places
	      (cdr places))
       ,tmp)))

(define-macro (rotatef . places)
  (let ((tmp (gensym))
	(last (places (- (length places) 1))))
    `(let ((,tmp ,(car places)))
       ,@(map (lambda (a b)
		`(set! ,a ,b))
	      places
	      (cdr places))
       (set! ,last ,tmp))))

(define-macro (progv vars vals . body)
  `(apply (apply lambda ,vars ',body) ,vals))

(define-macro (symbol-set! var val) ; like CL's set
  `(apply set! ,var ',val ()))

(define-macro (value->symbol expr)
  `(let ((val ,expr)
	 (e1 (curlet)))
     (call-with-exit
      (lambda (return)
	(do ((e e1 (outlet e))) ()
	  (for-each 
	   (lambda (slot)
	     (if (equal? val (cdr slot))
		 (return (car slot))))
	   e)
	  (if (eq? e (rootlet))
	      (return #f)))))))

(define-macro (enum . args)
  `(for-each define ',args (iota (length ',args))))

(define-macro (destructuring-bind lst expr . body) ; if only there were some use for this!
  (cons 'let 
	(cons (let flatten ((lst1 lst)
			    (lst2 (eval expr))
			    (args ()))
		(cond ((null? lst1) args)
		      ((not (pair? lst1)) (cons (list lst1 lst2) args))
		      (else (flatten (car lst1) (car lst2) 
				     (flatten (cdr lst1) (cdr lst2) args)))))
	      body)))

#|
;; kinda ugly!
(define-macro (and-map func args)
  `(let and-map-loop ((args ,args))
     (or (null? args)
	 (and (eval (list ,func (car args)))
	      (and-map-loop (cdr args))))))
|#

(define-macro (and-let* vars . body)      ; bind vars, if any is #f stop, else evaluate body with those bindings
  `(let () (and ,@(map (lambda (v) `(define ,@v)) vars) (begin ,@body))))

(define-macro (let*-temporarily vars . body)
  `(with-let (#_inlet :orig (#_curlet) 
		      :saved (#_list ,@(map car vars)))
     (dynamic-wind
	 (lambda () #f)
	 (lambda ()
	   (with-let orig
	     ,@(map (lambda (v)
		      `(set! ,(car v) ,(cadr v)))
		    vars)
	     ,@body))
	 (lambda ()
	   ,@(map (let ((ctr -1))
		    (lambda (v)
		      (if (symbol? (car v))
			  `(set! (orig ',(car v)) (list-ref saved ,(set! ctr (+ ctr 1))))
			  `(set! (with-let orig ,(car v)) (list-ref saved ,(set! ctr (+ ctr 1)))))))
		  vars)))))

;;; 14-8-16: moved let-temporarily to s7.c

(define-macro (while test . body)      ; while loop with predefined break and continue
  `(call-with-exit
    (lambda (break) 
      (let continue ()
	(if (let () ,test)
	    (begin 
	      (let () ,@body)
	      (continue))
	    (break))))))

(define-macro (do* spec end . body)
  `(let* ,(map (lambda (var) 
		 (list (car var) (cadr var))) 
	       spec)
     (do () ,end
       ,@body
       ,@(map (lambda (var) 
		(if (pair? (cddr var))
		    `(set! ,(car var) ,(caddr var))
		    (values)))
	      spec))))

(define-macro (string-case selector . clauses)
  (cons 'case       ; case with string constant keys
	(cons (list 'symbol selector)
	      (map (lambda (clause)
		     (if (pair? (car clause))
			 (cons (map symbol (car clause)) (cdr clause))
			 clause))
		   clauses))))

(define-macro (eval-case key . clauses) ; case with evaluated key-lists
  (let ((select (gensym)))
    `(let ((,select ,key))
       (cond ,@(map (lambda (lst)
		      (if (not (pair? (car lst)))
			  lst
			  (cons `(member ,select (list ,@(car lst)))
				(cdr lst))))
		    clauses)))))



;;; ---------------- 
(define hash-table->alist 
  (let ((documentation "(hash-table->alist table) returns the contents of table as an association list:\n\
    (hash-table->alist (hash-table '(a . 1))) -> '((a . 1))"))
    (lambda (table)
      (if (hash-table? table)
	  (map values table)
	  (error 'wrong-type-arg "hash-table->alist argument, ~A, is not a hash-table" table)))))

(define merge-hash-tables 
  (let ((documentation "(merge-hash-tables . tables) returns a new hash-table with the contents of all the tables"))
    (lambda tables
      (apply hash-table 
	     (apply append 
		    (map hash-table->alist tables))))))



;;; ----------------
(define-macro (c?r path)
  (define (X-marks-the-spot accessor tree)
    (if (pair? tree)
	(or (X-marks-the-spot (cons 'car accessor) (car tree))
	    (X-marks-the-spot (cons 'cdr accessor) (cdr tree)))
	(and (eq? tree 'X) accessor)))
  (let ((body 'lst))
    (for-each
     (lambda (f)
       (set! body (list f body)))
     (reverse (X-marks-the-spot () path)))
    `(dilambda
      (lambda (lst) 
	,body)
      (lambda (lst val)
	(set! ,body val)))))



;;; ----------------
(define find-if 
  (let ((documentation "(find-if func sequence) applies func to each member of sequence.\n\
If func approves of one, find-if returns that member of the sequence"))
    (lambda (f sequence)
      (call-with-exit
       (lambda (return)
	 (for-each (lambda (arg)
		     (if (f arg)
			 (return arg)))
		   sequence)
	 #f)))))

(define member? 
  (let ((documentation "(member? obj sequence) returns #t if obj is an element of sequence"))
    (lambda (obj sequence)
      (find-if (lambda (x) (equal? x obj)) sequence))))


(define index-if 
  (let ((documentation "(index-if func sequence) applies func to each member of sequence.\n\
If func approves of one, index-if returns the index that gives that element's position.\n\
    (index-if (lambda (x) (= x 32)) #(0 1 32 4)) -> 2\n\
    (index-if (lambda (x) (= (cdr x) 32)) (hash-table '(a . 1) '(b . 32))) -> 'b"))
    (lambda (f sequence)
      (call-with-exit
       (lambda (return) 
	 (if (or (hash-table? sequence)
		 (let? sequence))
	     (for-each (lambda (arg)
			 (if (f arg) (return (car arg))))
		       sequence)
	     (let ((position 0))
	       (for-each (lambda (arg)
			   (if (f arg) (return position))
			   (set! position (+ position 1)))
			 sequence)))
	 #f)))))

(define count-if 
  (let ((documentation "(count-if func sequence) applies func to each member of sequence, returning the number of times func approves."))
    (lambda (f sequence)
      (let ((count 0))
	(for-each (lambda (arg)
		    (if (f arg)
			(set! count (+ count 1))))
		  sequence)
	count))))

(define every? 
  (let ((documentation "(every? func sequence) returns #t if func approves of every member of sequence"))
    (lambda (f sequence)
      (call-with-exit
       (lambda (return) 
	 (for-each (lambda (arg) (if (not (f arg)) (return #f))) sequence)
	 #t)))))

(define any? 
  (let ((documentation "(any? func sequence) returns #t if func approves of any member of sequence"))
    (lambda (f sequence)
      (call-with-exit
       (lambda (return) 
	 (for-each (lambda (arg) (if (f arg) (return #t))) sequence)
	 #f)))))

(define collect-if 
  (let ((documentation "(collect-if type func sequence) gathers the elements of sequence that satisfy func, and returns them via type:\n\
    (collect-if list integer? #(1.4 2/3 1 1+i 2)) -> '(1 2)"))
    (lambda (type f sequence)
      (apply type (map (lambda (arg) (if (f arg) arg (values))) sequence)))))

;;; if type=list, this is slightly wasteful because list currently copies its args, so:
;;;   ((if (eq? type list) values (values apply type)) ...) would work
;;;
;;; to return (f arg) rather than arg, (apply type (map f sequence))

(define remove-if 
  (let ((documentation "(remove-if type f sequence) returns via type the elements of sequence that do not satisfy func:\n\
    (remove-if list integer? #(1.4 2/3 1 1+i 2)) -> '(1.4 2/3 1+1i)"))
    (lambda (type f sequence)
      (collect-if type (lambda (obj) (not (f obj))) sequence))))

(define nonce 
  (let ((documentation "(nonce type sequence) returns via type the elements of sequence that occur only once"))
    (lambda (type sequence) 
      (collect-if type (lambda (obj) (= (count-if (lambda (x) (equal? x obj)) sequence) 1)) sequence))))


(define full-find-if 
  (let ((documentation "(full-find-if func sequence) searches sequence, and recursively any sequences it contains, for an element that satisfies func"))
    (lambda (f sequence)
      (if (and (procedure? f)
	       (aritable? f 1))
	  (if (sequence? sequence)
	      (call-with-exit
	       (lambda (return)
		 (let full-find-if-1 ((seq sequence))
		   (for-each
		    (lambda (x)
		      (if (f x)
			  (return x)
			  (if (sequence? x) (full-find-if-1 x))))
		    seq))
		 #f))
	      (error 'wrong-type-arg "full-find-if second argument, ~A, is not a sequence" sequence))
	  (error 'wrong-type-arg "full-find-if first argument, ~A, is not a procedure of one argument" f)))))

(define full-count-if 
  (let ((documentation "(full-count-if func sequence) searches sequence, and recursively any sequences it contains, returning the number of elements that satisfy func"))
    (lambda (f sequence)
      (let ((count 0))
	(full-find-if (lambda (x) (if (f x) (set! count (+ count 1))) #f) sequence)
	count))))

(define full-index-if 
  (let ((documentation "(full-index-if func sequence) searches sequence, and recursively any sequences it contains, returning the indices of the first element that satisfies func:\n\
    (full-index-if (lambda (x) (and (integer? x) (= x 3))) '(1 (2 3))) -> '(1 1)"))
    (lambda (f sequence)
      (call-with-exit
       (lambda (return)
	 (letrec ((full-index-if-1 
		   (lambda (f seq path)
		     (if (or (hash-table? seq)
			     (let? seq))
			 (for-each (lambda (arg)
				     (if (f arg)
					 (return (reverse (cons (car arg) path)))
					 (if (indexable? (cdr arg))
					     (full-index-if-1 f (cdr arg) (cons (car arg) path)))))
				   seq)
			 (let ((position 0))
			   (for-each (lambda (arg)
				       (if (f arg)
					   (return (reverse (cons position path)))
					   (if (indexable? arg)
					       (full-index-if-1 f arg (cons position path))))
				       (set! position (+ position 1)))
				     seq))))))
	   (full-index-if-1 f sequence ())
	   #f))))))


(define (make-complete-iterator obj)
  (let ((iters ())
	(cycles (cyclic-sequences obj))
	(seen-cycles ()))
    
    (define (make-careful-iterator p)
      (if (not (pair? p))
	  (make-iterator p)
	  (let ((len (length p)))
	    (make-iterator
	     (cond ((infinite? len)      ; circular list
		    (let ((cur p)
			  (iterator? #t))
		      (lambda ()
			(if (memq cur seen-cycles)
			    #<eof>
			    (let ((result (car cur)))
			      (if (memq cur cycles) 
				  (set! seen-cycles (cons cur seen-cycles)))
			      (set! cur (cdr cur))
			      result)))))
		   ((positive? len)      ; normal list
		    p)
		   (else 
		    (let ((cur p)        ; dotted list
			  (iterator? #t))
		      (lambda ()
			(if (pair? cur)
			    (let ((result (car cur)))
			      (set! cur (cdr cur))
			      result)
			    (let ((result cur))
			      (set! cur #<eof>)
			      result))))))))))
    
    (make-iterator
     (let ((iter (make-careful-iterator obj))
	   (iterator? #t))       
       (define (iterloop) ; define returns the new value
	 (define (iter-memq p q)
	   (and (pair? q)
		(or (eq? p (iterator-sequence (car q)))
		    (iter-memq p (cdr q)))))
	 (let ((result (iter)))
	   (cond ((length result)
		  (if (or (memq result seen-cycles)             ; we've dealt with it already, so skip it
			  (eq? result (iterator-sequence iter))
			  (iter-memq result iters))             ; we're dealing with it the right now
		      (iterloop)                                ; this means the outermost sequence is ignored if encountered during the traversal
		      (begin
			(set! iters (cons iter iters))
			(set! iter (make-careful-iterator result))
			result)))
		 
		 ((not (eq? result #<eof>)) 
		  result)
		 
		 ((null? iters) 
		  #<eof>)
		 
		 (else
		  (set! seen-cycles (cons (iterator-sequence iter) seen-cycles))
		  (set! iter (car iters))
		  (set! iters (cdr iters))
		  (iterloop)))))))))


(define safe-find-if 
  (let ((documentation "(safe-find-if func sequence) searches sequence, and recursively any sequences it contains, for an element that satisfies func.\
Unlike full-find-if, safe-find-if can handle any circularity in the sequences."))
    (lambda (f sequence)
      (let ((iter (make-complete-iterator sequence)))
	(let loop ((x (iter)))
	  (if (f x) x
	      (and (not (and (eq? x #<eof>)
			     (iterator-at-end? iter)))
		   (loop (iter)))))))))

(define (safe-count-if f sequence)
  ;; currently the complete-iterator above skips repetitions, including the outer sequence,
  ;;   so this count will be off if there are repeated cycles?
  ;; Perhaps make an iterator that returns everything.
  (if (sequence? sequence)
      (if (procedure? f)
	  (let ((count 0))
	    (safe-find-if (lambda (x) (if (f x) (set! count (+ count 1))) #f) sequence)
	    count)
	  (error 'wrong-type-arg "safe-count-if first argument, ~A, should be a function" f))
      (error 'wrong-type-arg "safe-count-if second argument, ~A, should be a sequence" sequence)))




;;; ----------------
(define sequences->list 
  (let ((documentation "(sequences->list . sequences) returns a list of elements of all the sequences:\n\
    (sequences->list \"hi\" #(0 1) (hash-table '(a . 2))) -> '(#\\h #\\i 0 1 (a . 2))"))
    (lambda sequences
      (apply append 
	     (map (lambda (sequence) 
		    (map values sequence)) 
		  sequences)))))

(define concatenate 
  (let ((documentation "(concatenate type . sequences) concatenates sequences returning an object of type:\n\
    (concatenate vector '(1 2) #(3 4)) -> #(1 2 3 4)"))
    (lambda (type . sequences)
      (apply type (apply sequences->list sequences)))))

(define intersection 
  (let ((documentation "(intersection type . sequences) returns via type the intersection of the sequences:\n\
    (intersection vector '(1 2 3) #(2 3 4)) -> #(2 3)"))
    (lambda (type . sequences)
      (if (every? sequence? sequences)
	  (apply type (let ((lst ()))
			(if (pair? sequences)
			    (for-each (lambda (obj)
					(if (every? (lambda (seq) 
						      (member? obj seq))
						    (cdr sequences))
					    (set! lst (cons obj lst))))
				      (car sequences)))
			(reverse lst)))
	  (error 'wrong-type-arg "intersection arguments should be sequences: ~A" sequences)))))
  
(define union 
  (let ((documentation "(union type . sequences) returns via type the union of the sequences:\n\
    (union vector '(1 2 3) #(2 3 4)) -> #(1 2 3 4)"))
    (lambda (type . sequences)
      (apply type (let ((lst ()))
		    (for-each (lambda (obj)
				(if (not (member obj lst))
				    (set! lst (cons obj lst))))
			      (apply sequences->list sequences))
		    (reverse lst))))))

(define asymmetric-difference 
  (let ((documentation "(asymmetric-difference type . sequences) returns the elements in the rest of the sequences that are not in the first:\n\
    (asymmetric-difference vector '(1 2 3) #(2 3 4) '(1 5)) -> #(4 5)"))
    (lambda (type . sequences) ; complement, elements in B's not in A
      (if (not (and (pair? sequences)
		    (pair? (cdr sequences))))
	  (type)
	  (collect-if type (lambda (obj) 
			     (not (member obj (car sequences))))
		      (apply union list (cdr sequences)))))))

(define cl-set-difference 
  (let ((documentation "(cl-set-difference type .sequences) returns the elements in the first sequence that are not in the rest of the sequences:\n\
    (cl-set-difference vector '(1 2 3) #(2 3 4) '(1 5)) -> #()"))
    (lambda (type . sequences)     ; CL: elements in A not in B's
      (if (not (and (pair? sequences)
		    (pair? (cdr sequences))))
	  (type)
	  (let ((others (apply union list (cdr sequences))))
	    (collect-if type (lambda (obj) 
			       (not (member obj others)))
			(car sequences)))))))

(define symmetric-difference 
  (let ((documentation "(symmetric-difference type .sequences) returns the elements that are in an odd number of the sequences:\n\
    (symmetric-difference vector '(1 2 3) #(2 3 4) '(5)) -> #(1 4 5)"))
    (lambda (type . sequences)  ; xor, elements in an odd number of sequences (logxor A B...)
      (let ((all (apply sequences->list sequences)))
	(collect-if type (lambda (obj) 
			   (odd? (count-if (lambda (x) 
					     (equal? x obj)) 
					   all))) 
		    (apply union list sequences))))))

(define power-set 
  (let ((documentation "(power-set type . sequences) returns the power set of the union of the elements in the sequences."))
    (lambda (type . sequences) ; ignoring repeats
      (apply type
	     (let pset ((set (apply union list sequences)))
	       (if (null? set)
		   '(())
		   (let ((rest (pset (cdr set))))
		     (append rest (map (lambda (subset) (cons (car set) subset)) rest)))))))))

;;; ----------------
(define ->predicate
  (let ((predicates (list integer? rational? real? complex? number?
			  byte-vector? string?
			  float-vector? int-vector? vector?
			  null? proper-list? pair? list? 
			  keyword? gensym? symbol?
			  char? string?
			  hash-table?
			  iterator?
			  continuation? 
			  input-port? output-port? 
			  let? 			     
			  dilambda? procedure? macro?
			  boolean?
			  random-state? 
			  eof-object? 
			  c-object?
			  c-pointer? 
			  (lambda (obj) 
			    (eq? obj #<unspecified>))
			  (lambda (obj) 
			     (eq? obj #<undefined>))
			  (lambda (obj)
			    (memq obj (list quote if when unless begin set! let let* letrec letrec* cond and or case do
					    lambda lambda* define define* define-macro define-macro* define-bacro define-bacro*
					    define-constant with-baffle macroexpand with-let)))))
	(documentation "(->predicate obj) returns the type predicate function for obj: (->predicate 31) -> integer?"))
    (lambda (obj)
      (find-if (lambda (pred) (pred obj)) predicates))))

(define add-predicate 
  (let ((documentation "(add-predicate p) adds p (a boolean function of one argument) to the list of predicates used by ->predicate"))
    (lambda (p)
      (if (and (procedure? p)
	       (aritable? p 1))
	  (let ((e (funclet ->predicate)))
	    (set! (e 'predicates) (cons p (e 'predicates))))
	  (error 'wrong-type-arg "add-predicate argument, ~A, is not a procedure of one argument" p)))))

(define typeq? 
  (let ((documentation "(typeq? . objs) returns #t if all objs have the same type (as determined by ->predicate)"))
    (lambda objs
      (or (null? objs)
	  (every? (->predicate (car objs)) (cdr objs))))))

(define-macro (typecase expr . clauses) ; actually type=any boolean func
  (let ((obj (gensym)))
    `(begin                             ; normally this would be (let ((,obj ,expr)) ...)
       (define ,obj ,expr)              ;   but use begin so that internal defines are not blocked	    
       (cond ,@(map (lambda (clause)         
		      (if (memq (car clause) '(#t else))
			  clause
			  (if (= (length (car clause)) 1)
			      (cons (list (caar clause) obj) (cdr clause))
			      (cons (cons 'or (map (lambda (type)
						     (list type obj))
						   (car clause)))
				    (cdr clause)))))
		    clauses)))))



;;; ----------------
(define 2^n? 
  (let ((documentation "(2^n? x) returns #t if x is a power of 2"))
    (lambda (x)
      (and (integer> x)
	   (not (zero? x)) 
	   (zero? (logand x (- x 1)))))))

(define (2^n-1? x) 
  (and (integer? x)
       (zero? (logand x (+ x 1)))))

(define (lognand . ints) 
  (lognot (apply logand ints)))

(define (lognor . ints) 
  (lognot (apply logior ints)))

(define (logeqv . ints)
  (lognot (apply logxor (if (odd? (length ints))
			    (values -1 ints) ; Clisp does it this way
			    ints))))

(define (log-none-of . ints)  ; bits on in none of ints
  (lognot (apply logior ints)))

(define log-all-of logand)    ; bits on in all of ints
(define log-any-of logior)    ; bits on in at least 1 of ints

(define (log-n-of n . ints)   ; return the bits on in exactly n of ints
  (if (not (integer? n))
      (error 'wrong-type-arg "log-n-of first argument, ~A, should be an integer" n)
      (if (not (every? integer? ints))
	  (error 'wrong-type-arg "log-n-of ints arguments, ~A, should all be integers" ints)
	  (let ((len (length ints)))
	    (cond ((= len 0) (if (= n 0) -1 0))
		  ((= n 0)   (lognot (apply logior ints)))
		  ((= n len) (apply logand ints))
		  ((> n len) 0)
		  (#t 
		   (do ((1s 0)
			(prev ints)
			(nxt (cdr ints))
			(ln (- len 1))
			(nn (- n 1))
			(i 0 (+ i 1)))
		       ((= i len) 1s)
		     (let ((cur (ints i)))
		       (if (= i 0)
			   (set! 1s (logior 1s (logand cur (apply log-n-of nn nxt))))
			   (let ((mid (cdr prev)))
			     (set! (cdr prev) (if (= i ln) () (cdr mid)))
			     (set! 1s (logior 1s (logand cur (apply log-n-of nn ints))))
			     (set! (cdr prev) mid)
			     (set! prev mid)))))))))))

;; from Rick
(define (byte siz pos) ;; -> cache size, position and mask.
  (list siz pos (ash (- (ash 1 siz) 1) pos)))

(define byte-size car)
(define byte-position cadr)
(define byte-mask caddr)

(define (ldb bytespec integer)
  (ash (logand integer (byte-mask bytespec))
       (- (byte-position bytespec))))

(define (dpb integer bytespec into)
  (logior (ash (logand integer (- (ash 1 (byte-size bytespec)) 1)) (byte-position bytespec))
	  (logand into (lognot (byte-mask bytespec)))))


;;; ----------------
(define-macro* (define-class class-name inherited-classes (slots ()) (methods ()))
  `(let ((outer-env (outlet (curlet)))
	 (new-methods ())
	 (new-slots ()))
     
     (for-each
      (lambda (class)
	;; each class is a set of nested environments, the innermost (first in the list)
	;;   holds the local slots which are copied each time an instance is created,
	;;   the next holds the class slots (global to all instances, not copied);
	;;   these hold the class name and other such info.  The remaining environments
	;;   hold the methods, with the localmost method first.  So in this loop, we
	;;   are gathering the local slots and all the methods of the inherited
	;;   classes, and will splice them together below as a new class.
	
	(set! new-slots (append (let->list class) new-slots))
	(do ((e (outlet (outlet class)) (outlet e)))
	    ((or (not (let? e))
		 (eq? e (rootlet))))
	  (set! new-methods (append (let->list e) new-methods))))
      ,inherited-classes)
     
     (let ((remove-duplicates 
	    (lambda (lst)         ; if multiple local slots with same name, take the localmost
	      (letrec ((rem-dup
			(lambda (lst nlst)
			  (cond ((null? lst) nlst)
				((assq (caar lst) nlst) (rem-dup (cdr lst) nlst))
				(else (rem-dup (cdr lst) (cons (car lst) nlst)))))))
		(reverse (rem-dup lst ()))))))
       (set! new-slots 
	     (remove-duplicates
	      (append (map (lambda (slot)
			     (if (pair? slot)
				 (cons (car slot) (cadr slot))
				 (cons slot #f)))
			   ,slots)                    ; the incoming new slots, #f is the default value
		      new-slots))))                   ; the inherited slots
     
     (set! new-methods 
	   (append (map (lambda (method)
			  (if (pair? method)
			      (cons (car method) (cadr method))
			      (cons method #f)))
			,methods)                     ; the incoming new methods
		   
		   ;; add an object->string method for this class (this is already a generic function).
		   (list (cons 'object->string 
			       (lambda* (obj (use-write #t))
				 (if (eq? use-write :readable)    ; write readably
				     (format #f "(make-~A~{ :~A ~W~^~})" 
					     ',class-name 
					     (map (lambda (slot)
						    (values (car slot) (cdr slot)))
						  obj))
				     (format #f "#<~A: ~{~A~^ ~}>" 
					     ',class-name
					     (map (lambda (slot)
						    (list (car slot) (cdr slot)))
						  obj))))))
		   (reverse! new-methods)))                      ; the inherited methods, shadowed automatically
     
     (let ((new-class (openlet
                       (apply sublet                             ; the local slots
			      (sublet                            ; the global slots
				  (apply inlet                   ; the methods
					 (reverse new-methods))
				'class-name ',class-name         ; class-name slot
				'inherited ,inherited-classes
				'inheritors ())                  ; classes that inherit from this class
			      new-slots))))
       
       (varlet outer-env                  
	 ',class-name new-class                                  ; define the class as class-name in the calling environment
	 
	 ;; define class-name? type check
	 (string->symbol (string-append (symbol->string ',class-name) "?"))
	 (lambda (obj)
	   (and (let? obj)
		(eq? (obj 'class-name) ',class-name))))
       
       (varlet outer-env
	 ;; define the make-instance function for this class.  
	 ;;   Each slot is a keyword argument to the make function.
	 (string->symbol (string-append "make-" (symbol->string ',class-name)))
	 (apply lambda* (map (lambda (slot)
			       (if (pair? slot)
				   (list (car slot) (cdr slot))
				   (list slot #f)))
			     new-slots)
		`((let ((new-obj (copy ,,class-name)))
		    ,@(map (lambda (slot)
			     `(set! (new-obj ',(car slot)) ,(car slot)))
			   new-slots)
		    new-obj))))
       
       ;; save inheritance info for this class for subsequent define-method
       (letrec ((add-inheritor (lambda (class)
				 (for-each add-inheritor (class 'inherited))
				 (if (not (memq new-class (class 'inheritors)))
				     (set! (class 'inheritors) (cons new-class (class 'inheritors)))))))
	 (for-each add-inheritor ,inherited-classes))
       
       ',class-name)))

(define-macro (define-generic name)    ; (define (genfun any) ((any 'genfun) any))
  `(define ,name 
     (lambda args 
       (let ((gf ((car args) ',name))) ; get local definition
	 (if (not (eq? gf ,name))      ; avoid infinite recursion
             (apply gf args)
	     (error 'syntax-error "attempt to call generic function wrapper recursively"))))))

(define-macro (define-slot-accessor name slot)
  `(define ,name (dilambda 
		  (lambda (obj) (obj ',slot)) 
		  (lambda (obj val) (set! (obj ',slot) val)))))

(define-macro (define-method name-and-args . body)
  `(let* ((outer-env (outlet (curlet)))
	  (method-name (car ',name-and-args))
	  (method-args (cdr ',name-and-args))
	  (object (caar method-args))
	  (class (symbol->value (cadar method-args)))
	  (old-method (class method-name))
	  (method (apply lambda* method-args ',body)))
     
     ;; define the method as a normal-looking function
     ;;   s7test.scm has define-method-with-next-method that implements call-next-method here
     ;;   it also has make-instance 
     (varlet outer-env
       method-name (apply lambda* method-args 
			  `(((,object ',method-name)
			     ,@(map (lambda (arg)
				      (if (pair? arg) (car arg) arg))
				    method-args)))))
     
     ;; add the method to the class
     (varlet (outlet (outlet class)) method-name method)
     
     ;; if there are inheritors, add it to them as well, but not if they have a shadowing version
     (for-each
      (lambda (inheritor) 
	(if (not (eq? (inheritor method-name) #<undefined>)) ; defined? goes to the global env
	    (if (eq? (inheritor method-name) old-method)
		(set! (inheritor method-name) method))
	    (varlet (outlet (outlet inheritor)) method-name method)))
      (class 'inheritors))
     
     method-name))

(define (all-methods obj method)
  ;; for arbitrary method combinations: this returns a list of all the methods of a given name
  ;;   in obj's class and the classes it inherits from (see example below)
  (if (symbol? method)
      (let ((methods (let ((base-method (obj method)))
		       (if (procedure? base-method) (list base-method) ()))))
	(for-each 
	 (lambda (ancestor)
	   (let ((next-method (ancestor method)))
	     (if (and (procedure? next-method)
		      (not (memq next-method methods)))
		 (set! methods (cons next-method methods)))))
	 (obj 'inherited))
	(reverse methods))
      (error 'wrong-type-arg "all-methods 'method argument should be a symbol: ~A" method)))



;;; ----------------
(define for-each-subset 
  (let ((documentation "(for-each-subset func args) forms each subset of args, then applies func to the subsets that fit its arity"))
    (lambda (func args)
      (let subset ((source args)
                   (dest ())
                   (len 0))
        (if (null? source)
            (if (aritable? func len)
                (apply func dest))
            (begin
              (subset (cdr source) (cons (car source) dest) (+ len 1))
              (subset (cdr source) dest len)))))))

(define for-each-permutation 
  (let ((documentation "(for-each-permutation func vals) applies func to every permutation of vals:\n\
    (for-each-permutation (lambda args (format () \"~{~A~^ ~}~%\" args)) '(1 2 3))"))
    (lambda (func vals)
      (define (pinner cur nvals len)
	(if (= len 1)
	    (apply func (car nvals) cur)
	    (do ((i 0 (+ i 1)))                       ; I suppose a named let would be more Schemish
		((= i len))
	      (let ((start nvals))
		(set! nvals (cdr nvals))
		(let ((cur1 (cons (car nvals) cur)))  ; add (car nvals) to our arg list
		  (set! (cdr start) (cdr nvals))      ; splice out that element and 
		  (pinner cur1 (cdr start) (- len 1)) ;   pass a smaller circle on down, "wheels within wheels"
		  (set! (cdr start) nvals))))))       ; restore original circle
      (let ((len (length vals)))
	(set-cdr! (list-tail vals (- len 1)) vals)    ; make vals into a circle
	(pinner () vals len)
	(set-cdr! (list-tail vals (- len 1)) ())))))    ; restore its original shape



;;; ----------------
(define (clamp minimum x maximum)
  (min maximum (max x minimum)))

(define (1- x) (- x 1))
(define (1+ x) (+ x 1))

(define n-choose-k 
  (let ((documentation "(n-choose-k n k) returns the binomial coefficient C(N,K)"))
    (lambda (n k)
      (if (not (integer? n))
	  (error 'wrong-type-arg "n-choose-k 'n argument, ~A, should be an integer" n)
	  (if (not (integer? k))
	      (error 'wrong-type-arg "n-choose-k 'k argument, ~A, should be an integer" k)
	      (let ((mn (min k (- n k))))
		(if (or (negative? mn)
			(negative? n))
		    0
		    (if (= mn 0)
			1
			(let ((mx (max k (- n k))))
			  (do ((cnk (+ 1 mx))
			       (i 2 (+ i 1)))
			      ((> i mn) cnk)
			    (set! cnk (/ (* cnk (+ mx i)) i))))))))))))
	      
;;; ----------------

(define continuable-error
  (let ((documentation "(continuable-error . args) is (apply error args) wrapped in a continuation named 'continue."))
    (lambda args
      (call/cc 
       (lambda (continue)
	 (apply error args))))))

(define continue-from-error ; maybe arg for value to pass back
  (let ((documentation "(continue-from-error) tries to continue from the point of the earlier continuable-error"))
    (lambda ()
      (if (continuation? ((owlet) 'continue))
	  (((owlet) 'continue))))))

(define (call-with-input-vector v proc)
  (if (vector? v)
      (let ((i -1))
	(proc (openlet
	       (inlet 'read (lambda (p)
			      (v (set! i (+ i 1))))))))
      (error 'wrong-type-arg "call-with-input-vector first argument, ~A, should be a vector" v)))

(define (call-with-output-vector proc)
  (let* ((size 1)
	 (v (make-vector size #f))
	 (i 0)
	 (write-to-vector (lambda (obj p)
			    (when (= i size) ; make the vector bigger to accommodate the output
			      (set! v (copy v (make-vector (set! size (* size 2)) #f))))
			    (set! (v i) obj)
			    (set! i (+ i 1))
			    #<unspecified>))) ; that's what write/display return!
    (proc (openlet
	   (inlet 'write (lambda* (obj p)
			   ((if (not (let? p)) write write-to-vector) obj p))
		  'display (lambda* (obj p)
			     ((if (not (let? p)) display write-to-vector) obj p))
		  'format (lambda (p . args)
			    (if (not (let? p))
				(apply format p args)
				(write (apply format #f args) p))))))
    (make-shared-vector v (list i)))) ; ignore extra trailing elements



;;; ----------------

(define* (flatten-let e (n -1))
  (if (not (let? e))
      (error 'wrong-type-arg "flatten-let argument, ~A, is not a let" e)
      (do ((slots ())
	   (pe e (outlet pe))
	   (i 0 (+ i 1)))
	  ((or (eq? pe (rootlet))
	       (= i n))
	   (apply inlet slots))
	(for-each (lambda (slot)
		    (if (not (or (assq (car slot) slots)
				 (constant? (car slot)))) ; immutable symbol
			(set! slots (cons slot slots))))
		  pe))))

(define* (owlets (ows 1)) (flatten-let (owlet) ows))
  


;;; ----------------

(define-macro (reflective-let vars . body)
  `(let ,vars
     ,@(map (lambda (vr)
	      `(set! (symbol-access ',(car vr))
		     (lambda (s v)
		       (format *stderr* "~S -> ~S~%" s v)
		       v)))
	    vars)
     ,@body))

#|
(define-bacro (reflective-probe)
  (with-let (inlet 'e (outlet (curlet)))
    (for-each (lambda (var)
		(format *stderr* "~S: ~S~%" (car var) (cdr var)))
	      e)))
|#
;; ideally this would simply vanish, and make no change in the run-time state, but (values) here returns #<unspecified>
;;   (let ((a 1) (b 2)) (list (set! a 3) (reflective-probe) b)) -> '(3 2) not '(3 #<unspecified> 2)
;;   I was too timid when I started s7 and thought (then) that (abs -1 (values)) should be an error
;; perhaps if we want it to disappear:

(define-bacro (reflective-probe . body)
  (with-let (inlet :e (outlet (curlet)) 
		   :body body)
    (for-each (lambda (var)
		(format *stderr* "~S: ~S~%" (car var) (cdr var)))
	      e)
    (cons 'begin body)))

;; now (let ((a 1) (b 2)) (list (set! a 3) (reflective-probe b))) -> '(3 2)
;; and (let ((a 1) (b 2)) (list (set! a 3) (reflective-probe) b)) -> '(3 () 2)
;; or use it to print function args: (define (f a b) (reflective-probe) (+ a b))

;; could this use reactive-lambda* to show changes as well?


;;; ----------------

(define (gather-symbols expr ce lst ignore)
  (cond ((symbol? expr)
	 (if (or (memq expr lst)
		 (memq expr ignore)
		 (procedure? (symbol->value expr ce))
		 (eq? (let symbol->let ((sym expr)
					(ce ce))
			(if (defined? sym ce #t)
			    ce
			    (and (not (eq? ce (rootlet)))
				 (symbol->let sym (outlet ce)))))
		      (rootlet)))
	     lst
	     (cons expr lst)))

	((not (pair? expr)) lst)

	((not (and (pair? (cdr expr)) (pair? (cddr expr))))
	 (if (eq? (car expr) '_)
	     (cons expr lst)
	     (gather-symbols (cdr expr) ce (gather-symbols (car expr) ce lst ignore) ignore)))

	((pair? (cadr expr))
	 (gather-symbols (case (car expr)
			   ((let let* letrec letrec* do)
			    (values (cddr expr) ce lst (append ignore (map car (cadr expr)))))
			   ((lambda) 
			    (values (cddr expr) ce lst (append ignore (cadr expr))))
			   ((lambda*)
			    (values (cddr expr) ce lst (append ignore (map (lambda (a) (if (pair? a) (car a) a)) (cadr expr)))))
			   (else
			    (values (cdr expr) ce (gather-symbols (car expr) ce lst ignore) ignore)))))

	((and (eq? (car expr) 'lambda)
	      (symbol? (cadr expr)))
	 (gather-symbols (cddr expr) ce lst (append ignore (list (cadr expr)))))

	(else 
	 (gather-symbols (cdr expr) ce (gather-symbols (car expr) ce lst ignore) ignore))))


(define-bacro (reactive-set! place value)
  (with-let (inlet 'place place                      ; with-let here gives us control over the names
		   'value value 
		   'e (outlet (curlet)))             ; the run-time (calling) environment
    (let ((nv (gensym))
	  (ne (gensym)))
      `(begin
	 (define ,ne ,e)
	 ,@(map (lambda (sym)
		  (if (symbol? sym)
		      `(set! (symbol-access ',sym)
			     (lambda (s v)  
			       (let ((,nv ,(if (not (with-let (sublet e 'sym sym) 
						      (symbol-access sym)))
					       'v
					       `(begin (,(procedure-source (with-let (sublet e 'sym sym) 
									     (symbol-access sym))) 
							s v)))))
				 (with-let (sublet ,ne ',sym ,nv)
				   (set! ,place ,value))
				 ,nv)))
		      (if (not (and (eq? (car sym) '_)
				    (pair? (cdr sym))
				    (integer? (cadr sym))
				    (null? (cddr sym))))
			  (error 'wrong-type-arg "reactive-vector can't handle: ~S~%" sym)
			  (let ((index (cadr sym)))
			    `(set! (_ 'local-set!)
				   (apply lambda '(obj i val)
					  (append (cddr (procedure-source (_ 'local-set!)))
						  `((if (= i ,,index) (set! ,',place ,',value))))))))))
		(gather-symbols value e () ()))
	 (set! ,place ,value)))))

#|
(let ((a 1)
      (b 2)
      (c 3))
  (reactive-set! b (+ c 4))  ; order matters!
  (reactive-set! a (+ b c))
  (set! c 5)
  a)

(let ((a 1) (v (vector 1 2 3))) (reactive-set! (v 1) (* a 3)) (set! a 4) v)
|#

;; just a first stab at this:

(define reactive-vector
  (let ()
    (require mockery.scm)
    (define make-mock-vector (*mock-vector* 'make-mock-vector))

    (define (reactive-vector-1 e . args)
      ;; set up accessors for any element that has an expression as its initial value
      ;; if any element depends on some other element, return a mock-vector with setter fixed up

      (let ((code `(let ((_ (,(if (any? (lambda (a) 
					  (tree-member '_ a)) 
					args)
				  '((funclet reactive-vector) 'make-mock-vector)
				  'make-vector)
			     ,(length args)))))))
	(let ((ctr 0))
	  (for-each
	   (lambda (arg)
	     (set! code (append code `((reactive-set! (_ ,ctr) ,arg))))
	     (set! ctr (+ ctr 1)))
	   args))
	(append code `(_))))
    
    (define-bacro (reactive-vector . args)
      (apply ((funclet reactive-vector) 'reactive-vector-1) (outlet (curlet)) args))))


#|
(let ((a 1)) (let ((v (reactive-vector a (+ a 1) 2))) (set! a 4) v)) -> #(4 5 2)
(let* ((a 1) (v (reactive-vector a (+ a 1) 2))) (set! a 4) v) -> #(4 5 2)
(let* ((a 1) (v (reactive-vector a (+ a 1) (* 2 (_ 0))))) (set! a 4) v) -> #(4 5 8)
;;; mock-vector could also be used for constant or reflective vectors, etc -- just like symbol-access but element-wise
|#

;; another experiment:

(define-bacro (reactive-format port ctrl . args)
  (with-let (inlet 'e (outlet (curlet))
		   'args args
		   'port port
		   'ctrl ctrl)
    (let* ((syms (gather-symbols args e () ()))
	   (sa's (map symbol-access syms)))
      `(begin
	 ,@(map (lambda (sym sa)
		  `(set! (symbol-access ',sym) 
			 (lambda (s v)
			   (let ((result (if ,sa (apply ,sa s v ()) v)))
			     (with-let (sublet ,e ',sym result)
			       (format ,port ,ctrl ,@args)) ; is this equivalent to an exported closure in the GC?
			     result))))
		syms sa's)
	 (format ,port ,ctrl ,@args)))))


;;; this is not pretty
;;; part of the complexity comes from the hope to be tail-callable, but even a version
;;;   using dynamic-wind is complicated because of shadowing
;;; what I think we want here is a globally accessible way to see set! that does not
;;;   require non-local state (not a hook with its list of functions, or symbol-access)
;;;   and that doesn't bring s7 to a halt.  Perhaps a symbol-access function that
;;;   traverses the let-chain (like *features*) looking for something??  But the relevant
;;;   chain is on the stack (is it?), so it won't be quick.  And weak refs are asking for trouble.
;;;   (let ((a 1)) (define (set-a x) (set! a x)) (let ((b 2)) (reactive-set! b (+ a 1)) (set-a 3) b))
;;;   Perhaps a way to share the original's slot?  No slow down, transparent, local setter can
;;;   run its own accessor, set -> shared slot so all sharers see the new value,
;;;   but how to trigger all accessors?
;;;     (set! (symbol-slot 'a e1) (symbol-slot 'a e2))
;;;   this isn't currently doable -- object.slt.val is a pointer, not a pointer to a pointer
;;;   there is the symbol's extra slot, but it is global.  I wonder how much slower s7 would be
;;;   with a pointer to a pointer here -- are there any other places this would be useful?
;;;   even with this, the entire accessor chain is not triggered.
;;; so, use with-accessors and reactive-set! for complex cases

(define unique-reactive-let-name ; the alternative is (apply define-bacro ...) with a top-level gensym
  (let ((name #f))
    (lambda ()
      (if (gensym? name)
	  name
	  (set! name (gensym "v"))))))

(define-bacro (reactive-let vars . body)
  (with-let (inlet 'vars vars 'body body 'e (outlet (curlet)))
    (let ((bindings ())
	  (accessors ())
	  (setters ())
	  (gs (gensym))
	  (v (unique-reactive-let-name)))

      (define (rlet-symbol sym)
	(symbol "{" (symbol->string sym) "}-rlet"))

      (for-each 
       (lambda (bd)
	 (let ((syms (gather-symbols (cadr bd) e () ())))
	   (for-each 
	    (lambda (sym)
	      (let ((fname (gensym (symbol->string sym))))
		(set! bindings (cons `(,fname (lambda (,sym) ,(copy (cadr bd)))) bindings))
		(if (not (memq sym setters))
		    (set! setters (cons sym setters)))
		(let ((prev (assq sym accessors)))
		  (if prev
		      (set-cdr! prev (cons (list 'set! (car bd) (list fname v)) (cdr prev)))
		      (set! accessors (cons (list sym (list 'set! (car bd) (list fname v))) accessors))))))
		      ;; (append `((set! ,(car bd) (,fname ,v))) (cdr prev)))
		      ;; (set! accessors (cons (cons sym `((set! ,(car bd) (,fname ,v)))) accessors))))))
	    syms)
	   (set! bindings (cons bd bindings))))
       vars)

      (let ((bsyms (gather-symbols body e () ()))
	    (nsyms ()))
	(for-each (lambda (s)
		    (if (and (with-let (sublet e (quote gs) s) 
			       (symbol-access gs))
			     (not (assq s bindings)))
			(if (not (memq s setters))
			    (begin
			      (set! setters (cons s setters))
			      (set! nsyms (cons (cons s (cdr (procedure-source (with-let (sublet e (quote gs) s) 
										 (symbol-access gs)))))
						nsyms)))
			    (let ((prev (assq s accessors)))
			      (if prev ; merge the two functions
				  (set-cdr! prev (append (cdddr (procedure-source (with-let (sublet e (quote gs) s) 
										    (symbol-access gs))))
							 (cdr prev))))))))
		  bsyms)

	`(let ,(map (lambda (sym)
		      (values
		       `(,(rlet-symbol sym) (lambda (,v) (set! ,sym ,v)))
		       `(,sym ,sym)))
		    setters)
	   (let ,(reverse bindings) 
	     ,@(map (lambda (sa)
		      (if (assq (car sa) bindings)
			  (values)
			  `(set! (symbol-access ',(car sa))
				 (lambda (,(gensym) ,v)
				   (,(rlet-symbol (car sa)) ,v)
				   ,@(cdr sa)
				   ,v))))
		    accessors)
	     ,@(map (lambda (ns)
		      `(set! (symbol-access ',(car ns))
			     (apply lambda ',(cdr ns))))
		    nsyms)
	     ,@body))))))


(define-macro (reactive-let* vars . body)
  (let add-let ((v vars))
    (if (pair? v)
	`(reactive-let ((,(caar v) ,(cadar v)))
	   ,(add-let (cdr v)))
	(cons 'begin body))))

;; reactive-letrec is not useful: lambdas already react and anything else is an error (use of #<undefined>)

(define-macro (reactive-lambda* args . body)
  `(let ((f (lambda* ,args ,@body))
	 (e (curlet)))
     (unless (eq? e (rootlet))

       (define (one-access s1 v)
	 (let* ((syms (map car e))
		(sa's (map (lambda (s) (symbol-access s e)) syms)))
	   (dynamic-wind
	       (lambda () (for-each (lambda (s) (if (not (eq? s s1)) (set! (symbol-access s e) #f))) syms))
	       (lambda () (f s1 v))
	       (lambda () (for-each (lambda (s a) (set! (symbol-access s e) a)) syms sa's)))))

       (for-each (lambda (s) (set! (symbol-access s e) one-access)) (map car e)))
     f))


(define-macro (with-accessors vars . body)
  `(let ((accessors ()))
     (dynamic-wind
	 (lambda ()
	   (set! accessors (map symbol-access ',vars)))
	 (lambda ()
	   ,@body)
	 (lambda ()
	   (for-each
	    (lambda (var accessor)
	      (set! (symbol-access var) accessor))
	    ',vars accessors)))))

;; (let ((a 1) (b 2)) (with-accessors (a b) (let ((c 3)) (reactive-set! c (+ (* 2 a) (* 3 b))) (set! a 4) c)))

#|
(let ((x 0.0)) (reactive-let ((y (sin x))) (set! x 1.0) y)) -- so "lifting" comes for free?

(map (lambda (s) (symbol-access (car s) e)) e)

(let ((a 1))
  (reactive-let ((b (+ a 1))
		 (c (* a 2)))
		(set! a 3)
		(+ c b)))

(let ((a 1) 
      (d 2))
  (reactive-let ((b (+ a d))
		 (c (* a d))
                 (d 0))
		(set! a 3)
		(+ b c)))

(let ((a 1))
  (reactive-let* ((b (+ a 1))
		  (c (* b 2)))
    (set! a 3)
    (+ c b)))

(let ((a 1))
  (reactive-let* ((b (+ a 1)))
    (set! a 3) 
    b))

(define rl (let ((a 1)
	         (b 2)
		 (c 3))
	     (reactive-lambda* (s v)
	       (format *stderr* "~S changed: ~S~%" s v))))

;; constant env:
;; (define e (let ((a 1) (b 2)) (reactive-lambda* (s v) ((curlet) s)) (curlet)))
|#

;;; what about (reactive-vector (v 0)) -- can we watch some other vector's contents?
;;;   if v were a mock-vector, we could use the same vector-set! stuff as now but with any name (how to distinguish?)
;;;   we can distinguish because this is happening at run-time where (v 0) has an ascertainable meaning
;;; how would reactive-hash-table work? (hash 'a (+ b 1)) and update 'a's value whenever b changes?
;;;   reactive-string? (reactive-string #\a c (integer->char a) (str 0) (_ 0))
;;;   reactive-eval reactive-if(expr changes)--reactive-assert for example


#|
;; this tests a bacro for independence of any runtime names
;; (bacro-shaker reactive-set! '(let ((a 21) (b 1)) (reactive-set! a (* b 2)) (set! b 3) a))

(define (bacro-shaker bac example)

  (define (swap-symbols old-code syms)
    (if (null? old-code)
	()
	(if (symbol? old-code)
	    (let ((x (assq old-code syms)))
	      (if x
		  (cdr x)
		  (copy old-code)))
	    (if (pair? old-code)
		(cons (swap-symbols (car old-code) syms)
		      (swap-symbols (cdr old-code) syms))
		(copy old-code)))))

  (let ((e (outlet (curlet)))
	(source (cddr (cadr (caddr (procedure-source bac))))))
    (let ((symbols (gather-symbols source (rootlet) () ()))
	  (exsyms (gather-symbols (cadr example) (rootlet) () ())))
      ;; now try each symbol at each position in exsyms, in all combinations
      
      (let ((syms ()))
	(for-each
	 (lambda (s)
	   (set! syms (cons (cons s s) syms)))
	 exsyms)
	(let ((result (eval example e)))

	  (define (g . new-args)
	    (for-each (lambda (a b) 
			(set-cdr! a b)) 
		      syms new-args)
	    (let ((code (swap-symbols example syms)))
	      (let ((new-result (catch #t 
				  (lambda ()
				    (eval code e))
				  (lambda args 
				    args))))
		(if (not (equal? result new-result))
		    (format *stderr* "~A -> ~A~%~A -> ~A~%"
			    example result
			    code new-result)))))

	  (define (f . args)
	    (for-each-permutation g args))
	    
	  (let ((subsets ())
		(func f)
		(num-args (length exsyms))
		(args symbols))
	    (define (subset source dest len)
	      (if (null? source)
		  (begin
		    (set! subsets (cons dest subsets))
		    (if (= len num-args)
			(apply func dest)))
		  (begin
		    (subset (cdr source) (cons (car source) dest) (+ len 1))
		    (subset (cdr source) dest len))))
	    (subset args () 0)))))))
|#



;;; ----------------

(define-macro (catch* clauses . error) 
  (let builder ((lst clauses))
    (if (null? lst)
	(apply values error)
	`(catch #t (lambda () ,(car lst)) (lambda args ,(builder (cdr lst)))))))


(define* (subsequence obj (start 0) end)
  (let ((new-len (let ((len (length obj)))
		   (- (min len (or end len)) start))))
    (if (negative? new-len)
	(error 'out-of-range "end: ~A should be greater than start: ~A" end start))

    (cond ((vector? obj) 
	   (make-shared-vector obj (list new-len) start))

          ((string? obj)
           (if (integer? end)
               (substring obj start end)
               (substring obj start)))

          ((not (pair? obj))
           (catch* (((obj 'subsequence) obj start end) 
		    (subsequence (obj 'value) start end))
		   #f))

          ((not end) 
	   (cdr* obj start))

          (else
           (do ((lst (make-list new-len #f))
		(i 0 (+ i 1)))
	       ((= i new-len) lst)
	     (set! (lst i) (obj (+ i start))))))))


(define (sequence->string val)
  (let ((ctrl-str (cond ((or (not (sequence? val)) 
			     (empty? val))    "~S")
			((vector? val)        "#(~{~A~| ~})")
			((let? val)           "(inlet ~{'~A~| ~})")
			((hash-table? val)    "(hash-table ~{'~A~| ~})")
			((not (string? val))  "(~{~A~| ~})")
			((byte-vector? val)   "#u8(~{~D~| ~})")
			(else                 "\"~{~A~|~}\""))))
    (format #f ctrl-str val)))


;;; ----------------

(define-macro (elambda args . body)  ; lambda but pass extra arg "*env*" = run-time env
  `(define-bacro (,(gensym) ,@args)
     `((lambda* ,(append ',args `((*env* (curlet))))
	 ,'(begin ,@body)) 
       ,,@args)))

(define-macro* (rlambda args . body) ; lambda* but eval arg defaults in run-time env
  (let ((arg-names (map (lambda (arg) (if (pair? arg) (car arg) arg)) args))
	(arg-defaults (map (lambda (arg) (if (pair? arg) (list (car arg) (list 'eval (cadr arg))) arg)) args)))
    `(define-bacro* (,(gensym) ,@arg-defaults)
       `((lambda ,',arg-names ,'(begin ,@body)) ,,@arg-names))))


;;; ----------------

(if (not (or (defined? 'apropos)
	     (provided? 'snd)))
    (define* (apropos name (port *stdout*) (e (rootlet)))
      (let ((ap-name (if (string? name) 
			 name 
			 (if (symbol? name) 
			     (symbol->string name)
			     (error 'wrong-type-arg "apropos argument 1 should be a string or a symbol"))))
	    (ap-env (if (let? e) 
			e 
			(error 'wrong-type-arg "apropos argument 3 should be an environment")))
	    (ap-port (if (output-port? port) 
			 port
			 (error 'wrong-type-arg "apropos argument 2 should be an output port"))))
	(for-each
	 (lambda (binding)
	   (if (and (pair? binding)
		    (string-position ap-name (symbol->string (car binding))))
	       (format ap-port "~A: ~A~%" 
		       (car binding) 
		       (if (procedure? (cdr binding))
			   (procedure-documentation (cdr binding))
			   (cdr binding)))))
	 ap-env))))


;;; ----------------

;; these need to be globally accessible since they're inserted in arbitrary source
(define Display #f)
(define Display-port #f)

(let ((spaces 0)
      (*display-spacing* 2)         ; (set! ((funclet Display) '*display-spacing*) 1) etc
      (*display-print-length* 6)
      (*display* *stderr*)          ; exported via Display-port
      (e (gensym))
      (result (gensym))
      (vlp (gensym)))
  
  ;; local symbol access -- this does not affect any other uses of these symbols
  (set! (symbol-access '*display-spacing* (curlet))
	(lambda (s v) (if (and (integer? v) (not (negative? v))) v *display-spacing*)))
  
  (set! (symbol-access '*display-print-length* (curlet))
	(lambda (s v) (if (and (integer? v) (not (negative? v))) v *display-print-length*)))
  
  ;; export *display* -- just a convenience
  (set! Display-port (dilambda
		      (lambda () *display*)
		      (lambda (val) (set! *display* val))))
  
  (define (prepend-spaces)
    (format *display* (format #f "~~~DC" spaces) #\space))
  
  (define (display-let le e)
    (let ((vlp (*s7* 'print-length)))
      (for-each
       (lambda (slot)
	 (unless (or (gensym? (car slot))
		     (eq? (cdr slot) e))
	   (set! (*s7* 'print-length) *display-print-length*)
	   (let ((str (sequence->string (cdr slot))))
	     (set! (*s7* 'print-length) 30)
	     (format (Display-port) " :~A ~{~A~|~}" (car slot) str))))
       le)
      (set! (*s7* 'print-length) vlp)))
  
  (define (last lst)
    (let ((end (let ((len (length lst)))
		 (list-tail lst (if (negative? len) (abs len) (- len 1))))))
      (if (pair? end)
	  (car end)
	  end)))
  
  (define* (butlast lst (result ()))
    (if (or (not (pair? lst))
	    (null? (cdr lst)))
	(reverse result)
	(butlast (cdr lst) (cons (car lst) result))))
  
  (define* (remove-keys args (lst ()))
    (if (pair? args)
	(remove-keys (cdr args) 
		     (if (or (not (keyword? (car args)))
			     (eq? (car args) :rest))
			 (cons (car args) lst)
			 lst))
	(if (null? args)
	    (reverse lst)
	    (append (reverse lst) args))))
  
  (define (proc-walk source)
    
    (define (display-format str . args)
      `(let ((,vlp (*s7* 'print-length)))
	 (with-let (funclet Display)
	   (set! (*s7* 'print-length) *display-print-length*)
	   (prepend-spaces))
	 (format (Display-port) ,str ,@args)
	 (set! (*s7* 'print-length) ,vlp)))
  
    (define (walk-let-body source)
      (let ((previous (butlast source))
	    (end (last source)))
	`(begin
	   ,@previous
	   (let ((,result ,end))
	     (with-let (funclet Display)
	       (prepend-spaces))
	     (format (Display-port) "  ~A~A) -> ~A~%"
		     ,(if (pair? previous) " ... " "")
		     ',end
		     ,result)
	     ,result))))
    
    (if (not (pair? source))
	source
	(case (car source)
	  
	  ((let let* letrec letrec*)                
	   ;; show local variables, (let vars . body) -> (let vars print-vars . body)
	   (if (symbol? (cadr source))           ; named let?
	       (append 
		(list (car source)               ; let
		      (cadr source)              ; name
		      (caddr source)             ; vars
		      (display-format "(let ~A (~{~A~| ~})~%" (cadr source) '(outlet (curlet))))
		(walk-let-body (cdddr source)))      ; body
	       (append 
		(list (car source)               ; let
		      (cadr source)              ; vars
		      (display-format "(~A (~{~A~| ~})~%" (car source) '(outlet (curlet))))
		(walk-let-body (cddr source)))))     ; body
	  
	  ((or and)
	   ;; report form that short-circuits the evaluation
	   (cons (car source)
		 (let ((ctr -1)
		       (len (- (length source) 2))
		       (eob (if (eq? (car source) 'or) 'when 'unless)))
		   (map (lambda (expr)
			  (set! ctr (+ ctr 1))
			  `(let ((,result ,expr))
			     (,eob ,result
				   (format (Display-port) "  (~A ~A~A~A) -> ~A~%" 
					   ',(car source)
					   ,(if (> ctr 0) " ... " "")
					   ',expr 
					   ,(if (< ctr len) " ... " "")
					   ,result))
			     ,result))
			(cdr source)))))
	  
	  ((begin with-let with-baffle)
	   ;; report last form
	   (let ((previous (butlast (cdr source)))
		 (end (last source)))
	     `(,(car source)
	       ,@previous
	       (let ((,result ,end))
		 (format (Display-port) "(~A ~A~A) -> ~A~%"
			 ',(car source)
			 ,(if (pair? previous) " ... " "")
			 ',end
			 ,result)
		 ,result))))
	  
	  ((when unless)
	   ;; report expr if body not walked, else report last form of body
	   (let ((previous (butlast (cddr source)))
		 (end (last (cddr source))))
	     `(,(car source) (let ((,result ,(cadr source)))
			       (,(car source) (not ,result)
				(format (Display-port) "(~A ~A -> ~A ...)~%"
					',(car source)
					',(cadr source)
					,result))
			       ,result)
	       ,@previous
	       (let ((,result ,end))
		 (format (Display-port) "(~A ... ~A) -> ~A~%"
			 ',(car source)
			 ',end
			 ,result)
		 ,result))))
	  
	  ((quote)
	   source)
	  
	  ((cond)
	   ;; report form that satisifies cond
	   (let ((ctr -1)
		 (len (- (length source) 2)))
	     `(cond ,@(map (lambda (clause)
			     (let ((test (car clause))
				   (body (cdr clause)))
			       (set! ctr (+ ctr 1))
			       (if (eq? (car body) '=>) 
				   `(,test => (lambda (,result) 
						(let ((,result (,@(cdr body) ,result)))
						  (format (Display-port) "  (cond ~A~A~A) -> ~A~%" 
							  ,(if (> ctr 0) " ... " "")
							  ',clause
							  ,(if (< ctr len) " ... " "")
							  ,result)
						  ,result)))
				   `(,test (let ((,result (begin ,@body)))
					     (format (Display-port) "  (cond ~A~A~A) -> ~A~%" 
						     ,(if (> ctr 0) " ... " "")
						     ',clause
						     ,(if (< ctr len) " ... " "")
						     ,result)
					     ,result)))))
			   (cdr source)))))
	  
	  ((case)
	   ;; as in cond but include selector value in [] and report fall throughs
	   (let ((ctr -1)
		 (len (- (length source) 3))
		 (default (member '(else #t) (cddr source) (lambda (a b) 
							     (memq (car b) a)))))
	     `(case ,(cadr source)
		,@(append 
		   (map (lambda (clause)
			  (let ((test (car clause))
				(body (cdr clause)))
			    (set! ctr (+ ctr 1))
			    (if (eq? (car body) '=>)
				`(,test => (lambda (,result)
					     (let ((,result (,@(cdr body) ,result)))
					       (format (Display-port) "  (case [~A] ~A~A~A) -> ~A~%" 
						       ,(cadr source)
						       ,(if (> ctr 0) " ... " "")
						       ',clause
						       ,(if (< ctr len) " ... " "")
						       ,result)
					       ,result)))
				`(,test (let ((,result (begin ,@body)))
					  (format (Display-port) "  (case [~A] ~A~A~A) -> ~A~%" 
						  ,(cadr source)
						  ,(if (> ctr 0) " ... " "")
						  ',clause
						  ,(if (< ctr len) " ... " "")
						  ,result)
					  ,result)))))
			(cddr source))
		   (if default
		       ()
		       `((else 
			  (format (Display-port) "  (case [~A] falls through~%" ,(cadr source)) 
			  #<unspecified>)))))))
	  
	  ((dynamic-wind)
	   ;; here we want to ignore the first and last clauses, and report the last of the second
	   (let ((body (let ((p (caddr source)))
			 (and (eq? (car p) 'lambda)
			      (cddr p)))))
	     (if (not body)
		 source
		 (let ((previous (and body (butlast body)))
		       (end (and body (last body))))
		   `(dynamic-wind
			,(cadr source)
			(lambda ()
			  ,@previous
			  (let ((,result ,end))
			    (format (Display-port) "(dynamic-wind ... ~A) -> ~A~%" ',end ,result)
			    ,result))
			,(cadddr source))))))
	  
	  (else
	   (cons (proc-walk (car source)) 
		 (proc-walk (cdr source)))))))

  
  (define-macro (Display-1 definition)
    (if (not (and (pair? definition)
		  (memq (car definition) '(define define*))
		  (pair? (cdr definition))
		  (pair? (cadr definition))))

	;; (Display <anything-else>)
	(proc-walk definition)                                                    ; (Display (+ x 1)) etc
	
	;; (Display (define (f ...) ...)
	(let ((func (caadr definition))
	      (args (cdadr definition))
	      (body (cons 'begin (proc-walk (cddr definition)))))
	  ;(format *stderr* "~A ~A ~A~%" func args body)
	  (let* ((no-noise-args (remove-keys args))                               ; omit noise words like :optional
		 (arg-names (cond ((null? args)
				   ())
				  ((proper-list? args)                            ; handle (f x ...), (f (x 1) ...), (f . x), and (f x . z)
				   (map (lambda (a) 
					  (if (symbol? a) a (car a)))             ; omit the default values
					no-noise-args))                                
				  ((pair? args)
				   (append (butlast no-noise-args) (list :rest (last args))))
				  (else (list :rest args))))
		 (call-args (cond ((null? args)
				   ())
				  ((proper-list? args)
				   (if (memq :rest args)
				       (append (butlast (butlast no-noise-args))   ; also omit the :rest
					       (list (list 'apply-values (last args))))
				       arg-names))                                 ; (... y x)
				  ((pair? args)
				   (append (butlast no-noise-args)            ; (... y (apply-values x))
					   (list (list 'apply-values (last args)))))
				  (else (list (list 'apply-values args))))))     ; (... (apply-values x))
	    `(define ,func
	       (define-macro* ,(cons (gensym) args)                                ; args might be a symbol etc
		 `((lambda* ,(cons ',e ',arg-names)                                ; prepend added env arg because there might be a rest arg
		     (let ((,',result '?))
		       (dynamic-wind
			   (lambda ()                                              ; when function called, show args and caller
			     (with-let (funclet Display)                           ; indent
			       (prepend-spaces)
			       (set! spaces (+ spaces *display-spacing*)))
			     (format (Display-port) "(~A" ',',func)                ; show args, ruthlessly abbreviated
			     (((funclet Display) 'display-let) (outlet (outlet (curlet))) ,',e)
			     (format (Display-port) ")")
			     (let ((caller (eval '__func__ ,',e)))                 ; show caller 
			       (if (not (eq? caller #<undefined>))
				   (format (Display-port) " ;called from ~A" caller)))
			     (newline (Display-port)))
			   (lambda ()                                              ; the original function body
			     (set! ,',result ,',body))                             ;   but annotated by proc-walk
			   (lambda ()                                              ; at the end, show the result
			     (with-let (funclet Display)
			       (set! spaces (- spaces *display-spacing*))  ; unindent
			       (prepend-spaces))
			     (format (Display-port) "    -> ~S~%" ,',result)))))
		   (curlet) ,,@call-args)))))))                                    ; pass in the original args and the curlet
	
  
  (set! Display Display-1))                                                        ; make Display-1 globally accessible



;;; --------------------------------------------------------------------------------


(define (*s7*->list) 
  (list 
   :print-length                  (*s7* 'print-length)
   :safety                        (*s7* 'safety)
   :cpu-time                      (*s7* 'cpu-time)
   :heap-size                     (*s7* 'heap-size)
   :free-heap-size                (*s7* 'free-heap-size)
   :gc-freed                      (*s7* 'gc-freed)
   :gc-protected-objects          (*s7* 'gc-protected-objects)
   :gc-stats                      (*s7* 'gc-stats)
   :max-string-length             (*s7* 'max-string-length)
   :max-list-length               (*s7* 'max-list-length)
   :max-vector-length             (*s7* 'max-vector-length)
   :max-vector-dimensions         (*s7* 'max-vector-dimensions)
   :default-hash-table-length     (*s7* 'default-hash-table-length)
   :initial-string-port-length    (*s7* 'initial-string-port-length)
   :default-rationalize-error     (*s7* 'default-rationalize-error)
   :default-random-state          (*s7* 'default-random-state)
   :morally-equal-float-epsilon   (*s7* 'morally-equal-float-epsilon)
   :hash-table-float-epsilon      (*s7* 'hash-table-float-epsilon)
   :float-format-precision        (*s7* 'float-format-precision)
   :bignum-precision              (*s7* 'bignum-precision)
   :file-names                    (*s7* 'file-names)
   :rootlet-size                  (*s7* 'rootlet-size)
   :c-types                       (*s7* 'c-types)
   :stack-top                     (*s7* 'stack-top)
   :stack-size                    (*s7* 'stack-size)
   :stacktrace-defaults           (*s7* 'stacktrace-defaults)
   :max-stack-size                (*s7* 'max-stack-size)
   :symbol-table-locked?          (*s7* 'symbol-table-locked?)
   :autoloading?                  (*s7* 'autoloading?)
   :undefined-identifier-warnings (*s7* 'undefined-identifier-warnings)
   :catches                       (*s7* 'catches)
   :exits                         (*s7* 'exits)))



;;; --------------------------------------------------------------------------------

(define* (make-directory-iterator name (recursive #t))
  (if (not (string? name))
      (error 'wrong-type-arg "directory name should be a string: ~S" name)
      (make-iterator
       (with-let (sublet *libc* :name name :recursive recursive)
	 (let ((dir (opendir name)))
	   (if (equal? dir NULL)
	       (error 'io-error "can't open ~S: ~S" name (strerror (errno)))
	       (let ((iterator? #t))
		 (define reader
		   (let ((dirs ())
			 (dir-names ())
			 (dir-name name))
		     (lambda* (quit)                   ; returned from with-let
		       (if (eq? quit #<eof>)           ; caller requests cleanup and early exit
			   (begin                      ;   via ((iterator-sequence iter) #<eof>)
			     (closedir dir)
			     (for-each closedir dirs)
			     (set! dirs ())
			     quit)
			   (let ((file (read_dir dir)))
			     (if (zero? (length file)) ; null filename => all done
				 (begin
				   (closedir dir)
				   (if (null? dirs)
				       #<eof>
				       (begin          ; else pop back to outer dir
					 (set! dir (car dirs))
					 (set! dirs (cdr dirs))
					 (set! dir-name (car dir-names))
					 (set! dir-names (cdr dir-names))
					 (reader))))
				 (if (member file '("." "..") string=?)
				     (reader)
				     (let ((full-dir-name (string-append dir-name "/" file)))
				       (if (and recursive 
						(reader-cond 
						 ((defined? 'directory?)
						  (directory? full-dir-name))
						 (#t (let* ((buf (stat.make))
							    (result (and (stat full-dir-name buf) 
									 (S_ISDIR (stat.st_mode buf)))))
						       (free buf)
						       result))))
					   (let ((new-dir (opendir full-dir-name)))
					     (if (equal? new-dir NULL)  ; inner directory is unreadable?
						 (format *stderr* "can't read ~S: ~S" file (strerror (errno)))
						 (begin
						   (set! dirs (cons dir dirs))
						   (set! dir new-dir)
						   (set! dir-names (cons dir-name dir-names))
						   (set! dir-name full-dir-name)))
					     (reader))
					   (string-append dir-name "/" file)))))))))))))))))


;;; --------------------------------------------------------------------------------

(define null-environment
  (let ((e #f))
    (lambda ()
      (or e
	  (set! e (let ((lt (inlet)))
		    (for-each (lambda (c)
				(if (and (or (procedure? (cdr c))
					     (macro? (cdr c)))
					 (not (constant? (car c))))
				    (varlet lt (car c) (symbol "[" (symbol->string (car c)) "]"))))
			      (rootlet))
		    (sublet lt)))))))


(define sandbox 
  (let ((documentation "(sandbox code) evaluates code in an environment where nothing outside that code can be affected by its evaluation.")
	(built-ins 
	 (let ((ht (make-hash-table))) ; bad guys removed
	   (for-each
	    (lambda (op) 
	      (set! (ht op) #t))
	    '(symbol? gensym? keyword? let? openlet? iterator? constant? macro? c-pointer? c-object? 
	      input-port? output-port? eof-object? integer? number? real? complex? rational? random-state? 
	      char? string? list? pair? vector? float-vector? int-vector? byte-vector? hash-table? 
	      continuation? procedure? dilambda? boolean? float? proper-list? sequence? null? gensym 
	      symbol->string string->keyword symbol->keyword byte-vector-ref byte-vector-set!
	      inlet sublet coverlet openlet let-ref let-set! make-iterator iterate iterator-sequence
	      iterator-at-end? provided? provide c-pointer port-line-number port-filename 
	      pair-line-number pair-filename port-closed? let->list char-ready? flush-output-port 
	      open-input-string open-output-string get-output-string quasiquote call-with-values multiple-value-bind
	      newline write display read-char peek-char write-char write-string read-byte write-byte 
	      read-line read-string call-with-input-string with-input-from-string 
	      call-with-output-string with-output-to-string 
	      real-part imag-part numerator denominator even? odd? zero? positive? 
	      negative? infinite? nan? complex magnitude angle rationalize abs exp log sin cos tan asin 
	      acos atan sinh cosh tanh asinh acosh atanh sqrt expt floor ceiling truncate round lcm gcd
	      + - * / max min quotient remainder modulo = < > <= >= logior logxor logand lognot ash 
	      random-state random inexact->exact exact->inexact integer-length make-polar make-rectangular 
	      logbit? integer-decode-float exact? inexact? random-state->list number->string string->number 
	      char-upcase char-downcase char->integer integer->char char-upper-case? char-lower-case? 
	      char-alphabetic? char-numeric? char-whitespace? char=? char<? char>? char<=? char>=? 
	      char-position string-position make-string string-ref string-set! string=? string<? string>? 
	      string<=? string>=? char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=? string-ci=? string-ci<? 
	      string-ci>? string-ci<=? string-ci>=? string-copy string-fill! list->string string-length 
	      string->list string-downcase string-upcase string-append substring string object->string 
	      format cons car cdr set-car! set-cdr! caar cadr cdar cddr caaar caadr cadar cdaar caddr 
	      cdddr cdadr cddar caaaar caaadr caadar cadaar caaddr cadddr cadadr caddar cdaaar cdaadr 
	      cdadar cddaar cdaddr cddddr cddadr cdddar assoc member list list-ref list-set! list-tail 
	      make-list length copy fill! reverse reverse! sort! append assq assv memq memv vector-append 
	      list->vector vector-fill! vector-length vector->list vector-ref vector-set! vector-dimensions 
	      make-vector make-shared-vector vector float-vector make-float-vector float-vector-set! 
	      float-vector-ref int-vector make-int-vector int-vector-set! int-vector-ref string->byte-vector 
	      byte-vector make-byte-vector hash-table hash-table* make-hash-table hash-table-ref 
	      hash-table-set! hash-table-entries cyclic-sequences call/cc call-with-current-continuation 
	      call-with-exit apply for-each map dynamic-wind values 
	      catch throw error procedure-documentation procedure-signature help procedure-source
	      procedure-setter arity aritable? not eq? eqv? equal? morally-equal? s7-version
	      dilambda make-hook hook-functions stacktrace tree-leaves tree-memq object->let
	      pi most-positive-fixnum most-negative-fixnum nan.0 inf.0 -nan.0 -inf.0
	      *stderr* *stdout* *stdin*
	      apply-values list-values
	      quote if begin let let* letrec letrec* cond case or and do set! unless when else 
	      with-let with-baffle
	      lambda lambda* define define* 
	      define-macro define-macro* define-bacro define-bacro*))
	   ht))
	(baddies (list #_eval #_eval-string #_load #_autoload #_define-constant #_define-expansion #_require
		       #_string->symbol #_symbol->value #_symbol->dynamic-value #_symbol-table #_symbol #_keyword->symbol 
		       #_defined? #_symbol-access
		       #_call/cc #_gc #_read
		       #_open-output-file #_call-with-output-file #_with-output-to-file
		       #_open-input-file #_call-with-input-file #_with-input-from-file
		       #_current-output-port #_current-error-port #_current-input-port
		       #_set-current-output-port #_set-current-error-port #_set-current-input-port
		       #_varlet #_cutlet #_rootlet #_curlet #_owlet #_outlet #_funclet
		       #_exit #_emergency-exit
		       (reader-cond
			((provided? 'system-extras)
			 #_getenv #_system #_delete-file #_directory->list #_directory? #_file-exists? #_file-mtime))
		       )))
    (lambda (code)
      ;; block any change to calling program, or access to files, etc
      (let ((new-code 
	     (call-with-exit
	      (lambda (quit)
		(let walk ((tree code))
		  (cond ((symbol? tree)
			 (let ((val (symbol->value tree)))
			   ;; don't accept any symbol with an accessor
			   (if (or (symbol-access tree)
				   (memq tree '(*s7* unquote abort))
				   (let? val))  ; not sure about this
			       (quit #f))
			   ;; don't accept anything except safe built-ins and local vars
			   (if (not (or (hash-table-ref built-ins tree)
					(eq? (symbol->value tree (outlet (funclet sandbox))) #<undefined>)))
			       (quit #f))
			   ;; if value is also in rootlet, check that it's safe (protect against (set! abs exit) sometime earlier)
			   (if (or (procedure? val)
				   (macro? val))
			       (let ((unval (symbol->value tree (sublet (rootlet) (unlet))))) ; unlet returns the new unshadowing let
				 (if (not (eq? val unval))
				     (quit #f))))
			   tree))
			
			((memq tree baddies)  ; if tree is a bad procedure (probably via #_) quit
			 (quit #f))
			
			((not (pair? tree))
			 tree)
			
			(else
			 ;; do we need to check IO ports and set! here?
			 (cons (walk (car tree))
			       (walk (cdr tree))))))))))
	(and new-code
	     ;; make sure *s7* will not call any outside code upon error, clear out readers, etc
	     (let-temporarily ((*#readers* ())
			       (*libraries* ())
			       ((*s7* 'max-stack-size) 10000) ; block infinite recursion
			       ((*s7* 'autoloading?) #f)      ; turn off the autoloader
			       ((hook-functions *unbound-variable-hook*) ())
			       ((hook-functions *missing-close-paren-hook*) ())
			       ((hook-functions *load-hook*) ())
			       ((hook-functions *error-hook*) ())
			       ((hook-functions *read-error-hook*) ())
			       ((hook-functions *rootlet-redefinition-hook*) ())
			       ((current-output-port) *stdout*)
			       ((current-error-port) *stderr*))
	       (catch #t
		 (lambda ()
		   (eval new-code (sublet (rootlet) (unlet))))
		 (lambda args
		   (format #f "error: ~A"
			   (catch #t
			     (lambda ()
			       (apply format #f (cadr args)))
			     (lambda args
			       (copy "?"))))))))))))
