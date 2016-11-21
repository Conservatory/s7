(provide 'write.scm)

;;; -------------------------------- pretty-print --------------------------------

(define pretty-print

  (let ((*pretty-print-length* 100)
	(*pretty-print-spacing* 2)
	(*pretty-print-float-format* "~,4F")
	(*pretty-print-left-margin* 0))
    
    (define (any? f sequence) ; this and every? ought to be built-in!
      (member #f sequence (lambda (a b) (f b))))

    (lambda* (obj (port (current-output-port)) (column 0))

      (define pretty-print-1 
	(let ((newlines 0))

	  (define (spaces port n) 
	    (set! newlines (+ newlines 1))
	    (format port "~%~NC" (+ n *pretty-print-left-margin*) #\space))
	  
	  (define (stacked-list port lst col)
	    (do ((p lst (cdr p))
		 (added 0 0))
		((not (pair? p)))
	      (if (not (eq? p lst)) (spaces port col))
	      (let ((len (length (object->string (car p)))))
		(if (and (keyword? (car p))
			 (pair? (cdr p)))
		    (begin
		      (write (car p) port)
		      (write-char #\space port)
		      (set! added (+ 1 len))
		      (set! p (cdr p))))
		(if (not (pair? p))
		    (format port " . ~S" p)
		    (if (and (pair? (car p))
			     (pair? (cdar p))
			     (null? (cddar p))
			     (> len (/ *pretty-print-length* 2)))
			(begin
			  (if (eq? (caar p) 'quote)
			      (write-char #\' port)
			      (begin
				(write-char #\( port)
				(pretty-print-1 (caar p) port col)
				(spaces port (+ col 1))))
			  (pretty-print-1 (cadar p) port (+ col 1))
			  (if (not (eq? (caar p) 'quote))
			      (write-char #\) port)))
			(pretty-print-1 (car p) port (+ col added)))))))
	  
	  (define (stacked-split-list port lst col)
	    (if (not (pair? lst))
		(write lst port)
		(do ((p lst (cdr p)))
		    ((not (pair? p)))
		  (if (not (eq? p lst)) (spaces port col))
		  (write-char #\( port)
		  (if (pair? (car p))
		      (begin
			(write (caar p) port)
			(write-char #\space port)
			(if (and (pair? (cdar p))
				 (symbol? (caar p)))
			    (pretty-print-1 (cadar p) port (+ col (length (symbol->string (caar p))) 2))
			    (write (cdar p) port)))
		      (write (car p) port))
		  (write-char #\) port))))
	  
	  (define (messy-number z)
	    (if (real? z)
		(if (or (nan? z)
			(infinite? z))
		    (object->string z)
		    (if (= z pi)
			(copy "pi")
			(format #f *pretty-print-float-format* z)))
		(format #f "~A~A~Ai" 
			(messy-number (real-part z))
			(if (negative? (imag-part z)) "-" "+")
			(messy-number (abs (imag-part z))))))
	  
	  (define (any-keyword? lst)
	    (and (pair? lst)
		 (or (keyword? (car lst))
		     (any-keyword? (cdr lst)))))
	  
	  (let ((writers (let ((h (make-hash-table)))
			   
			   ;; -------- quote
			   (define (w-quote obj port column)
			     (if (not (pair? (cdr obj))) ; (quote) or (quote . 1)
				 (write obj port)
				 (begin
				   (write-char #\' port)
				   (pretty-print-1 (cadr obj) port column))))
			   (hash-table-set! h 'quote w-quote)
			   
			   ;; -------- define
			   (define (w-define obj port column)
			     (if (not (pair? (cdr obj)))
				 (write obj port)
				 (begin
				   (format port "(~A ~A " (car obj) (cadr obj))
				   (if (pair? (cadr obj))
				       (begin
					 (spaces port (+ column *pretty-print-spacing*))
					 (stacked-list port (cddr obj) (+ column *pretty-print-spacing*)))
				       (if (pair? (cddr obj))
					   (let ((str (object->string (caddr obj))))
					     (if (> (length str) 60)
						 (begin
						   (spaces port (+ column *pretty-print-spacing*))
						   (pretty-print-1 (caddr obj) port (+ column *pretty-print-spacing*)))
						 (write (caddr obj) port)))
					   (write (cddr obj) port)))
				   (write-char #\) port))))
			   (hash-table-set! h 'define w-define)
			   
			   ;; -------- if
			   (define (w-if obj port column)
			     (let ((objstr (object->string obj))
				   (ifcol (+ column 4)))
			       (if (< (length objstr) 40)
				   (display objstr port)
				   (begin
				     (format port "(if ")
				     (pretty-print-1 (cadr obj) port ifcol)
				     (when (pair? (cddr obj)) ; might be a messed-up if
				       (spaces port ifcol)
				       (pretty-print-1 (caddr obj) port ifcol)
				       (when (pair? (cdddr obj))
					 (spaces port ifcol)
					 (pretty-print-1 (cadddr obj) port ifcol)))
				     (write-char #\) port)))))
			   (hash-table-set! h 'if w-if)
			   
			   ;; -------- when unless
			   (define (w-when obj port column)
			     (let ((objstr (object->string obj)))
			       (if (< (length objstr) 40)
				   (display objstr port)
				   (begin
				     (format port "(~A " (car obj))
				     (pretty-print-1 (cadr obj) port (+ column (if (eq? (car obj) 'when) 6 8)))
				     (spaces port (+ column *pretty-print-spacing*))
				     (when (pair? (cddr obj))
				       (stacked-list port (cddr obj) (+ column *pretty-print-spacing*)))
				     (write-char #\) port)))))
			   (hash-table-set! h 'when w-when)
			   (hash-table-set! h 'unless w-when)
			   
			   ;; -------- let let* letrec letrec*
			   (define (w-let obj port column)
			     (if (not (and (pair? (cdr obj))
					   (pair? (cddr obj))))
				 (write obj port)
				 (begin
				   (let ((head-len (length (symbol->string (car obj)))))
				     (if (symbol? (cadr obj))
					 (begin
					   (format port "(~A ~A (" (car obj) (cadr obj))
					   (if (pair? (cddr obj))
					       (if (pair? (caddr obj)) ; (let x () ...)
						   (stacked-split-list port (caddr obj) (+ column head-len (length (symbol->string (cadr obj))) 4))
						   (if (not (null? (caddr obj)))
						       (write (caddr obj) port))) ; () is already being written
					       (if (not (null? (cddr obj)))
						   (format port " . ~S" (cddr obj)))))
					 (begin
					   (format port "(~A (" (car obj))
					   (if (pair? (cadr obj))
					       (stacked-split-list port (cadr obj) (+ column head-len 3))))))
				   (write-char #\) port)
				   (spaces port (+ column *pretty-print-spacing*))
				   (if (pair? ((if (symbol? (cadr obj)) cdddr cddr) obj))
				       (stacked-list port ((if (symbol? (cadr obj)) cdddr cddr) obj) (+ column *pretty-print-spacing*)))
				   (write-char #\) port))))
			   (for-each
			    (lambda (f)
			      (hash-table-set! h f w-let))
			    '(let let* letrec letrec*))
			   
			   ;; -------- set!
			   (define (w-set obj port column)
			     (let ((str (object->string obj)))
			       (if (<= (length str) 60)
				   (display str port)
				   (let ((settee (object->string (cadr obj))))
				     (format port "(set! ~A" settee)
				     (if (pair? (cddr obj))
					 (if (> (length settee) 20)
					     (begin
					       (spaces port (+ column 6))
					       (pretty-print-1 (caddr obj) port (+ column 6)))
					     (begin
					       (write-char #\space port)
					       (pretty-print-1 (caddr obj) port (+ column 7 (length settee))))))
				     (write-char #\) port)))))
			   (hash-table-set! h 'set! w-set)
			   
			   ;; -------- cond
			   (define (w-cond obj port column)
			     (format port "(cond ")
			     (do ((lst (cdr obj) (cdr lst)))
				 ((not (pair? lst)))
			       (if (not (eq? lst (cdr obj)))
				   (spaces port (+ column 6)))
			       (if (not (pair? (car lst)))
				   (write (car lst) port)
				   (let ((has=> (and (pair? (cdar lst))
						     (eq? (cadar lst) '=>))))
				     (let ((extras (and (pair? (cdar lst))
							(pair? (cddar lst))
							(or (not has=>)
							    (pair? (cdddar lst)))))
					   (too-long (> (length (object->string (cdar lst))) 50)))
				       (write-char #\( port)
				       (let ((oldlines newlines))
					 (pretty-print-1 (caar lst) port (+ column 7))
					 (if (or extras 
						 (not (= oldlines newlines))
						 too-long)
					     (spaces port (+ column 7))
					     (if (and (pair? (cdar lst))
						      (or (not has=>)
							  (= oldlines newlines)))
						 (write-char #\space port)))
					 (if (and (pair? (cdar lst))
						  (not extras)
						  (not too-long))
					     (begin
					       (write (cadar lst) port)
					       (when (and has=>
							  (pair? (cddar lst)))
						 (write-char #\space port)
						 (write (caddar lst) port)))
					     (if (not (null? (cdar lst)))
						 (stacked-list port (cdar lst) (+ column 7)))))
				       (write-char #\) port)))))
			     (write-char #\) port))
			   (hash-table-set! h 'cond w-cond)
			   
			   ;; -------- and or
			   (define (w-and obj port column)
			     (if (> (length (object->string obj)) 40)
				 (begin
				   (format port "(~A " (car obj))
				   (stacked-list port (cdr obj) (+ column *pretty-print-spacing* (length (symbol->string (car obj)))))
				   (write-char #\) port))
				 (write obj port)))
			   (hash-table-set! h 'and w-and)
			   (hash-table-set! h 'or w-and)
			   
			   ;; -------- case
			   (define (w-case obj port column)
			     (if (not (and (pair? (cdr obj))
					   (pair? (cddr obj))))
				 (write obj port)
				 (begin
				   (format port "(case ~A" (cadr obj)) ; send out the selector
				   (for-each 
				    (lambda (lst)
				      (spaces port (+ column *pretty-print-spacing*))
				      (if (not (pair? lst))
					  (write lst port)
					  (begin
					    (write-char #\( port)
					    (if (not (pair? (car lst)))
						(write (car lst) port)
						(let ((len (length (car lst))))
						  (if (< len 6)
						      (write (car lst) port)
						      (let ((p (car lst)))
							(write-char #\( port)
							(do ((i 0 (+ i 6)))
							    ((>= i len))
							  (do ((j 0 (+ j 1)))
							      ((or (= j 6) (null? p)) (if (pair? p) (spaces port (+ column 4))))
							    (write (car p) port)
							    (set! p (cdr p))
							    (if (pair? p)
								(write-char #\space port))))
							(write-char #\) port)))))
					    (if (not (null? (cdr lst)))
						(if (and (pair? (cdr lst))
							 (null? (cddr lst))
							 (< (length (object->string (cadr lst))) 60))
						    (begin
						      (write-char #\space port)
						      (write (cadr lst) port))
						    (begin
						      (spaces port (+ column 3))
						      (stacked-list port (cdr lst) (+ column 3)))))
					    (write-char #\) port))))
				    (cddr obj))
				   (write-char #\) port))))
			   (hash-table-set! h 'case w-case)
			   
			   ;; -------- map for-each
			   (define (w-map obj port column)
			     (let* ((objstr (object->string obj))
				    (strlen (length objstr)))
			       (if (< (+ column strlen) *pretty-print-length*)
				   (display objstr port)
				   (begin
				     (format port "(~A" (car obj))
				     (if (pair? (cdr obj))
					 (begin
					   (write-char #\space port)
					   (stacked-list port (cdr obj) (+ column *pretty-print-spacing*))))
				     (write-char #\) port)))))
			   (hash-table-set! h 'map w-map)
			   (hash-table-set! h 'for-each w-map)
			   
			   ;; -------- do
			   (define (w-do obj port column)
			     (if (not (pair? (cdr obj)))
				 (write obj port)
				 (begin
				   (format port "(do ")
				   (if (list? (cadr obj))
				       (write-char #\( port)
				       (display (cadr obj) port))
				   (if (pair? (cadr obj))
				       (stacked-list port (cadr obj) (+ column 5)))
				   (if (list? (cadr obj))
				       (write-char #\) port))
				   (when (pair? (cddr obj))
				     (spaces port (+ column 4))
				     (let ((end (caddr obj)))
				       (if (< (length (object->string end)) (- *pretty-print-length* column))
					   (write end port)
					   (begin
					     (write-char #\( port)
					     (pretty-print-1 (car end) port (+ column 4))
					     (spaces port (+ column 5))
					     (stacked-list port (cdr end) (+ column 5))
					     (write-char #\) port))))
				     (when (pair? (cdddr obj))
				       (spaces port (+ column *pretty-print-spacing*))
				       (stacked-list port (cdddr obj) (+ column *pretty-print-spacing*))))
				   (write-char #\) port))))
			   (hash-table-set! h 'do w-do)
			   
			   ;; -------- begin etc
			   (define (w-begin obj port column)
			     (format port "(~A" (car obj))
			     (if (pair? (cdr obj))
				 (begin
				   (spaces port (+ column *pretty-print-spacing*))
				   (stacked-list port (cdr obj) (+ column *pretty-print-spacing*))))
			     (write-char #\) port))
			   (for-each
			    (lambda (f)
			      (hash-table-set! h f w-begin))
			    '(begin call-with-exit call/cc call-with-current-continuation with-baffle with-output-to-string call-with-output-string))
			   
			   ;; -------- dynamic-wind call-with-values
			   (define (w-dynwind obj port column)
			     (format port "(~A" (car obj))
			     (spaces port (+ column *pretty-print-spacing*))
			     (stacked-list port (cdr obj) (+ column *pretty-print-spacing*))
			     (write-char #\) port))
			   (hash-table-set! h 'dynamic-wind w-dynwind)
			   (hash-table-set! h 'call-with-values w-dynwind)
			   
			   ;; -------- lambda etc
			   (define (w-lambda obj port column)
			     (if (not (and (pair? (cdr obj))
					   (pair? (cddr obj))))
				 (write obj port)
				 (begin
				   (format port "(~A ~A" (car obj) (cadr obj))
				   (spaces port (+ column *pretty-print-spacing*))
				   (stacked-list port (cddr obj) (+ column *pretty-print-spacing*))
				   (write-char #\) port))))
			   (for-each
			    (lambda (f)
			      (hash-table-set! h f w-lambda))
			    '(lambda lambda* define* define-macro define-macro* define-bacro define-bacro* with-let
				     call-with-input-string call-with-input-file call-with-output-file
				     with-input-from-file with-input-from-string with-output-to-file))
			   
			   ;; -------- defmacro defmacro*
			   (define (w-defmacro obj port column)
			     (if (not (and (pair? (cdr obj))
					   (pair? (cddr obj))))
				 (write obj port)
				 (begin
				   (format port "(~A ~A ~A" (car obj) (cadr obj) (caddr obj))
				   (spaces port (+ column *pretty-print-spacing*))
				   (stacked-list port (cdddr obj) (+ column *pretty-print-spacing*))
				   (write-char #\) port))))
			   (hash-table-set! h 'defmacro w-defmacro)
			   (hash-table-set! h 'defmacro* w-defmacro)
			   
			   ;; -------- catch
			   (define (w-catch obj port column)
			     (if (not (pair? (cdr obj))) ; (catch) or (catch . 1)
				 (write obj port)
				 (begin
				   (format port "(~A ~S" catch (cadr obj))
				   (spaces port (+ column *pretty-print-spacing*))
				   (stacked-list port (cddr obj) (+ column *pretty-print-spacing*))
				   (write-char #\) port))))
			   (hash-table-set! h 'catch w-catch)
			   
			   ;; -------- inlet
			   (define (w-inlet obj port column)
			     (format port "(inlet ")
			     (if (pair? (cdr obj))
				 (do ((lst (cdr obj) (cddr lst)))
				     ((not (and (pair? lst)
						(pair? (cdr lst)))))
				   (if (not (eq? lst (cdr obj)))
				       (spaces port (+ column *pretty-print-spacing*)))
				   (if (pair? (cdr lst))
				       (begin
					 (write (car lst) port)
					 (write-char #\space port)
					 (pretty-print-1 (cadr lst) port (+ column *pretty-print-spacing* (length (object->string (car lst))))))
				       (write lst port))))
			     (write-char #\) port))
			   (hash-table-set! h 'inlet w-inlet)

			   h)))
	
	    (lambda (obj port column) ; pretty-print-1
	    
	      (cond ((number? obj)
		     (if (rational? obj)
			 (write obj port)
			 (display (messy-number obj) port)))
		    
		    ((not (pair? obj))
		     (write obj port))

		    ((hash-table-ref writers (car obj)) => (lambda (f) (f obj port column)))

		    (else
		     (let* ((objstr (object->string obj))
			    (strlen (length objstr)))
		       (if (< (+ column strlen) *pretty-print-length*)
			   (display objstr port)
			   (let ((lstlen (length obj)))
			     (if (or (infinite? lstlen)
				     (< lstlen 2))
				 (display objstr port)
				 (if (and (pair? (car obj))
					  (memq (caar obj) '(lambda lambda* let let* letrec letrec* cond if case)))
				     (begin
				       (write-char #\( port)
				       (pretty-print-1 (car obj) port column)
				       (spaces port (+ column 1))
				       (if (and (memq (caar obj) '(cond if case))
						(do ((p (cdr obj) (cdr p)))
						    ((or (null? p)
							 (pair? (car p)))
						     (null? p))))
					   (do ((p (cdr obj) (cdr p)))
					       ((null? p))
					     (display (car p) port)
					     (if (pair? (cdr p))
						 (write-char #\space port)))
					   (stacked-list port (cdr obj) (+ column 1)))
				       (write-char #\) port))
				     (let* ((carstr (object->string (car obj)))
					    (carstrlen (length carstr)))
				       (if (eq? (car obj) 'quote)
					   (write-char #\' port)
					   (format port "(~A" carstr))
				       (if (any-keyword? (cdr obj))
					   (begin
					     (spaces port (+ column *pretty-print-spacing*))
					     (stacked-list port (cdr obj) (+ column *pretty-print-spacing*)))
					   (let ((line-start (+ column *pretty-print-spacing*
								(if (or (> carstrlen 16)
									(pair? (cadr obj)))
								    0 
								    carstrlen))))
					     (if (= lstlen 2)
						 (begin
						   (write-char #\space port)
						   (pretty-print-1 (cadr obj) port line-start))
						 (if (< lstlen 4)
						     (begin
						       (write-char #\space port)
						       (stacked-list port (cdr obj) line-start))
						     (do ((obj-start line-start)
							  (lst (cdr obj) (cdr lst)))
							 ((null? lst))
						       (let* ((str (object->string (car lst)))
							      (strlen1 (length str)))
							 (if (and (> strlen1 (- *pretty-print-length* obj-start))
								  (not (eq? lst (cdr obj))))
							     (begin
							       (set! obj-start (+ line-start 1 strlen1))
							       (spaces port line-start)
							       (pretty-print-1 (car lst) port line-start))
							     (begin
							       (set! obj-start (+ obj-start 1 strlen1))
							       (if (> strlen1 40)
								   (begin
								     (spaces port line-start)
								     (pretty-print-1 (car lst) port line-start))
								   (begin
								     (write-char #\space port)
								     (display str port)))))))))))
				       (if (not (eq? (car obj) 'quote))
					   (write-char #\) port))))))))))))))
      
      (let ((old-port port))
	(if (boolean? old-port)
	    (set! port (open-output-string)))
	(pretty-print-1 obj port column)
	(flush-output-port port)
	(if (not (boolean? old-port))
	    (values)
	    (let ((str (get-output-string port)))
	      (close-output-port port)
	      (if (eq? old-port #t)
		  (display str))
	      str))))))

(define (pp obj)
  (call-with-output-string
    (lambda (p)
      (pretty-print obj p))))

#|
(define (pretty-print-all)
  (let ((st (symbol-table)))
    (for-each
     (lambda (sym)
       (if (defined? sym)
	   (let ((val (symbol->value sym)))
	     (let ((source (and (procedure? val)
				(procedure-source val))))
	       (if (pair? source)
		   (format *stderr* "~<sym~> ~<val~>:~%~<(pp source)~>~%~%"))))))
     st)))

(define-macro (fully-macroexpand form)
  (define (expand form)
    ;; walk form looking for macros, expand any that are found -- see stuff.scm for a newer version
    (if (pair? form)
	(if (and (symbol? (car form))
		 (macro? (symbol->value (car form))))
	    (expand ((eval (procedure-source (symbol->value (car form)))) form))
	    (cons (expand (car form))
		  (expand (cdr form))))
	form))
  `(pretty-print ',(expand form)))

(define* (pp-sequence seq)
  (let ((iter (make-iterator seq))
	(strs ())
	(plen (*s7* 'print-length)))
    (do ((i 0 (+ i 1))
	 (entry (iterate iter) (iterate iter)))
	((or (= i plen)
	     (eof-object? entry))
	 (if (not (eof-object? entry))
	     (apply string-append (append (reverse! strs) (list "...")))
	     (apply string-append (reverse! strs))))
      (set! strs (cons (format #f "~S " entry) strs)))))
|#
