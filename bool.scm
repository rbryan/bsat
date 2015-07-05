
;define some useful macros
(define-syntax 1+
  (syntax-rules ()
	((1+ thing) (+ 1 thing))))

(define-syntax 1-
  (syntax-rules ()
	((1- thing) (- thing 1))))

(define-syntax define-operator
  (syntax-rules ()
	((define-operator name symbol) (define name
				    	(lambda (a b)
				      	`(,a symbol ,b))))))

;perform an operation at the n'th index of a list
(define-syntax at-nth
  (syntax-rules ()
	((at-nth operation) (lambda (n l)
			      (define check
				(lambda (n l)
				      (cond 	((< n 1) #f)
						((> n (length l)) #f)
						((= n 1) (operation l))
						((> n 1) (check (- n 1) (cdr l))))))
			      (check n l)))))

;define a macro that operates on
;a number and a list
;sorry it's not the most readable thing.
;I went to great lengths to avoid code duplication
(define-syntax def-n-l
 (syntax-rules ()
	((def-n-l name operation) 
		(define-syntax name
		   (syntax-rules (of)
			((name num of list) (operation num list))
			((name list) (name 1 of list))
			((name num list) (name num of list))
			((name of list) (name 1 of list)))))))


(def-n-l last (lambda (num ls)
		(begin
		    (define check
		      (lambda (n l)
			(cond ((= (length l) n) l)
			      ((> n (length l)) #f)
			      (else (check n (cdr l))))))
		    (check num ls))))
		
(def-n-l first
	 (lambda (num lst) 
	   (let ((l (last num of (reverse (quote lst)))))
	     (if l 
	       (reverse l)
	       #f))))

		   
(define-syntax rotate
  (syntax-rules (right left by)
	((rotate l right by n) (let ((n (modulo n (length l))))(append (last n of l) (first (- (length l) n) of l))))
	((rotate l by n) (rotate l right by n))
	((rotate l n) (rotate l right by n))
	((rotate l left by n) (let ((n (modulo n (length l))))(append (last (- (length l) n) of l) (first n of l))))))

;shift operators like i'm used to.
;why did I go to all that trouble writing those macros?
;I guess I learned.
(define <<
  (lambda (l n)
    (rotate l left by n)))

(define >>
  (lambda (l n)
    (rotate l right by n)))

(define rrot >>)
(define lrot <<)
(define rshift
  (lambda (l n)
    (append (build-list (lambda (n) 0) n '()) (cleave-right l (- (length l) n)))))

(define lshift
  (lambda (l n)
    (append (cleave-left l (1+ n)) (build-list (lambda (n) 0) n '()))))

;chop and throw of everything to the right
;of the cleaver (higher indices
(define cleave-left
  (lambda (l n)
    (if (< n (length l))
    (last (- (length l) (- n 1)) of l)
    #f)))


(define cleave-right
  (lambda (l n)
    (first n of l)))

;define a function that applies an operation
;between to bound indeces in a list inclusively
(define-syntax def-op-between
  (syntax-rules ()
	((def-op-between name op) (define name
				       (lambda (a b l n)
					 (let ((tail (cleave-left l (1+ b)))
					       (body (cleave-left (cleave-right l b) a))
					       (head (cleave-right l (1- a))))
					   (append head (op body n) tail)))))))

(def-op-between <<-between <<)
(def-op-between >>-between >>)

;Some useful functions
(define get-nth (at-nth car))
(define get-after (at-nth cdr))
(define tail
  (lambda (n l)
    (get-after (- (length l) n) l)))

;Lets define some operators
(define-operator _and ^)
(define-operator _or v)
(define-operator _xor x)

(define build-list
  (lambda (f n l)
    (cond ((> n 0) (build-list f (1- n) (append (list (f n)) l)))
	  (else l))))

(define bool->bit
  (lambda (v)
    (cond (v 1)
	  (else 0))))

(define bit->bool
  (lambda (v)
    (cond ((= v 1) #t)
	  (else #f))))

(define int32->bitlist
  (lambda (n)
    (reverse (build-list (lambda (i) (bit-set? n (1- i))) 32 '()))))

;returns (value . carry)
(define add-bits 
  (lambda (a b c)
    `(,(_xor a (_xor b c)) . ,(_or (_and a b) (_and c (_xor a b))))))

(define two-list-length-sort
  (lambda (a b)
    (cond ((> (length a) (length b)) `(,a . ,b))
	  (else `(,b . ,a)))))

(define even-length
  (lambda (first second)
    (let ((a (car (two-list-length-sort first second)))
	  (b (cdr (two-list-length-sort first second))))
      (cons a (append (build-list (lambda (n) 0) (- (length a) (length b)) '()) b)))))

;returns (carry . (bitlist))
;adds to equal length bit lists
;I wish this was more readable
(define add-bitlists
  (lambda (a b)
    (cond ((= (length a) 0) '(#f . ()))
	  (else (let* ((prev (add-bitlists (cdr a) (cdr b))) 
		       (vc (add-bits (car a) (car b) (car prev)))
		       (v  (car vc))
		       (c  (cdr vc)))
		  (cons c (append `(,v) (cdr prev))))))))

(define split
  (lambda (a b l)
    (let ((tail (cleave-left l (1+ b)))
	  (body (cleave-left (cleave-right l b) a))
	  (head (cleave-right l (1- a))))
      `(,head . (,body . (,tail))))))

(define get-between
  (lambda (a b l)
    (cadr (split a b l))))

(define nums->bits
  (lambda (l)
    (apply append (map int32->bitlist l))))

(define pow
  (lambda (a b)
    (cond ((> b 0) (* a (pow a (1- b))))
	  (else 1))))

(define tobools
  (lambda (l)
    (map bit->bool l)))

(define tobits
  (lambda (l)
    (map bool->bit l)))

(define bits->int32
  (lambda (l)
    (define work
	  (lambda (i l)
		  (cond ((= i 0) (* 1 (car l)))
			(else (+ (* (pow 2 i) (car l)) (work (1- i) (cdr l)))))))
    (work 31 l)))

(define chunk
  (lambda (n l)
    (if (< n (length l))
      (let ((head (cleave-right l n))
	    (rest (cleave-left l (1+ n))))
	(cons head (chunk n rest)))
      `(,l))))

(define-syntax define-list-op
  (syntax-rules ()
    ((define-list-op name op) (define name
				  (lambda (a b)
				    (cond ((eq? a '()) '())
					  (else `(,(op (car a) (car b)) . ,(name (cdr a) (cdr b))))))))))

(define-list-op and-list _and)
(define-list-op or-list _or)
(define-list-op xor-list _xor)

(define-syntax _not
  (syntax-rules ()
    ((_not a) (if (isbool a)
		(not a)
		`(~ ,a)))))
;
;apparently we can't use map on a macro
(define not-list
  (lambda (l)
    (define _not (lambda (ls) (_not ls)))
    (map _not l)))

(define isbool
  (lambda (b)
    (if (or (eq? b #t) (eq? b #f))
       #t
       #f)))


(define perm-length
  (lambda (a)
    (cond ((list? a) (length a))
	  (else 0))))

(define choose-shortest
  (lambda (a b)
    (cond ((> (perm-length a) (perm-length b)) b)
	  (else a))))

;best if a is a boolean
;try both ways and pick simplest
(define eval-xor
  (lambda (a b)
    (cond ((eq? #f a) b)
	  ((eq? #t a) (_not b))
	  (else	(_xor a b)))))
(define eval-or
  (lambda (a b)
    (cond ((eq? #f a) b)
	  ((eq? #t a) #t)
	  (else (_or a b)))))
(define eval-and
  (lambda (a b)
    (cond ((eq? a #t) b)
	  ((eq? a #f) #f)
	  (else (_and a b)))))

;try to evaluate a function both ways
;and pick the simplest outcome
(define try-eval
  (lambda (f a b)
    (let ((first-try (f a b))
	  (second-try (f b a)))
	(choose-shortest first-try second-try))))
;      (cond ((isbool second-try) second-try)
;	    ((isbool first-try) first-try)
;	    (else first-try)))))

(define simplify-expr
  (lambda (expr)
    (if (eq? (car expr) '~)

      (_not (simplify-expr (cdr expr)))

      (let ((a (car expr))
  	  (op (cadr expr))
  	  (b (cddr expr)))
        (cond ((eq? op '^) (try-eval eval-and a b))
	      ((eq? op 'v) (try-eval eval-or a b))
	      ((eq? op 'x) (try-eval eval-xor a b)))))))



;retrieve a bitlist representing the
;n'th 32bit int in a bitlist
(define get-nth-int
  (lambda (n l)
	  (let* ((istart (1+ (* 32 (1- n))))
		(iend (+ istart 31)))
		  (cadr (split istart iend l))))) 

;;Everything that follows is the boolean implementation of sha256

(define num-input-variables 640)



(define m
  (build-list (lambda (n) n) num-input-variables '()))

(define sha256
  (lambda (message)

	(define h '(
		 #xbb67ae85
		 #x3c6ef372
		 #xa54ff53a
		 #x510e527f
		 #x9b05688c
		 #x1f83d9ab
		 #x5be0cd19))

	(set! h (foldr append '() (map int32->bitlist h)))

	(define k '(
		    #x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
		    #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
		    #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
		    #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
		    #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
		    #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
		    #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
		    #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2
					 ))

	(set! k (foldr append '() (map int32->bitlist k)))

	;Append the bit '1' to the message
	(set! message (append message '(#t)))

	;append k bits '0', where k is the minimum number >= 0 such that the resulting message
	;    length (modulo 512 in bits) is 448.
	(define mkmod512=448
	  (lambda ()
		  (if (= (modulo (length message) 512) 448)
		      #t
		      (begin
			(set! message (append message '(#f)))
			(mkmod512=448)))))
	(mkmod512=448)

	;append length of message (without the '1' bit or padding), in bits, 
	;as 64-bit big-endian integer
	;(this will make the entire post-processed length a multiple of 512 bits)
	(set! message (append message (append (int32->bitlist 0) (int32->bitlist (length message)))))

	;break the message into 512 bit chunks
	(define chunks (chunk 512 message))
	
	(define chunk-work
	  (lambda (chunk)

	    ;create message schedule array
	    (define message-schedule chunk)
	    
	    ;extend the message schedule to 64 words
	    (define extend-ms
	      (lambda (ms i)
		(if (< i 65)
		  (let* ((i-14 (get-nth-int (- i 14) ms))
			 (i-1  (get-nth-int (- i 1)  ms))
			 (i-15 (get-nth-int (- i 15) ms))
			 (i-6  (get-nth-int (- i 6)  ms))
			 (s0   (foldr xor-list (rshift i-14 3) ((rrot i-14 7) (rrot i-14 18))))
			 (s1   (foldr xor-list (rshift i-1 10) ((rrot i-1 17) (rrot i-1 19)))))
		    (set! message-schedule (append ms (foldr (lambda (a b) (cdr (add-bitlists a b))) s1 '(i-15 s0 i-6))))
		    (extend-ms message-schedule (1+ i))))))
	    (extend-ms message-schedule 17)

	    ;initialize working variables to current
	    ;hash value
	    (define wa (get-nth-int 1 h))
	    (define wb (get-nth-int 2 h))
	    (define wc (get-nth-int 3 h))
	    (define wd (get-nth-int 4 h))
	    (define we (get-nth-int 5 h))
	    (define wf (get-nth-int 6 h))
	    (define wg (get-nth-int 7 h))
	    (define wh (get-nth-int 8 h))

	    (do ((i 1 (1+ i)))
		 ((= i 65))
		 (define s1 (xor-list (rrot we 6) (xor-list (rrot we 11) (rrot we 25))))
		 (define ch (xor-list (and-list we wf) (and-list (not-list we) wg)))
		 (define temp1 (map (lambda (a b) (cdr (add-bitlists a b))) (list wh s1 ch (get-nth-int i k) (get-nth-int i message-schedule))))
		 (define s0 (xor-list (rrot wa 2) (xor-list (rrot wa 13) (rrot wa 22))))
		 (define maj (xor-list (and-list wa wb) (xor-list (and-list wa wc) (and-list wb wc))))
		 (define temp2 (cdr (add-bitlists s0 maj)))

		 (set! wh wg)
		 (set! wg wf)
		 (set! wf we)
		 (set! we (cdr (add-bitlists wd temp1)))
		 (set! wd wc)
		 (set! wc wb)
		 (set! wb wa)
		 (set! wa (cdr (add-bitlists temp1 temp2))))

	    (set! h (map (lambda (a b) (add-bitlists (get-nth-int a h) b)) '(1 2 3 4 5 6 7 8) (list wa wb wc wd we wf wg wh)))

	    ))
	(map chunk-work chunks)
	  h
	
	))

