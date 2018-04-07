; Progetto Prolog e Common Lisp 2018-01-15  (E1P)
; -- json-parsing.lisp --
;

; -- SEZIONE PARSING --

; Funzione json-parse

(defun json-parse (string)
  (check-json (map 'list #'identity string)))

(defun check-json (l)
	(cond 	
	((null  l) (error "syntax error"))
	((equal (first l) #\{) (parse-object l))
	((equal (first l) #\[) (parse-array l))
	((null  l) (error "No Json Object passed"))))		


(defun parse-array (l)
  (cond 	
   ((null  l) (error "Bad Array or bad or unsupported JSON sintax."))
   ((equal (first l) #\Space) (parse-array (cdr l)))
   ((equal (car (last l)) #\Space) (parse-array (butlast l)))
   ((equal (car (last l)) #\}) (error "syntax error"))
   ((equal (second l) #\]) (append (list 'json-array)))
   ((null l) (error "No array element found!"))
   ((json-array l) 
    (append (list 'json-array) (append (parse-elements l))))
   (t (error "No correct or supported Array passed"))))

(defun json-array (l)
  (cond 
   ((null l)(error "Bad Array or bad or unsupported JSON sintax."))
   ((or (null (position #\[ l))	(null (position #\] l))) nil)
   ((null (json-space (subseq l 0 (position #\[ l)))) nil)
   ((null (json-space (subseq l (1+(position #\] l))))) nil)
   (t t)))


(defun parse-object (l)
  (cond
   ((null l) (error "Bad Object or bad or unsupported JSON sintax."))
   ((equal (first l) #\Space) (parse-object (cdr l)))
   ((equal (car (last l)) #\Space) (parse-object (butlast l)))
   ((equal (car (last l)) #\]) (error "syntax error"))
   ((equal (second l) #\}) (append (list 'json-obj)))
   ((null l) nil)
   ((json-object l)
    (append (list 'json-obj) (append (parse-member (nthcdr 1 (butlast l)) 0))))
   ((null (nthcdr 1 (butlast l))) (append (list 'json-obj)))
   (t (print l) (error "Bad or unsupported JSON sintax."))))

(defun json-object (l)
  (cond
   ((and
     (equal (first l) #\{)
     (equal (car (last l)) #\}))
    (json-member (nthcdr 1 (butlast l)) 0))
   ((equal (first l) #\Space) (json-object (cdr l)))
   ((equal (car (last l)) #\Space) (json-object (butlast l)))))


(defun parse-member (l p)
  (cond 	
   ((null  l) (error "Bad Member or bad or unsupported JSON sintax."))
   ((equal (first l) #\Space) (parse-member (cdr l) 0))
   ((equal (car (last l)) #\Space) (parse-member (butlast l) 0))
   ((null (position #\, l :start p)) 
    (parse-pair l 0))
   ((json-pair(subseq l 0 (position #\, l :start p)) 0)
    (append (parse-pair(subseq l 0 
			       (position #\, l :start p))
		       0)
	    (parse-member(subseq l (1+ (position #\, l :start p))) 0)))
   (t (parse-member l (1+ (position #\, l :start p))))))

(defun json-member (l p)
  (cond
   ((null (position #\, l :start p)) (json-pair l 0)) 
   ((null l) t)
   (t (json-pair (subseq l 0 (position #\, l :start p)) 0)
      (json-member (subseq l (1+ (position #\, l :start p))) 0))))


(defun parse-elements (l)
  (cond 
  ((or (null (position #\[ l))(null (position #\] l))) nil)
  ((null (json-space (subseq l 0 (position #\[ l)))) nil)
  ((null (json-space (subseq l (1+ (position #\] l))))) nil)
  (t (json-elements (subseq l (1+ (position #\[ l))(position #\] l))))))

(defun json-elements (l)
  (cond  
   ((null  l) nil)
   ((null (position #\, l)) (append (json-element l)))
   ((null (position #\, l :start (1+ (position #\, l))))
    (append (json-element (subseq l 0 (position #\, l)))
	    (json-element (subseq l (1+ (position #\, l))))))
   (t (append (json-element (subseq l 0 (position #\, l)))
	      (json-elements (subseq l (1+ (position #\, l))))))))

(defun json-element (l)
  (cond	((null l) t)
	((equal #\Space (first l)) (json-element (cdr l)))
	((equal #\Space (car (last l)))	(json-element (butlast l)))
        ((json-value l) (parse-value l))
	(t (error "Unknown element or bad JSON sintax."))))


(defun parse-pair(l p)
  (cond 	
   ((null  l) 	(error "Bad Pair or bad or unsupported JSON sintax."))
   ((null(position #\: l :start p)) (error "Bad pair or bad JSON sintax."))
   ((and (json-string (subseq l 0 (position #\: l :start p))0)
	 (json-value (subseq l (1+ (position #\: l :start p)))))
    (list (append (parse-string (subseq l 0 (position #\: l :start p)))
		  (parse-value (subseq l (+ 1 (position #\: l :start p)))))))
   (t (parse-pair l (1+ (position #\: l :start p))))))

(defun json-pair (l p)
  (cond 	
   ((null l) nil)
   ((equal (first l) #\Space) (json-pair (cdr l) 0))
   ((equal (car (last l)) #\Space) (json-pair (butlast l) 0))
   ((null (position #\: l :start p)) nil)	
   ((and
     (json-string(subseq l 0 (position #\: l)) 0)
     (json-value(subseq l (1+ (position #\: l)))))
    t)
   ((null (position #\: l :start 
		    (1+ (position #\: l :start p))))
    nil)
   (t (json-pair l (position #\: l :start 
			     (1+ (position #\: l :start p)))))))


(defun parse-string (l)
  (cond 
   ((null  l) (error "Bad String or bad or unsupported JSON sintax."))
   ((equal (first l) #\Space) (parse-string (cdr l)))
   ((equal (car (last l)) #\Space) (parse-string (butlast l)))
   ((json-string l 0) (list (concatenate 'string (butlast (cdr l)))))
   (t (print l) (error "Bad string or sintax."))))

(defun json-string (l p)
  (cond	
   ((null  l) 	(error "Empty String or bad or unsupported JSON sintax."))
   ((null (position #\" l)) nil)
   ((null (position #\" l :start
		    (1+ (position #\" l :start p)))) 
    (error "Bad String or bad or unsupported JSON sintax."))
   ((and
     (json-space (subseq l 0 (position #\" l)))
     (json-chars (subseq l (1+ (position #\" l))
		    (1+ (position #\" l :start
		    (1+ (position #\" l :start p)))))))
    (cond
     ((equal (length (subseq l 0
		    (1+ (position #\" l :start
		    (1+ (position #\" l :start p))))))
	     (length l))
      t)
     ((and 
       (json-space(subseq l 
		     (1+ (position #\" l :start 
		     (1+ (position #\" l :start p))))))
       (not(equal (length (subseq l 0 
		     (1+ (position #\" l :start 
		     (1+ (position #\" l :start p))))))
		  (length l))))
      t)))
   (t (json-string l (position #\" l 
		     :start (1+ (position #\" l :start p)))))))

(defun json-space (l)
  (cond
   ((null l) t)
   ((not (equal (car l) #\Space)) nil)
   (t (json-space (cdr l)))))

(defun json-chars (l)
  (cond
   ((null l) nil)
   ((and 
     (equal (car l) #\\ ) (null (cdr l))) nil)
   ((and (equal (car l) #\\) (not (null (cdr l))))
    (json-chars(cddr l)))
   ((equal (car l) #\") t) (t (json-chars(cdr l)))))


(defun parse-value (l)
  (cond
   ((null  l) (error "Empty value or bad or unsupported JSON sintax."))
   ((equal (first l) #\Space) (parse-value (cdr l)))
   ((equal (car (last l)) #\Space) (parse-value (butlast l)))
   ((json-string l 0) (list (concatenate 'string (butlast (cdr l)))))
   ((json-value l) (list (parse-integer (concatenate 'string l))))
   (t (error "Bad or unsupported Array."))))

(defun json-value (l)
  (cond 	
   ((null  l)(error "Empty value or bad or unsupported JSON sintax."))
   ((json-string l 0) t)
   ((and (numberp (car l))(null (cdr l))) t)
   ((and (numberp (read-from-string (concatenate 'string l)))
	 (null (cdr l)))
    t)
   ((numberp (read-from-string (concatenate 'string l))) t)
   ((json-array  l) t)
   (t (print l) (error "Bad or unsupported JSON sintax."))))

  
; Funzione json-get

(defun json-get(json &rest path)
  (find-element json path))

(defun find-element (list elements)
  (if (not (null elements))
      (let ((element (car elements)))
        (if (numberp element)
            (find-element (nth (1+ element) list)
			     (cdr elements))       
          (find-element (nth 1 (find-pair list element)) 
			   (cdr elements)))) list))

(defun find-pair(list pairname)
  (if (not (null list))
      (if (equal (nth 0 (car (cdr list))) pairname)
          (car (cdr list))
        (find-pair (cdr list) pairname))))


;  -- SEZIONE I/O --

; Funzione json-load

(defun json-load (filename)
  (with-open-file (in filename :direction :input
		      :if-does-not-exist :error)
    (json-parse (coerce (readfile in) 'string))))

(defun readfile (pass-input)
  (let ((e (read-char pass-input nil 'eof)))
    (unless (eq e 'eof)
      (cons e (readfile pass-input)))))


; Funzione json-write

(defun json-write (json-obj filename)
  (with-open-file (out filename :direction :output 
		                :if-exists :supersede
		                :if-does-not-exist :create)
   (format out (write (js-wr json-obj)))
      filename))

(defun js-wr (json-obj)
  (cond ((equal (car json-obj) 'json-array) (format nil (csa json-obj)))
        ((equal (car json-obj) 'json-obj) (format nil (cso json-obj)))
        (t "error")))

(defun csa (json-obj)
  (cond ((and (equal (car json-obj) 'json-array)(equal (cdr json-obj) nil)) 
         (format nil "[]"))
        ((and (equal (car json-obj) #\Space) (not (equal (cdr json-obj) nil)))
	 (csa (cdr json-obj)))
        ((equal (car json-obj) 'json-array) 
         (format nil (concatenate 'string "[" (csa (cdr json-obj)))))
        ((equal (car json-obj) 'json-obj) (cso json-obj))
        ((and (not (atom (car json-obj))) (equal (cdr json-obj) nil))
	 (concatenate 'string (csa (car json-obj)) "]"))
        ((and (not (atom (car json-obj))) (not (atom (cdr json-obj))))
	 (concatenate 'string (csa (car json-obj)) "," (csa (cdr json-obj))))
        ((and (not (atom (car json-obj))) (atom (cdr json-obj)))
	 (concatenate 'string (csa (car json-obj)) "," (cdr json-obj)))
        ((and (numberp (car json-obj)) (not (equal (cdr json-obj) nil)))
	 (format nil (concatenate 'string (write-to-string(car json-obj))
				  "," (csa (cdr json-obj))))) 
        ((and (stringp (car json-obj)) (not (equal (cdr json-obj) nil)))
         (format nil (concatenate 'string "\"" (car json-obj) "\"" ","
				  (csa (cdr json-obj)))))  
        ((and (numberp (car json-obj)) (equal (cdr json-obj) nil))
	 (format nil (concatenate 'string (write-to-string(car json-obj)) "]")))
        ((and (stringp (car json-obj)) (equal (cdr json-obj) nil))
	 (format nil (concatenate 'string "\"" (car json-obj) "\"" "]")))
        (t "Errore scrittura Array")))        

(defun cso (json-obj)
  (cond ((and (equal (car json-obj) 'json-obj) (equal (cdr json-obj) nil))
	 (format nil "{}"))
        ((and (equal (car json-obj) #\Space) (not (equal (cdr json-obj) nil))) 
         (cso (cdr json-obj)))
        ((and (equal (car json-obj) 'json-obj))
         (format nil (concatenate 'string "{" (cso (cdr json-obj)))))
        ((and (not (atom (car json-obj))) (equal (cdr json-obj) nil))
         (format nil (concatenate 'string (cso (car json-obj)) "}")))
        ((and (not (atom (car json-obj))) (not (atom (cdr json-obj))))
         (format nil (concatenate 'string (cso (car json-obj)) ","
				  (cso (cdr json-obj)))))
        ((and (atom (car json-obj)) (stringp (verifica-cdr (cdr json-obj))))
	 (cdr json-obj)
         (format nil (concatenate 'string "\"" (car json-obj) "\"" ":" "\""
				  (verifica-cdr (cdr json-obj)) "\"")))
        ((and (atom (car json-obj)) (numberp (verifica-cdr (cdr json-obj))))
         (format nil (concatenate 'string "\""
				  (car json-obj) "\"" ":" (write-to-string
							   (verifica-cdr
							    (cdr json-obj))))))
        ((and (atom (car json-obj)) (not (atom (cdr json-obj))))
         (format nil (concatenate 'string "\"" (car json-obj) "\"" ":"
				  (verifica-cdr (cdr json-obj)))))
        (t "Errore scrittura oggetto")))

      
(defun verifica-cdr (json-obj)
  (cond ((stringp (car json-obj)) (car json-obj))
        ((numberp (car json-obj)) (car json-obj))
        ((equal (car (car json-obj)) 'json-array) (csa (car json-obj)))
        ((equal (car (car json-obj)) 'json-obj) (cso (car json-obj))) 
        (t "Errore verifica cdr")))

; -- end-of-file --
; -- json-parsing.lisp --
