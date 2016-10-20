

;; Return the radix of input string
(defun get-radix (input-string)
  (cond
    ((null input-string)
    nil)
    ((numberp input-string) nil)
    ((eq #\- (car (coerce (string input-string) 'list))) 
      (get-radix (coerce (cdr (coerce (string input-string) 'list)) 'string)))
    ((symbolp input-string ) 
      ;; current element is a symbol convert it to string
      ;; if the input is a letter (x ,y) return it
      (if (not (digit-char-p (car (coerce (string input-string) 'list))))

        (if (= 1 (length (coerce (string input-string) 'list)))
         (list (car (coerce (string input-string) 'list))) 
         (get-radix (string input-string)))
        (get-radix (string input-string))
      )
    )
    ((not(digit-char-p (car (coerce input-string 'list)))) 
     ;; add "1" to the input to make "X" become "1X", which might
     ;; be the exception when coefficient is missing
      (get-radix (concatenate 'string "1" input-string))    
    )
    ((digit-char-p (car (coerce input-string 'list)))

      (if (not (digit-char-p (cadr (coerce input-string 'list))))

        (cdr (coerce input-string 'list))
        (get-radix (coerce(cdr (coerce input-string 'list)) 'string) )
      )
    )
  )
)
;;return the coefficient of input string
(defun get-coefficient (coefficient input-string)

  (cond
    ((null input-string)
    (if (string= "-" (car (coerce input-string 'list)))
     (* -1 (parse-integer (concatenate 'string (cdr coefficient))))
    (parse-integer (concatenate 'string coefficient))))

    ((string= "-" (car (coerce input-string 'list)))
      (get-coefficient (append coefficient (list 
        (car (coerce input-string 'list)))) 
      (cdr (coerce input-string 'list)))
    )
    ((digit-char-p (car (coerce input-string 'list)))
      (get-coefficient (append coefficient 
        (list (car (coerce input-string 'list)))) 
      (cdr (coerce input-string 'list)))
    )
    ((not (digit-char-p (car (coerce input-string 'list))))
      (cond
        ((eq coefficient '()) 1)
        (t (parse-integer (concatenate 'string coefficient)))
      )
    )
    (t (get-coefficient coefficient (cdr input-string)))
  )
)
;; Return the list with same radix
(defun matching (match match-list) 
  (cond
    ((null match-list)
    nil
    match
    )
    ((null match)
    nil)
    ((stringp match) (matching (intern match) match-list))
    ((symbolp match)
      (matching (list (string match)) match-list)
    );; keep operands in the list
    ((or (string= "-"  (string (car match-list)))     
        (string= "*"  (string (car match-list)))
        (string= "+"  (string (car match-list))))
      (if (equal(get-radix (car match)) (get-radix (cadr match-list)))
        (matching (append  match (list 
          (string (car match-list)))) (cdr match-list))
        (matching match (cdr match-list))
      )
    )
    ((equal(get-radix (car match)) (get-radix (car match-list))) 
     ;;When target is matched, keep the matched element in the list
      (matching (append  match (list 
        (string (car match-list)))) (cdr match-list))
    )
    (t
      (matching match (cdr match-list))

    )
  )
)
;; Compute the addition and subtraction of coefficient
(defun cal-coefficient (matched-list)
  (cond
    ((or (symbolp matched-list) 
      (stringp matched-list)) (cal-coefficient (list matched-list)))
    ((= 1 (length matched-list))
      matched-list
    )
    ((null matched-list)
    nil)
    ((or (string= "-"  (car matched-list))
        (string= "*"  (car matched-list))
        (string= "+"  (car matched-list)))
        (concatenate 'string (string (+ (get-coefficient '() 
          (car matched-list)) (get-coefficient '()(cadr matched-list))))
           (get-radix (car matched-list)))
    )
    ((string= "+"  (cadr matched-list))
      (cal-coefficient (append (list (concatenate 'string (write-to-string
       (+ (get-coefficient '() (car matched-list)) 
        (get-coefficient '() (caddr matched-list)))) 
      (get-radix (car matched-list))))
                    (cdddr matched-list)))
    )
    ((string= "-"  (cadr matched-list))
      (cal-coefficient (append (list (concatenate 'string 
        (write-to-string (- (get-coefficient '() 
        (car matched-list)) (get-coefficient '() 
        (caddr matched-list)))) (get-radix (car matched-list))))
                    (cdddr matched-list)))
    )
  )
)

;; Replace - with + (negative number)
(defun negative-number (input-list result) 
  (cond
    ((null input-list) result)
    ((eq '- (car input-list))
      (negative-number (cddr input-list) (append result (list '+) 
        (list (intern(concatenate 'string "-" (string (cadr input-list)))))))
    )
    (t (negative-number (cdr input-list) 
      (append result (list (car input-list)))))



  )
)
;; When operands showed up in sequence, work out the actuall operand
(defun multiple-operand (result output)

  (cond
    ((null result)
    output)
    ((eq nil (cadr result))
      (if (or (string= "+" (car result)) 
        (string= "-" (car result)) (string= "*" (car result)))
      (multiple-operand (cdr result) output)
      (multiple-operand (cdr result) (append output (list (car result))))
      )
    )
    ( (or (string= "+" (car result)) 
      (string= "-" (car result)) (string= "*" (car result)))
      (cond
        ((and (not (eq nil (caddr result))) 
          (not(or (string= "+" (caddr result)) 
            (string= "-" (caddr result)) (string= "*" (caddr result)))))
          (multiple-operand (cddr result)
           (append output (list (cadr result))))
        )
        ( (or (string= "+" (cadr result)) 
          (string= "-" (cadr result)) (string= "*" (cadr result)))
          (multiple-operand (cdr result) output)
        )
        (t (multiple-operand (cdr result) 
          (append output (list (car result))))
        )
      )
    )

    (t
      (multiple-operand (cdr result) (append output (list (car result))))
    )
  )
)
;; if multiple "+" presented in series, merge them
(defun merge-operand (result output)

  (cond
    ((null result)
    output)
    ((or (and (string= (car result) (cadr result)) (string= "+" (car result)))
    (and (string= "+" (car result)) (string= "-" (cadr result))))
      (merge-operand (cdr result) output)
    )
    ((eq nil (cadr result))
      (if (or (string= "+" (car result)) 
        (string= "-" (car result)) (string= "*" (car result)))
      (merge-operand (cdr result) output)
      (merge-operand (cdr result) (append output (list (car result))))
      )
    )
    (t
      (merge-operand (cdr result) (append output (list (car result))))
    )
  )
)
;;if multiple "-" presented in series, combine them to form "+"

(defun combine-operand (result output)

  (cond
    ((null result)
    (merge-operand output nil))
    ((and (string= "-" (car result)) (string= "+" (cadr result)))
      (combine-operand (cddr result) (append output (list (car result))))
    )
    ((string= "-" (car result))
      (if (string= "-" (cadr result)) 
        (combine-operand (cddr result) (append output (list "+")))
      (combine-operand (cdr result) (append output (list (car result)))))
    )
    ((eq nil (cadr result))
      (if (or (string= "+" (car result)) 
        (string= "-" (car result)) (string= "*" (car result)))
      (combine-operand (cdr result) output)
      (combine-operand (cdr result) (append output (list (car result))))
      )
    )
    (t (combine-operand (cdr result) (append output (list (car result))))
    )
  )
)

;; Remove any integer in the list with their sign
(defun remove-integer (groups output)
  (cond
    ((null groups)
    output)
    ((or (equal '+ (car groups))
     (equal '- (car groups)) (equal '* (car groups)))
      (cond
        ((numberp (cadr groups)) (remove-integer (cddr groups) output))
        ((numberp (read-from-string (string(cadr groups))))
         (remove-integer (cddr groups) output))
        (t (remove-integer (cdr groups) (append output (list (car groups)))))
      )
    )
    (t
      (remove-integer (cdr groups) (append output (list (car groups))))
    )
  )
)

;; Return the list of integer from a list of polynomial
(defun get-integer (groups output)

  (cond
    ((null groups)
    output)
    ((or (equal '+ (car groups)) 
      (equal '- (car groups)) (equal '* (car groups)))
      (cond
        ((numberp (cadr groups)) (get-integer (cdr groups) 
          (append output (list (car groups)))))
        ((numberp (read-from-string (string(cadr groups))))
         (get-integer (cdr groups) (append output (list (car groups)))))
        (t (get-integer (cdr groups) output))
      )
    )
    ((numberp (car groups))
      (get-integer (cdr groups) (append output (list (car groups))))
    )
    ((numberp (read-from-string (string(car groups))))
      (get-integer (cdr groups) 
        (append output (list (parse-integer(car groups)))))
    )
    (t
      (get-integer (cdr groups) output)
    )
  )
)
;;
(defun cal-integer (int-list result)
  (cond
    ((null int-list) result)
    ((eq '+ (car int-list)) (cal-integer 
      (cdr int-list) (+ result (cadr int-list))))
    ((eq '- (car int-list)) (cal-integer 
      (cdr int-list) (- result (cadr int-list))))
    (t (cal-integer (cdr int-list) result))
  )

)
;; return list with '+ as the first element
(defun add-postive-sign (groups)
  (cond
    ((null groups) groups)
    ((or (eq '- (car groups))(eq '+ (car groups))) groups)
    (t (cons '+ groups))
  )
)
;; raise the power of the kind (xx yy) to (x^2 y^2)
(defun raise-power (input-list)
  (if (= 1(length input-list)) (car input-list)
    (concatenate 'string 
      (car input-list) "^" (write-to-string(length input-list))))

)
;; raise the power of a radix eg. xyx will become x^2y
(defun match-power (existed result radix)


  (cond
    ((null radix) (combine-string result nil))
    ((or (string= "-" (car radix))
     (numberp (parse-integer (car radix):junk-allowed t)))
      (match-power existed 
        (append result (list (car radix))) (cdr radix)))
    ((not(member (car radix) existed :test #'equal))
      (if (= 1 (length radix)) (match-power 
        (append existed radix) (append result radix) (cdr radix))
      (match-power (append existed (list (car radix)))
       (append result (list (raise-power (matching 
        (car radix) (cdr radix))))) (cdr radix))
      )
    )
    (t
      (match-power existed result (cdr radix))
    )
  )
)
;; Return the string with same sum of ascii of char
(defun match-ascii (match match-list)
  (cond
    ((null match-list)

    (fix-order match nil (car match)))

    ((null match)
    nil)
    ((stringp match) (match-ascii (intern match) match-list))
    ((symbolp match)
      (match-ascii (list (string match)) match-list)
    )
    ((= (cal-ascii (coerce (get-radix(car match)) 'list))
     (cal-ascii (coerce (get-radix(car match-list)) 'list)))
      (match-ascii (append match (list (car match-list))) 
        (cdr match-list))
    )
    ((or (eq '+ (car match-list)) (eq '- (car match-list)))
      (match-ascii (append match (list (car match-list))) (cdr match-list)))

    (t (match-ascii match (cdr match-list)))
  )
)
;; make the order of letter presentation in alphabetical order e.g(yx -> xy)
(defun fix-order (input-list output-list initial)
  (cond
    ((null input-list) output-list)
    ((or (eq '- (car input-list)) (eq '+ (car input-list)))
      (fix-order (cdr input-list) (append output-list 
        (list (car input-list))) initial))
    ((not(string= initial (car input-list)))

      (fix-order (cdr input-list) (append output-list 
        (list (concatenate 'string 
          (write-to-string(get-coefficient nil (car input-list))) 
          (get-radix initial))))initial)
    )
    (t (fix-order (cdr input-list) (append output-list
     (list (car input-list))) initial))
  )
)
;; This function raise the power of the whole varibles in the list
(defun raise-power-list (input-list result)
  (cond
    ((null input-list) result)
    ((not (or(eq '- (car input-list)) (eq '+ (car input-list))))
      (raise-power-list (cdr input-list) (append result 
        (list (match-power nil nil (char-to-string-list
         (coerce (car input-list) 'list) nil)))))
    )
    (t (raise-power-list (cdr input-list) 
      (append result (list (car input-list)))))
  )
)
;; If the coefficient is zero, remove it from the list
(defun remove-zero (input-list result)
  (cond
    ((null input-list) result)
    ((stringp (car input-list)) 
      (cond 
        ((char= #\0 (car (coerce (car input-list) 'list))) 
          (remove-zero (cdr input-list) result))

        (t (remove-zero (cdr input-list) 
          (append result (list (car input-list)))))

      )

    )

    (t (remove-zero (cdr input-list) (append result (list (car input-list)))))

  )

)

;; Doing computation including + - 
(defun calculation (existed result groups)
  (cond
    ((null groups)
    (raise-power-list(multiple-operand (remove-zero result nil) nil)nil))

    ((and (= 1 (length groups)) (symbolp (car groups))) ;; handle the last element in groups
      (if (member (coerce (get-radix (car groups)) 'string)
       existed :test #'equal) (calculation existed result (cdr groups))
        (calculation (append  existed 
          (list(coerce (get-radix(car groups)) 'string))) 
          (append result (list (string (car groups)))) (cdr groups))
      )
    )
    ((or (string= "-"  (car groups))
        (string= "*"  (car groups))
        (string= "+"  (car groups)))
        (calculation existed 
          (append result (list (string (car groups)))) (cdr groups))
    )
    ;;((not(member (car (get-radix (car groups))) existed))
    ((not(member (coerce (get-radix (car groups)) 'string)
     existed :test #'equal))
      (calculation (append existed (list 
        (coerce (get-radix(car groups)) 'string)))
                    (append result (cal-coefficient 
                      (matching (car groups) (cdr groups)))) (cdr groups))
    )
    ;;((member (car(get-radix (car groups))) existed)
    ((member (coerce (get-radix (car groups)) 'string) existed :test #'equal)
      (calculation existed result (cdr groups))
    )
  )
)
;; convert positve to negative polynomial
(defun make-negative (input-list output-list)
  (cond
    ((null input-list )
      output-list)
    ((eq '+ (car input-list) )
      (make-negative (cdr input-list) (append output-list (list '-)))
    )
    ((eq '- (car input-list) )
      (make-negative (cdr input-list) (append output-list (list '+)))
    )
    (t
      (make-negative (cdr input-list)
       (append  output-list (list (car input-list)))))
  )

)
;; convert a list of char to a list of string
(defun char-to-string-list (input-list output)
  (cond
    ((null input-list) output)
    (t  (char-to-string-list (cdr input-list)
     (append output (list (string(car input-list)))))
    )
  )
)

;; return the sum of ascii of characters of a string
(defun cal-ascii (string)
  (cond
    ((null string) 0)
    (t (+ (char-code (car string)) (cal-ascii(cdr string))))
  )
)
;; return the ascii value of char in the list
(defun ascii-list (input-list output)
  (cond
    ((null input-list) output)
    (t  (ascii-list (cdr input-list) (append output (list (cal-ascii(coerce 
      (get-radix(car input-list)) 'list))))))
  )
)
;; Convert unordered radix in the list to be ordered
(defun cal-pow (existed result input)

  (cond
    ((null input) (multiple-operand result nil))
    ((or (eq '+ (car input)) (eq '- (car input))) 
      (cal-pow existed (append result (list (car input))) (cdr input)))
    ((not(member (cal-ascii(get-radix(car input)))
     (ascii-list existed nil) :test #'equal))
      (if (= 1 (length input)) (cal-pow existed 
        (append result input) (cdr input))
        (cal-pow (append existed (list (car input))) 
          (append result (match-ascii (car input) (cdr input))) (cdr input))
      )
    )
    (t (cal-pow existed result (cdr input)))

  )
)
;; Contatenate the input string
(defun combine-string (string-list result)
  (cond
    ((null string-list) result)
    (t (combine-string (cdr string-list)
      (concatenate 'string result (car string-list)))
    )
  )
)

;; return the string type of input
(defun return-string (symbols)
  (cond
    ((null symbols) nil)
    ((numberp symbols) (write-to-string symbols))
    ((listp symbols) (coerce symbols 'string))
    (t (string symbols))
  )
)
; Doing the multiplication and output the result with only "+" and "-"
(defun multiplication (p1 p2 result temp) ;temp = original p2
  (cond
    ((null p1) (combine-operand (merge-operand (remove-zero result nil) nil) nil))
    ((null p2) (multiplication (cdr p1) temp result temp))
    ((or (eq '- (car p1)) (eq '+ (car p1)))
      (multiplication p1 (cddr p2) (append result 
        (list (car p1)) (list (car p2))
        (list (concatenate 'string (write-to-string(*
        (get-coefficient nil (return-string (cadr p1)))
        (get-coefficient nil (return-string (cadr p2)))))
        (return-string (get-radix (cadr p1)))
        (return-string (get-radix (cadr p2)))))) temp)
    )
    (t (multiplication p1 (cdr p2) result temp))
  )
)
;; Represent all element by symbol
(defun result-to-symbol (input-list result)
  (cond
    ((null input-list) result)
    ((stringp(car input-list)) (result-to-symbol (cdr input-list) 
      (append result (list (intern (car input-list))))))
    (t (append result (list (car input-list))))
  )
)

(defun poly+ (p1 p2)
  (let* ((groups (append p1 '(+) p2))
        (polynomial (calculation '() '() 
          (negative-number(remove-integer groups nil) nil)))
        (digital (cal-integer (add-postive-sign (get-integer groups nil))0))
        )
          (if(eq nil
          (if (= 0 digital) (result-to-symbol polynomial nil) 
            (result-to-symbol (append polynomial (list "+") (list digital)) nil))) 
          0
          (if (= 0 digital) (result-to-symbol polynomial nil) 
            (result-to-symbol (append polynomial (list "+") (list digital)) nil))) 
        
  )
)
(defun poly- (p1 p2)
  (let* ((groups (append p1 '(-) (make-negative p2 nil)))
        (polynomial (calculation '() '()
         (negative-number(remove-integer groups nil)nil)))
        (digital (cal-integer (add-postive-sign (get-integer groups nil))0)))

         (if(eq nil
          (if (= 0 digital) (result-to-symbol polynomial nil)
           (result-to-symbol (append polynomial (list "+") (list digital)) nil))) 
          0
          (if (= 0 digital) (result-to-symbol polynomial nil) 
            (result-to-symbol (append polynomial (list "+") (list digital)) nil))) 
  )
)
(defun poly* (p1 p2) 
  (let* ((groups (cal-pow nil nil(remove-integer 
    (multiplication p1 p2 nil p2) nil)))

        (polynomial (calculation '() '()  groups))
        (digital (cal-integer (add-postive-sign 
          (get-integer (multiplication p1 p2 nil p2) nil))0))
        )
          
         (if(eq nil
          (if (= 0 digital) (result-to-symbol polynomial nil) 
            (result-to-symbol (append polynomial (list "+") (list digital)) nil))) 
          0
          (if (= 0 digital) (result-to-symbol polynomial nil) 
            (result-to-symbol (append polynomial (list "+") (list digital)) nil))) 
  )
)
