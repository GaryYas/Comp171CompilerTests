;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; Comp171 - Compiler - Tests

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "compiler.scm")	    

(define tests-counter 0)
(define failed-tests-counter 0)

;;;; Configuration
(define show-passed-tests #t)
(define show-summary #t)
(define catch-exceptions #f)

(define try-catch
  (lambda (try-thunk catch-thunk)
    (if catch-exceptions
      (guard (c (else (catch-thunk)))
      (try-thunk))
      (try-thunk))
))
		
(define file->string
  (lambda (in-file)
    (let ((in-port (open-input-file in-file)))
      (letrec ((run
	(lambda ()
	  (let ((ch (read-char in-port)))
	    (if (eof-object? ch)
	      (begin
		(close-input-port in-port)
		'())
	      (cons ch (run)))))))
	(list->string (run))))
))

(define string->file
  (lambda (str out-file)
    (letrec ((out-port (open-output-file out-file))
	  (run (lambda (lst)
		  (if (null? lst) #t
		      (begin (write-char (car lst) out-port)
			     (run (cdr lst)))))))
	(begin
	  (run (string->list str))
	  (close-output-port out-port)))
	    
))

(define test-func
  (lambda (input)
    (string->file input "outFile.scm")
    (compile-scheme-file "outFile.scm" "outFile.c")
    (system "gcc -o outFile outFile.c")
    (system "./outFile > outResult")
    (let ((result (file->string "outResult")))
      (system "rm -f outResult")
      (system "rm -f outFile")
      (system "rm -f outFile.c")
      (system "rm -f outFile.scm")
      result)
))
		
(define assert
	(lambda (input expected-output)
		(set! tests-counter (+ 1 tests-counter))
		(try-catch (lambda ()
		(let ((actual-output (test-func input)))			
			(cond ((equal? actual-output expected-output)
				(if show-passed-tests
				  (begin (display (format "~s) ~s\n" tests-counter input))
				  (display (format "\033[1;32m Success! ☺ \033[0m \n\n")))) 
				  #t)
				(else
				  (set! failed-tests-counter (+ 1 failed-tests-counter))
				  (display (format "~s) ~s\n" tests-counter input))
				  (display (format "\033[1;31mFailed! ☹\033[0m\n\n\033[1;34mExpected:\n ~s\033[0m\n\n\033[1;29mActual:\n ~a\033[0m\n\n" expected-output actual-output))
				#f))))
			(lambda () (set! failed-tests-counter (+ 1 failed-tests-counter))
				(display (format "~s) ~s\n" tests-counter input))
				(display 
				    (format "\n\033[1;31mEXCEPTION OCCURED. PLEASE CHECK MANUALLY THE INPUT:\n ~s\033[0m\n\n" input)) #f))
			))
			
(define runTests
  (lambda (tests-name lst)
	(newline)
	(display tests-name)
	(display ":")
	(newline)
	(display "==============================================")
	(newline)
	(let ((results (map (lambda (x) (assert (car x) (cdr x))) lst)))
	(newline)
	(cond ((andmap (lambda (exp) (equal? exp #t)) results)	
		(display (format "\033[1;32m~s Tests: SUCCESS! ☺ \033[0m\n \n" tests-name)) #t)		
		(else
		(display (format "\033[1;31m~s Tests: FAILED! ☹ \033[0m\n \n" tests-name)) #f)))
))

(define runAllTests
  (lambda (lst)
    (let ((results (map (lambda (test) (runTests (car test) (cdr test))) lst)))
	(if show-summary
	  (begin
	    (display (format "Summary\n=============================\n\033[1;32mPassed: ~s of ~s tests ☺\033[0m\n" (- tests-counter failed-tests-counter) tests-counter))
	    (if (> failed-tests-counter 0)
	      (display (format "\033[1;31mFailed: ~s of ~s tests ☹\033[0m\n\n" failed-tests-counter tests-counter)))))
      	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32m!!!!!  ☺  ALL TESTS SUCCEEDED  ☺  !!!!\033[0m\n") #t)
		(else (display "\033[1;31m#####  ☹  SOME TESTS FAILED  ☹  #####\033[0m\n") #f))
		(newline))
))

(define constants-table-tests
  (list
  
    ;;Numbers
    (cons "0" "0")
    (cons "1" "1")
    (cons "5" "5")
    (cons "-10" "-10")
    
    ;;Fractions
    (cons "2/4" "1/2")
    (cons "-3/5 " "-3/5")
    
    ;;Strings
    (cons "\"123abc\"" "\"123abc\"")
    (cons "\"aA\"" "\"aA\"")
    (cons "\"\"" "\"\"")
    
    ;;Boolean
    (cons "#t" "#t")
    (cons "#f" "#f")
    
    ;;Chars
    (cons "#\\a" "#\\a")
    (cons "#\\space" "#\\space")
    (cons "#\\newline" "#\\newline")
    
    ;;Vectors
    (cons "#(1 2 3)" "#3(1 2 3)")
    (cons "#(1 (1 2 3) #t #f)" "#4(1 (1 . (2 . (3 . ()))) #t #f)")
    
))

(define or-if-begin-tests
(list
    (cons "(or #t)" "#t")
    (cons "(or #f)" "#f")
    (cons "(or (or #f #f) (or #t))" "#t")
    (cons "(or (or #f #f) (or 0))" "0")
    
    (cons "(or #f #f)" "#f")
    (cons "(or #f #t)" "#t")
    (cons "(if #t 1 0)" "1")
    
    ;;Nested If
    (cons "(if #t (if #t (if #t (if #t (if #t
	    (if #t (if #t (if #t (if #t 
	    (if #t (if #t (if #t (if #t (if #t #t #f) #f) #f) #f) #f) #f) #f) #f) #f) 
	    #f) #f ) #f) #f) #f)" "#t")
    
    
    (cons "(if #f 1 0)" "0")
    (cons "(or 25 #t #f 1 2 3 #f)" "25")
    (cons "(and 25 #t #f 1 2 3 #f)" "#f")
    
    (cons "(begin (or #t #t) (or #f))" "#f")
    (cons "(begin #\\a)" "#\\a")
    (cons "(begin (or #t #t) (or #f) (begin 1 2 3 45 \"Its a String\"))" "\"Its a String\"")
))


(display (format "\033[1mComp171 - Compiler Tests\033[0m\n================================\n"))

(runAllTests
  (list      
      (cons "Constants Table" constants-table-tests)  
      (cons "Or, If and Begin" or-if-begin-tests)      
))