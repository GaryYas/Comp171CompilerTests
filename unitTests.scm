;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; Comp171 - Compiler -  Unit Tests

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "compiler.scm")

(define assert
	(lambda (f input output)
		(let* ((my-res (apply f input)))
			(display (format "~s:\n ==>" input))
			(display (format "\033[1;34m ~s \033[0m" my-res))
			(cond ((equal? my-res output)
				(display (format "\033[1;32m Success! ☺ \033[0m \n")) #t)
				(else 
				(display (format "\033[1;31m Failed! ☹\033[0m , Expected: ~s, Actual: ~s \n" output my-res)) #f))
			)))
			
(define runTests
  (lambda (tests-name lst)
	(newline)
	(display tests-name)
	(display ":")
	(newline)
	(display "==============================================")
	(newline)
	(let ((results (map (lambda (el) (assert (car el) (cadr el) (caddr el))) lst)))
	(newline)
	(cond ((andmap (lambda (exp) (equal? exp #t)) results)	
		(display (format "\033[1;32m~s Tests: SUCCESS! ☺ \033[0m\n \n" tests-name)) #t)		
		(else
		(display (format "\033[1;31m~s Tests: FAILED! ☹ \033[0m\n \n" tests-name)) #f)))
))

(define runAllTests
  (lambda (lst)
    (let ((results (map (lambda (test) (runTests (car test) (cdr test))) lst)))
      	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32m !!!!!  ☺  ALL TESTS SUCCEEDED  ☺  !!!!\033[0m\n") #t)
		(else (display "\033[1;31m #####  ☹  SOME TESTS FAILED  ☹  #####\033[0m\n") #f))
		(newline))
))

(define create-sub-constants-tests
  (list
    ;; Format (list func-name list-of-args expected-result)
    (list create-sub-constants (list '(1 2 3)) '(1 2 3 (3) (2 3) (1 2 3)))
))

(display (format "\033[1mComp171 - Compiler Unit Tests\033[0m\n================================\n"))

(runAllTests
  (list  
      (cons "create-sub-constants" create-sub-constants-tests)           

))