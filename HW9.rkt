;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname HW9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((eq? varName (caar env)) (cadar env))
      (else (resolve varName (cdr env))))))

(define extend-env
  (lambda (lo-vars lo-vals env)
    (cond
      ((null? lo-vars) env)
      (else (extend-env (cdr lo-vars)
                        (cdr lo-vals)
                        (cons (list (car lo-vars) (car lo-vals)) env))))))

(define create-env
  (lambda (lo-vars lo-vals)
    (cond
      ((null? lo-vars) '())
      (else (cons (list (car lo-vars) (car lo-vals)) (create-env (cdr lo-vars) (cdr lo-vals)))))))

(define merge-env
  (lambda (env1 env2)
    (list env1 env2)))

(define complete-env
  (lambda (lo-vars lo-vals env)
    (merge-env (create-env lo-vars lo-vals) env)))

(define extend-env-with-two-list
  (lambda (two-list env)
    (append two-list env)))

(define do-arithmetic-boolean-stuff-toaster
  (lambda (op num1 num2)
    (cond
      ((eq? op '<) (if (< num1 num2) 'True 'False))
      ((eq? op '>) (if (> num1 num2) 'True 'False))
      ((eq? op '<=) (if (<= num1 num2) 'True 'False))
      ((eq? op '>=) (if (>= num1 num2) 'True 'False))
      ((eq? op '=) (if (= num1 num2) 'True 'False))
      ((eq? op '!=) (if (not (= num1 num2)) 'True 'False)))))

(define do-mathy-stuff-toaster
  (lambda (op num1 num2)
    (cond
      ((eq? op '+) (+ num1 num2))
      ((eq? op '-) (- num1 num2))
      ((eq? op '/) (/ num1 num2))
      ((eq? op '//) (quotient num1 num2))
      ((eq? op '%) (modulo num1 num2))
      ((eq? op '*) (* num1 num2))
      (else #f))))

; ***** PARSERS *****
(define no-code-boolean-parser
  (lambda (no-code-boolean)
    (list
          (cadr no-code-boolean)
          (no-parser (caddr no-code-boolean))
          (no-parser (cadddr no-code-boolean)))))

(define no-code-function-parser
  (lambda (no-code-function)
    (list 'func-exp
             (append (list 'params) (cadr no-code-function))
             (list 'body
                   (no-parser (caddr no-code-function))))))

(define no-parser
  (lambda (no-code)
    (cond
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code))
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'ask)
       (list 'ask-exp
             (no-code-boolean-parser (cadr no-code))
             (no-parser (caddr no-code))
             (no-parser (car (reverse no-code)))))
      ((eq? (car no-code) 'let)
       (list 'let-exp
             (cadr no-code)
             (no-parser (caddr no-code))))
      (else (list 'call-exp
                  (no-code-function-parser (cadr no-code))
                  (map no-parser (cddr no-code)))))))

; ***** Interpreters *****
(define run-parsed-boolean-code
  (lambda (parsed-no-code-boolean env)
    (if (eq? (do-arithmetic-boolean-stuff-toaster
              (caadr parsed-no-code-boolean)
              (run-parsed-code (cadr (cadr parsed-no-code-boolean)) env)
              (run-parsed-code (caddr (cadr parsed-no-code-boolean)) env)) 'True)
        (run-parsed-code (caddr parsed-no-code-boolean) env)
        (run-parsed-code (cadddr parsed-no-code-boolean) env))))

(define run-parsed-function-code
  (lambda (parsed-no-code-function env)
    (run-parsed-code (cadr (caddr parsed-no-code-function)) env)))

(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'num-lit-exp)
       (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp)
       (resolve (cadr parsed-no-code) (car (reverse env))))
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'ask-exp)
       (run-parsed-boolean-code parsed-no-code env))
      ((eq? (car parsed-no-code) 'let-exp)
       (run-parsed-code (caddr parsed-no-code)
                        (extend-env-with-two-list (cadr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'call-exp)
         (run-parsed-function-code
        (cadr parsed-no-code)
        (complete-env
         (cdr (cadr (cadr parsed-no-code)))
         (map (lambda (packet) (run-parsed-code (car packet) (cadr packet))) (map (lambda (x) (list x env)) (caddr parsed-no-code)))
         env))))))

(define env '((age 21) (a 7) (b 5) (c 23)))

(define sample-no-code '(call (function (a) (call (function () a))) 5))
(define parsed-no-code (no-parser sample-no-code))
(display parsed-no-code)
(run-parsed-code parsed-no-code env)