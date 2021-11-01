(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((or (eq? (car env) 'temp) (eq? (car env) 'global)) (resolve varName (cdr env)))
      ((eq? varName (caar env)) (cadar env))
      (else (resolve varName (cdr env))))))

(define resolve2
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((not (eq? (resolve varName (car env)) #f)) (resolve varName (car env)))
      (else (resolve2 varName (cdr env))))))


(define burn-to-global
  (lambda (env)
    (if (eq? (caar env) 'global) env
        (burn-to-global (cdr env)))))





(define extend-env
  (lambda (lo-vars lo-vals env)
    (cond
      ((null? lo-vars) env)
      (else (extend-env (cdr lo-vars)
                        (cdr lo-vals)
                        (cons (list (car lo-vars) (car lo-vals)) env))))))

(define extend-env2
  (lambda (lo-vars lo-vals env)
    (cons (reverse (create-env lo-vars lo-vals)) env)))


(define env-let-get-var-names
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (caar lst) (env-let-get-var-names (cdr lst))))))

(define env-let-get-var-values
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (cadar lst) (env-let-get-var-values (cdr lst))))))

(define env-let-mapper
  (lambda (lst)
    (list (env-let-get-var-names lst)
          (env-let-get-var-values lst))))



(define create-env
  (lambda (lo-vars lo-vals)
    (cond
      ((null? lo-vars) '(temp))
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
       (resolve2 (cadr parsed-no-code) env))
      ((eq? (car parsed-no-code) 'let-exp)
       (let* ((var-2-lists (env-let-mapper (cadr parsed-no-code)))
              (new-env (extend-env (car var-2-lists) (cadr var-2-lists) env))
              (body (caddr parsed-no-code)))
         (run-parsed-code body new-env))) 
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'ask-exp)
       (if (run-parsed-boolean-code (cadr parsed-no-code) env) 
           (run-parsed-code (caddr parsed-no-code) env)
           (run-parsed-code (cadddr parsed-no-code) env)))
      (else
         (run-parsed-function-code
        (cadr parsed-no-code)
        (extend-env2
         (cdr (cadr (cadr parsed-no-code)))
         (map (lambda (packet) (run-parsed-code (car packet) (cadr packet))) (map (lambda (x) (list x env)) (caddr parsed-no-code)))
         (burn-to-global env)))))))

(define env '((global (age 21) (a 7) (b 5) (c 23))))

(define env2 '((temp (a 5)) (global (age 21) (a 7) (b 5) (c 23))))

;((temp (a 5)) (global (age 21) (a 7) (b 5) (c 23)))



(define sample-no-code '(call (function (a) (call (function (r) a) a)) 5))
(define sample-no-code2 '(call (function (x) x) a))
(define parsed-no-code (no-parser sample-no-code))
;(display parsed-no-code)
(run-parsed-code parsed-no-code env)
