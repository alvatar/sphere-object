;;!!! Macros for programming in type-class style in Scheme
;; .author André van Tonder: original work, 2004
;; .author Álvaro Castro-Castilla: ported to rsc-macro-transformer, testing, adapted to SchemeSpheres

;; (define-class <field-form> ...)
;;
;; (define=> (<procedure-name> <class-form> ...) . body)
;;
;; (lambda=> (<class-form> ...) . body)
;;
;; (with (<instance-form> ...) . body)
;;
;; (import-instance <instance-form> ...)
;;
;; <field-form> = field-label
;;              | (<superclass-name> field-label)
;;
;; <class-form> = <class-name>
;;              | (<class-name> <prefix-symbol>)
;;
;; <instance-form> = (<class-name> <instance-expr>)
;;                 | (<class-name> <instance-expr> <prefix-symbol>)

(define-syntax define-class
  (rsc-macro-transformer
   (lambda (form env)
     (let ((name (list-ref form 1))
           (fields (cddr form))
           (filter (lambda (p l)
                     (let recur ((l l))
                       (cond ((null? l) l)
                             ((p (car l)) (cons (car l) (recur (cdr l))))
                             (else (recur (cdr l))))))))
       (let ((k       (gensym))
             (args    (gensym))
             (formals (map (lambda (field) (gensym)) fields))
             (supers  (filter pair? fields))
             (labels  (map (lambda (field)
                             (match field
                                    ((super label) label)
                                    (label         label)))
                           fields)))
         (if #f                         ; debug
             (pp `(begin
                    (define ,(string->symbol
                              (string-append "make-" (symbol->string name)))
                      (lambda ,formals
                        (lambda (,k) (,k . ,formals))))
                    (define-syntax ,name
                      (rsc-macro-transformer
                       (lambda (form env)
                         (let ((,k (list-ref form 1))
                               (,args (cddr form)))
                           `(,,k "descriptor" ,',supers ,',labels . ,,args))))))))
         `(begin
            (define ,(string->symbol
                      (string-append "make-" (symbol->string name)))
              (lambda ,formals
                (lambda (,k) (,k . ,formals))))
            (define-syntax ,name
              (rsc-macro-transformer
               (lambda (form env)
                 (let ((,k (list-ref form 1))
                       (,args (cddr form)))
                   `(,,k "descriptor" ,',supers ,',labels . ,,args)))))))))))

(define-syntax with
  (rsc-macro-transformer
   (lambda (form env)
     (let ((body (cdr form)))
       (match body
              ((() . exps)
               `(let () . ,exps))
              ((((name instance) . rest) . exps)
               `(,name with 
                       ,name "" ,instance ,rest . ,exps))
              ((((name instance prefix) . rest) . exps)
               `(,name with 
                       ,name ,(symbol->string prefix)
                       ,instance ,rest . ,exps))
              (("descriptor" supers labels name pre instance rest . exps)
               (let ((pre-labels
                      (map (lambda (label)
                             (string->symbol 
                              (string-append pre (symbol->string label))))
                           labels))
                     (super-bindings
                      (map (lambda (class-label)
                             `(,(car class-label)
                               ,(string->symbol
                                 (string-append pre
                                                (symbol->string
                                                 (cadr class-label))))
                               ,(string->symbol pre)))
                           supers)))
                 (if #f                 ; debug
                     (pp `(,instance (lambda ,pre-labels
                                       (with ,super-bindings
                                             (with ,rest . ,exps))))))
                 `(,instance (lambda ,pre-labels
                               (with ,super-bindings
                                     (with ,rest . ,exps)))))))))))

(define-syntax import-instance
  (rsc-macro-transformer
   (lambda (form env)
     (let ((bindings (cdr form)))
       (match bindings
              (()
               "Bindings imported")
              (((name instance) . rest)
               `(,name import-instance
                       ,name "" ,instance ,rest))
              (((name instance prefix) . rest)
               `(,name import-instance 
                       ,name ,(symbol->string prefix) 
                       ,instance ,rest))
              (("descriptor" supers labels name pre instance rest)
               (let ((pre-labels.temps
                      (map (lambda (label)
                             (cons 
                              (string->symbol 
                               (string-append pre (symbol->string label)))
                              (gensym)))
                           labels))
                     (super-bindings
                      (map (lambda (class-label)
                             `(,(car class-label)
                               ,(string->symbol
                                 (string-append pre
                                                (symbol->string
                                                 (cadr class-label))))
                               ,(string->symbol pre)))
                           supers)))
                 (if #f                 ; debug
                     (pp `(begin ,@(map (lambda (pre-label.temp)
                                          `(define ,(car pre-label.temp) #f))
                                        pre-labels.temps)
                                 (,instance (lambda ,(map cdr pre-labels.temps)
                                              ,@(map (lambda (pre-label.temp)
                                                       `(set! ,(car pre-label.temp)
                                                              ,(cdr pre-label.temp)))
                                                     pre-labels.temps)))
                                 (import-instance . ,super-bindings)
                                 (import-instance . ,rest))))
                 `(begin ,@(map (lambda (pre-label.temp)
                                  `(define ,(car pre-label.temp) #f))
                                pre-labels.temps)
                         (,instance (lambda ,(map cdr pre-labels.temps)
                                      ,@(map (lambda (pre-label.temp)
                                               `(set! ,(car pre-label.temp)
                                                      ,(cdr pre-label.temp)))
                                             pre-labels.temps)))
                         (import-instance . ,super-bindings)
                         (import-instance . ,rest)))))))))

(define-syntax lambda=>
  (rsc-macro-transformer
   (lambda (form env)
     (let ((quals (list-ref form 1))
           (body (cddr form)))
       (let ((quals-binds (map (lambda (qual)
                                 (match qual
                                        ((cls prefix) (list cls (gensym) prefix))
                                        (cls          (list cls (gensym)))))
                               quals)))
         `(lambda ,(map cadr quals-binds)
            (with ,quals-binds
                  . ,body)))))))

(define-syntax define=>
  (rsc-macro-transformer
   (lambda (form env)
     (let ((name.quals (list-ref form 1))
           (body (cddr form)))
       (let ((name  (car name.quals))
             (quals (cdr name.quals)))
         `(define ,name (lambda=> ,quals . ,body)))))))
