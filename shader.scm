
(define (make-shader color phong reflection no-shadow)
  (lambda (selector)
    (case selector
      ((get-color)
       (lambda ()
         color))
      ((get-phong)
       (lambda ()
         phong))
      ((get-reflection)
       (lambda ()
         reflection))
      ((get-no-shadow)
       (lambda ()
         no-shadow))
      (else
       (render-error "make-shader" "Invalid selector: " selector)))))
