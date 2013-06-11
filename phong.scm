
(define (make-phong ambient diffuse specular specular-N)
  (lambda (selector)
    (case selector
      ((get-ambient)
       (lambda ()
         ambient))
      ((get-diffuse)
       (lambda ()
         diffuse))
      ((get-specular)
       (lambda ()
         specular))
      ((get-specular-N)
       (lambda ()
         specular-N))
      (else
       (render-error "make-phong" "Invalid selector: " selector)))))
