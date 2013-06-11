
(define (make-light location color)
  (set! lights (cons
    (lambda (selector)
      (case selector
        ((location)
         (lambda ()
           location))
        ((color)
         (lambda ()
           color))
        ((display)
         (lambda ()
           (display "location ") ((location 'display))))
        (else
         (render-error "make-light" "Invalid selector: " selector))))
    lights)))
