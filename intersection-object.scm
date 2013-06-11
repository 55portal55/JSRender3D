(define (make-intersection-object)
  (let*
    ((distance INFINITY)
     (scene-object '())
     (min-intersection-point '()))
    (lambda (selector)
      (case selector
        ((get-distance)
         (lambda () distance))
        ((get-scene-object)
         (lambda () scene-object))
        ((get-min-intersection-point)
         (lambda () min-intersection-point))
        ((set-distance)
         (lambda (a) (set! distance a)))
        ((set-scene-object)
         (lambda (a) (set! scene-object a)))
        ((set-min-intersection-point)
         (lambda (a) (set! min-intersection-point a)))
        (else
         (render-error "make-intersection-object" "Invalid selector: " selector))))))
