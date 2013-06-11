(define (make-background color)
  (set! scene-background
  (lambda (selector)
    (case selector
      ((get-background)
       (lambda () color))
      (else
       (render-error "make-background" "Invalid selector: " selector))))))

(define (make-fog color distance)
  (set! scene-fog
  (lambda (selector)
    (case selector
      ((get-color)
       (lambda () color))
      ((get-distance)
       (lambda () distance))
      (else
       (render-error "make-fog" "Invalid selector: " selector))))))

(define (get-x p)
  ((p 'get-x)))

(define (get-y p)
  ((p 'get-y)))

(define (get-z p)
  ((p 'get-z)))

; TODO dont need anymore
;(define (get-clock) scene-clock)
