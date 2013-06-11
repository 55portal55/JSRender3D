(define (make-cylinder top bottom radius shader)
  (set! scene-objects (cons
  (let*
    ((axis (normalize (v- top bottom)))
     (top-plane (make-plane-given-normal top (v-scale axis 1.0) shader))
     (bottom-plane (make-plane-given-normal bottom (v-scale axis -1.0) shader))
     (surf-in 'SIDE)
     (surf-out 'SIDE))
    (lambda (selector)
      (case selector
        ((get-bounding-box)
          (lambda ()
            (let*
              ((half-axis (v-scale (v- top bottom) 0.5))
               (half-axis-length (vec-length-1 half-axis))
               (center (v+ bottom half-axis))
               (radius (sqrt (+ (* radius radius) half-axis-length))) ) 
              (make-bounding-box
                (make-point
                  (- ((center 'get-x)) radius)
                  (- ((center 'get-y)) radius)
                  (- ((center 'get-z)) radius))
                (make-point
                  (+ ((center 'get-x)) radius)
                  (+ ((center 'get-y)) radius)
                  (+ ((center 'get-z)) radius))))))
        ((get-shader)
          (lambda ()
            shader))
        ((get-normal)
          (lambda (intersection-point)
            (if (eq? surf-in 'SIDE)
              (let
                ((v (v- intersection-point bottom))
                 (u (v- top bottom)))
                (v-scale (normalize
                  (v- v
                      (v-scale u (/ (dot v u) (vec-length-1 u)))))
                  -1.0))
              ; else top or bottom cap
              (if (eq? surf-in 'TOP)
                ((top-plane 'get-normal))
                ((bottom-plane 'get-normal))))))
        ((intersect-ray)
          (lambda (ray-source dir-x dir-y dir-z shadow-test)
            (let*
              ((ray-intersects-cylinder?
                (lambda (orig direction)
                  (let*
                    ((rc (v- orig bottom))
                     (d 0.0)
                     (hit #f)
                     (n (cross-product direction axis))
                     (ln (vec-length n)))
                    (if (= ln 0.0) ; ray parallel to cylinder
                      (let*
                        ((d (dot rc axis))
                         (D (v- rc (v-scale axis d)))
                         (d (vec-length D)))
                        (if (<= d radius)
                          (list (- INFINITY) INFINITY)
                          '()))
                      (begin
                        (set! n (normalize n))
                        (set! d (abs (dot rc n)))
                        (set! hit (<= d radius))
                        (if hit
                          (let*
                            ((O (cross-product rc axis))
                             (t (- (/ (dot O n) ln)))
                             (O (cross-product n axis))
                             (O (normalize O))
                             (s (abs
                               (/ (sqrt (- (* radius radius) (* d d)))
                                  (dot direction O)))))
                            (list (- t s) (+ t s)))
                          '()))))))
              (clip-object
                (lambda (orig direction in out)
                  (let
                    ((return-false #f)
                     (dc 0.0)
                     (dw 0.0)
                     (t 0.0))
                    (set! surf-in 'SIDE)
                    (set! surf-out 'SIDE)
                    (set! dc (+
                      (* ((bottom-plane 'get-a)) ((direction 'get-x)))
                      (* ((bottom-plane 'get-b)) ((direction 'get-y)))
                      (* ((bottom-plane 'get-c)) ((direction 'get-z)))))
                    (set! dw (+
                      (* ((bottom-plane 'get-a)) ((orig 'get-x)))
                      (* ((bottom-plane 'get-b)) ((orig 'get-y)))
                      (* ((bottom-plane 'get-c)) ((orig 'get-z)))
                      ((bottom-plane 'get-d))))
                    (if (= dc 0.0)
                      (if (>= dw 0.0)
                        (set! return-false #t))
                      (begin
                        (set! t (- (/ dw dc)))
                        (if (>= dc 0.0)
                          (begin
                            (if (and (> t in) (< t out))
                              (begin
                                (set! out t)
                                (set! surf-out 'BOT)))
                            (if (< t in)
                              (set! return-false #t)))
                          (begin
                            (if (and (> t in) (< t out))
                              (begin
                                (set! in t)
                                (set! surf-in 'BOT)))
                            (if (> t out)
                              (set! return-false #t))))))
                    (if return-false
                      #f
                      (begin
                        (set! dc (+
                          (* ((top-plane 'get-a)) ((direction 'get-x)))
                          (* ((top-plane 'get-b)) ((direction 'get-y)))
                          (* ((top-plane 'get-c)) ((direction 'get-z)))))
                        (set! dw (+
                          (* ((top-plane 'get-a)) ((orig 'get-x)))
                          (* ((top-plane 'get-b)) ((orig 'get-y)))
                          (* ((top-plane 'get-c)) ((orig 'get-z)))
                          ((top-plane 'get-d))))
                        (if (= dc 0.0)
                          (if (>= dw 0.0)
                            (set! return-false #t))
                          (begin
                            (set! t (- (/ dw dc)))
                            (if (>= dc 0.0)
                              (begin
                                (if (and (> t in) (< t out))
                                  (begin
                                    (set! out t)
                                    (set! surf-out 'TOP)))
                                (if (< t in)
                                  (set! return-false #t)))
                              (begin
                                (if (and (> t in) (< t out))
                                  (begin
                                    (set! in t)
                                    (set! surf-in 'TOP)))
                                (if (> t out)
                                  (set! return-false #t))))))
                        (if return-false
                          #f
                          (if (< in out)
                            (list in out)
                            #f))))))))
                (let*
                  ((direction (make-point dir-x dir-y dir-z))
                   (intersect-value (ray-intersects-cylinder? ray-source direction)))
                  (if (null? intersect-value)
                    '()
                    (let
                      ((intersect-value (clip-object
                        ray-source direction
                        (car intersect-value) (cadr intersect-value))))
                      (if (not intersect-value)
                        '()
                        (v+ ray-source (v-scale direction (car intersect-value))))))
          ))))
        ((display)
          (lambda ()
            (display "cylinder ")
              ((top 'display)) (display " ")
              ((bottom 'display)) (display " ")
              (display radius) (newline)))
        (else
          (render-error "make-cylinder" "Invalid selector: " selector)))))
  scene-objects)))
