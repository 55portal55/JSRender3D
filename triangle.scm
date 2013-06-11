(define (make-triangle p1 p2 p3 shader)
  (set! scene-objects (cons
  (let*
    ((plane (make-plane p1 p2 p3 shader))
     (ray-intersects-triangle?
       (lambda (orig dir-x dir-y dir-z)
         (let
           ((det 0.0)
            (inv-det 0.0)
            (u 0.0)
            (v 0.0)
            (edge1 (make-point 0.0 0.0 0.0))
            (edge2 (make-point 0.0 0.0 0.0))
            (pvec (make-point 0.0 0.0 0.0))
            (qvec (make-point 0.0 0.0 0.0))
            (tvec (make-point 0.0 0.0 0.0))
            (dir (make-point dir-x dir-y dir-z)))
           (v-! p2 p1 edge1)
           (v-! p3 p1 edge2)
           (set! pvec (cross-product dir edge2))
           (set! det (dot edge1 pvec))
           (if (and (> det (- EPSILON)) (< det EPSILON))
             #f
             (begin
               (set! inv-det (/ 1.0 det))
               (v-! orig p1 tvec)
               (set! u (* (dot tvec pvec) inv-det))
               (if (or (< u 0.0) (> u 1.0))
                 #f
                 (begin
                   (set! qvec (cross-product tvec edge1))
                   (set! v (* (dot dir qvec) inv-det))
                   (if (or (< v 0.0) (> (+ u v) 1.0))
                     #f
                     #t)))))))))
;    (ray-intersects-triangle?
;      (lambda (orig dir-x dir-y dir-z)
;        (does-ray-intersect-triangle
;          ((p1 'get-x))
;          ((p1 'get-y))
;          ((p1 'get-z))
;          ((p2 'get-x))
;          ((p2 'get-y))
;          ((p2 'get-z))
;          ((p3 'get-x))
;          ((p3 'get-y))
;          ((p3 'get-z))
;          ((orig 'get-x))
;          ((orig 'get-y))
;          ((orig 'get-z))
;          dir-x dir-y dir-z)
;        )))
    (lambda (selector)
      (case selector
        ((get-bounding-box)
          (lambda ()
            (make-bounding-box
              (make-point
                (min ((p1 'get-x)) ((p2 'get-x)) ((p3 'get-x)))
                (min ((p1 'get-y)) ((p2 'get-y)) ((p3 'get-y)))
                (min ((p1 'get-z)) ((p2 'get-z)) ((p3 'get-z))))
              (make-point
                (max ((p1 'get-x)) ((p2 'get-x)) ((p3 'get-x)))
                (max ((p1 'get-y)) ((p2 'get-y)) ((p3 'get-y)))
                (max ((p1 'get-z)) ((p2 'get-z)) ((p3 'get-z)))))))
        ((get-shader)
          (lambda ()
            shader))
        ((get-normal)
          (lambda (intersection-point)
            ((plane 'get-normal))))
        ((intersect-ray)
          (lambda (ray-source dir-x dir-y dir-z shadow-test)
            (if (ray-intersects-triangle? ray-source dir-x dir-y dir-z)
              (let
                ((intersection-point
                  ((plane 'intersect-ray)
                    ray-source dir-x dir-y dir-z shadow-test)))
                (if (null? intersection-point)
                  '()
                  intersection-point))
              '())))
        ((display)
          (lambda ()
            (display "triangle ")
              ((p1 'display)) (display " ")
              ((p2 'display)) (display " ")
              ((p3 'display)) (newline)
            ((plane 'display))))
        (else
          (render-error "make-triangle" "Invalid selector: " selector)))))
  scene-objects)))
