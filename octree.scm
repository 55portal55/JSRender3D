(define (make-octree level octree-objects bounding-box)
  (letrec
    ((octree-nodes 8)
     (max-leaf-objects 12)
     (max-levels 4)
     (octree-bounding-box '())
     (scene-object-count 0)
     (octree '())
     (get-min-distance-to-all-objects-in-leaf
       (lambda (ray-source camera-direction
         dir-x dir-y dir-z objects-in-leaf intersection-object)
         (if (not (null? objects-in-leaf))
           (let*
             ((scene-object (car objects-in-leaf))
              (intersection-point
                ((scene-object 'intersect-ray)
                  ray-source dir-x dir-y dir-z
                  #f))) ; false is no shadow test
             (set! intersection-tests (+ intersection-tests 1)) ; global var
             (if (not (null? intersection-point))
               (if (> (dot camera-direction
                           (normalize (v- intersection-point ray-source)))
                      0.0)
                 ; point not behind ray source
                 (let
                   ((distance ((ray-source 'distance) intersection-point)))
                   (if (< distance ((intersection-object 'get-distance)))
                     (begin
                       ((intersection-object 'set-distance) distance)
                       ((intersection-object 'set-scene-object) scene-object)
                       ((intersection-object 'set-min-intersection-point)
                         intersection-point))))))
             (get-min-distance-to-all-objects-in-leaf
               ray-source camera-direction
               dir-x dir-y dir-z
               (cdr objects-in-leaf) intersection-object)))))
     (shadow-test-all-objects-in-leaf
       (lambda (origin ray-direction
         dir-x dir-y dir-z objects-in-leaf
         light-distance)
         (if (null? objects-in-leaf)
           #f
           (let*
             ((octree-object (car objects-in-leaf))
              (intersection-point
                ((octree-object 'intersect-ray)
                  origin dir-x dir-y dir-z
                  #t))) ; true is shadow test
             (set! intersection-tests (+ intersection-tests 1)) ; global var
             (if (not (null? intersection-point))
               (if (> (dot ray-direction
                           (normalize (v- intersection-point origin)))
                      0.0)
                 ; point not behind ray source
                 (let
                   ((distance ((origin 'distance) intersection-point)))
                   (if (and (>= distance EPSILON) (< distance light-distance))
                     #t
                     (shadow-test-all-objects-in-leaf
                       origin ray-direction
                       dir-x dir-y dir-z
                       (cdr objects-in-leaf)
                       light-distance)))
                 (shadow-test-all-objects-in-leaf
                   origin ray-direction
                   dir-x dir-y dir-z
                   (cdr objects-in-leaf)
                   light-distance))
               (shadow-test-all-objects-in-leaf
                 origin ray-direction
                 dir-x dir-y dir-z
                 (cdr objects-in-leaf)
                 light-distance))))))
     (select-objects-in-octree
       (lambda (objects
         octree-bb-lower-x octree-bb-lower-y octree-bb-lower-z
         octree-bb-upper-x octree-bb-upper-y octree-bb-upper-z)
         (if (null? objects)
           '()
           (let*
             ((scene-object (car objects))
              (object-bounding-box ((scene-object 'get-bounding-box))))
             (if (not (or
               (< ((((object-bounding-box 'get-upper-extent)) 'get-x))
                  octree-bb-lower-x)
               (> ((((object-bounding-box 'get-lower-extent)) 'get-x))
                  octree-bb-upper-x)
               (< ((((object-bounding-box 'get-upper-extent)) 'get-y))
                  octree-bb-lower-y)
               (> ((((object-bounding-box 'get-lower-extent)) 'get-y))
                  octree-bb-upper-y)
               (< ((((object-bounding-box 'get-upper-extent)) 'get-z))
                  octree-bb-lower-z)
               (> ((((object-bounding-box 'get-lower-extent)) 'get-z))
                  octree-bb-upper-z)))
               (cons
                 scene-object
                 (select-objects-in-octree
                   (cdr objects)
                   octree-bb-lower-x octree-bb-lower-y octree-bb-lower-z
                   octree-bb-upper-x octree-bb-upper-y octree-bb-upper-z))
               (select-objects-in-octree
                 (cdr objects)
                 octree-bb-lower-x octree-bb-lower-y octree-bb-lower-z
                 octree-bb-upper-x octree-bb-upper-y octree-bb-upper-z)))))))

    (let*
      ((lower-extent ((bounding-box 'get-lower-extent)))
       (upper-extent ((bounding-box 'get-upper-extent)))
       (bb-lower-x ((lower-extent 'get-x)))
       (bb-lower-y ((lower-extent 'get-y)))
       (bb-lower-z ((lower-extent 'get-z)))
       (bb-upper-x ((upper-extent 'get-x)))
       (bb-upper-y ((upper-extent 'get-y)))
       (bb-upper-z ((upper-extent 'get-z))))
      (set! octree-objects
        (select-objects-in-octree octree-objects
          bb-lower-x bb-lower-y bb-lower-z
          bb-upper-x bb-upper-y bb-upper-z))
      (set! scene-object-count (length octree-objects))

;(display (length octree-objects)) (newline)
      (if (not (= scene-object-count 0))
        (begin
          (set! octree-bounding-box (make-AABB bounding-box))
          (if (and (> scene-object-count max-leaf-objects)
                   (< level max-levels))
            ; subdivide
            (let
              ((delta-x (/ (- bb-upper-x bb-lower-x) 2.0))
               (delta-y (/ (- bb-upper-y bb-lower-y) 2.0))
               (delta-z (/ (- bb-upper-z bb-lower-z) 2.0)))
              (set! octree (make-vector octree-nodes))
              (do ((z 0 (+ z 1))) ((> z 1))
                (do ((y 0 (+ y 1))) ((> y 1))
                  (do ((x 0 (+ x 1))) ((> x 1))
                    (vector-set! octree (+ (* z 4) (* y 2) x)
                      (make-octree
                        (+ level 1)
                        octree-objects
                        (make-bounding-box
                          (make-point
                            (+ bb-lower-x (* delta-x x))
                            (+ bb-lower-y (* delta-y y))
                            (+ bb-lower-z (* delta-z z)))
                          (make-point
                            (+ bb-lower-x (* delta-x x) delta-x)
                            (+ bb-lower-y (* delta-y y) delta-y)
                            (+ bb-lower-z (* delta-z z) delta-z)))))))))))))
    (lambda (selector)
      (case selector
        ((get-bounding-box)
          (lambda ()
            ((octree-bounding-box 'get-bounding-box))))
        ((ray-traverse-octree)
          (lambda (level ray camera-direction intersection-object)
            (if (not (null? octree-bounding-box))
              (if ((octree-bounding-box 'ray-intersects?) ray)
                (if (or (<= scene-object-count max-leaf-objects) ; leaf node
                        (= level max-levels))
                  (get-min-distance-to-all-objects-in-leaf
                    ((ray 'get-origin))
                    camera-direction
                    ((ray 'get-dir-x))
                    ((ray 'get-dir-y))
                    ((ray 'get-dir-z))
                    octree-objects intersection-object)
                  (do ((i 0 (+ i 1))) ((= i octree-nodes))
                    (((vector-ref octree i) 'ray-traverse-octree)
                      (+ level 1)
                      ray camera-direction intersection-object)))))))
        ((shadow-ray-traverse-octree)
          (lambda (level ray light-distance)
            (if (not (null? octree-bounding-box))
              (if ((octree-bounding-box 'ray-intersects?) ray)
                (if (or (<= scene-object-count max-leaf-objects) ; leaf node
                        (= level max-levels))
                  (shadow-test-all-objects-in-leaf
                    ((ray 'get-origin)) ((ray 'get-direction))
                    ((ray 'get-dir-x))
                    ((ray 'get-dir-y))
                    ((ray 'get-dir-z))
                    octree-objects
                    light-distance)
                  ; else subtree
                  (let
                    ((return #f))
                    (do ((i 0 (+ i 1))) ((or (= i octree-nodes) return))
                      (set! return
                        (((vector-ref octree i) 'shadow-ray-traverse-octree)
                          (+ level 1)
                          ray light-distance)))
                    return))
                  #f)
                #f)))
        (else
          (render-error "make-octree" "Invalid selector: " selector))))))
