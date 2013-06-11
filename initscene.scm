(define (init-scene)
  (letrec (
    (read-number
      (lambda ()
        (if (null? number-list)
          '()
          (let
            ((value (car number-list)))
            (set! number-list (cdr number-list))
            value))))
    (read-integer
      (lambda () (read-number)))
    (read-double
      (lambda () (read-number)))
    (read-point
      (lambda () (make-point (read-double) (read-double) (read-double))))
    (read-phong
      (lambda ()
        (let* ; forces left to right evaluation
          ((ambient (read-double))
           (diffuse (read-double))
           (specular (read-double))
           (specular-N (read-integer)))
          (make-phong ambient diffuse specular specular-N))))
    (read-no-shadow
      (lambda () (read-integer)))
    (read-shader
      (lambda ()
        (let* ; forces left to right evaluation
          ((color (read-point))
           (phong (read-phong))
           (reflection (read-double))
           (no-shadow (= (read-no-shadow) 1))
           (decal-image (read-integer))
           (latitude-offset (read-double))
           (longitude-offset (read-double)))
          (make-shader color phong reflection no-shadow))))
    (read-camera
      (lambda ()
        (make-camera
          (read-integer) ; perspective
          (read-point) ; up
          (read-point) ; location
          (read-point) ; look-at
          (read-double)))) ; view-angle
    (read-light
      (lambda ()
        (make-light
          (read-point) ; location
          (read-point)))) ; color
    (read-triangle
      (lambda ()
        (make-triangle
          (read-point) ; p1
          (read-point) ; p2
          (read-point) ; p3
          (read-shader)))) ; shader
    (read-sphere
      (lambda ()
        (make-sphere
          (read-point) ; center
          (read-double) ; radius
          (read-shader)))) ; shader
    (read-fog
      (lambda ()
        (make-fog
          (read-point) ; color
          (read-double)))) ; distance
    (read-cylinder
      (lambda ()
        (make-cylinder
          (read-point) ; top
          (read-point) ; bottom
          (read-double) ; radius
          (read-shader)))) ; shader
    (read-actions
      (lambda ()
        (let
          ((action (read-integer)))
          (if (not (null? action))
            (begin
              (case action
                ((0) (make-background (read-point)))
                ((1) (set! camera (read-camera)))
                ((2) (read-light))
                ((3) (read-triangle))
                ((4) (read-sphere))
                ((5) (read-fog))
                ((6) (read-cylinder))
                (else
                  (render-error "init-scene" "Invalid case " action)))
                (read-actions))))))
    )

    (read-actions)

    (if (not (null? scene-fog))
      (begin
        (set! fog-color ((scene-fog 'get-color)))
        (set! fog-distance ((scene-fog 'get-distance))))
      (begin
        (set! fog-color (make-point 0.0 0.0 0.0))
        (set! fog-distance INFINITY)))

;(set! fog-color (make-point 0.0 0.0 0.0))
;(set! fog-distance INFINITY)

    (let
      ((bounding-box
        (if scene-objects
          (get-bounding-box scene-objects (make-bounding-box '() '()))
          (make-bounding-box
            (make-point 0.0 0.0 0.0)
            (make-point 0.0 0.0 0.0)))))
      (set! octree
        (make-octree
          0
          scene-objects
          bounding-box)))))
