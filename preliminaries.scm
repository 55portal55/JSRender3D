; global vars

(define (render-error a b c)
  (display a) (display " ") (display b) (display c) (newline))

(define fisheye '()) ; TODO fold into camera class

(define octree '())
(define intersection-tests 0)

(define pixel-color (make-point 0.0 0.0 0.0))

(define scene-background '())
(define lights '())
(define scene-objects '())
(define scene-fog '())
(define fog-color '())
(define fog-distance '())

; camera

(define camera '())

; initilize global vars

(define (init-globals)
  (set! fisheye '())

  (set! octree '())
  (set! intersection-tests 0);

  (set! pixel-color (make-point 0.0 0.0 0.0))

  (set! scene-background '())
  (set! lights '())
  (set! scene-objects '())
  (set! scene-fog '())
  (set! fog-color '())
  (set! fog-distance '()))

; used to initialize scene bounds
(define (get-bounding-box scene-objects max-bounding-box)
  (if (null? scene-objects)
    max-bounding-box
    (let*
      ((scene-object (car scene-objects))
       (bounding-box ((scene-object 'get-bounding-box))))
      ((max-bounding-box 'update-max) bounding-box)
      (get-bounding-box (cdr scene-objects) max-bounding-box))))

(define (shadow no-shadow origin light-location)
  (if no-shadow
    #f
    ((octree 'shadow-ray-traverse-octree)
      0
      (make-ray origin ; shadow ray
        ((((origin 'direction-vector) light-location) 'zero-adjust)))
      INFINITY))) ; TODO use light distance

(define (pow x y)
  (if (= y 0)
    1.0
    (* x (pow x (- y 1)))))

(define (clip-0 x)
  (if (< x 0.0)
    0.0
    x))

(define (phong
  ray object-color object-phong object-normal light light-direction n-lights)
  (let
    ((ambient ((object-phong 'get-ambient)))
     (diffuse ((object-phong 'get-diffuse)))
     (specular ((object-phong 'get-specular)))
     (specular-N ((object-phong 'get-specular-N))))
    (let
      ((V (normalize ((ray 'get-direction))))
       (c-diffuse (* diffuse (dot object-normal light-direction))))
      (if (< c-diffuse 0.0)
        (set! c-diffuse 0.0))
      (let*
        ((L-dot-N (dot light-direction object-normal))
         (light-reflection
           (v- (v-scale object-normal (* 2.0 L-dot-N)) light-direction))
         (c-specular (* specular (pow (clip-0 (dot light-reflection V)) specular-N)))
  ;      (c-specular (* specular 1.0))
         (temp (v-scale ((light 'color)) c-specular))
         (c-ambient (* (/ 1.0 n-lights) ambient))
         (light-color (v-scale ((light 'color)) c-diffuse))
         (temp2 (v-scale object-color c-ambient)))
        (v+ (v+ temp light-color) temp2))))) ; returns light color)

(define (get-light-direction light intersection-point)
  (let
    ((location ((light 'location))))
    (normalize (v- intersection-point location))))

(define (get-light-color
  no-shadow
  light n-lights intersection-point
  ray object-phong object-color object-normal)
  (if (shadow
        no-shadow
        intersection-point ((light 'location)))
    (v-scale object-color (* ((object-phong 'get-ambient)) (/ 1.0 n-lights)))
    (let
      ((light-direction (get-light-direction light intersection-point)))
      (phong
        ray object-color object-phong object-normal
        light light-direction n-lights))))

(define (get-color-of-all-lights
  no-shadow
  n-lights lights intersection-point
  ray object-phong object-color object-normal sum)
  (letrec (
    (get-color-of-all-lights-proper
      (lambda (
        no-shadow
        n-lights lights intersection-point
        ray object-phong object-color object-normal sum)
        (if (null? lights)
          (clip-upper sum)
          (let
            ((sum (v+ sum (get-light-color
                            no-shadow
                            (car lights)
                            n-lights
                            intersection-point
                            ray object-phong object-color object-normal))))
            (get-color-of-all-lights-proper
              no-shadow
              n-lights (cdr lights) intersection-point
              ray object-phong object-color object-normal sum))))))
    (if (null? lights) ; no lights assume ambient color of object
      object-color
      (get-color-of-all-lights-proper
        no-shadow
        n-lights lights intersection-point
        ray object-phong object-color object-normal sum))))

; reflect an incident direction vector aboubt a normal
(define (reflect direction normal)
  (v- direction (v-scale normal (* 2.0 (dot direction normal)))))

; fresnel metallic reflectance
(define (reflectance fraction ang)
  (let
    ((metal-fresnal 0.8))
    (* fraction
       (+ metal-fresnal (* (- 1.0 metal-fresnal) (- 1.0 (pow ang 5)))))))

(define (process-reflection recursion-level location normal direction)
  (set! normal (v-scale normal -1.0)) ; reverse direction of normal
  (set! direction (reflect direction normal))
  (set! location (v+ location (v-scale direction (* 20.0 EPSILON)))) ; get off surface - multiplying by 20 gets rid of some jitteriness
  (let
    ((ray (make-ray location direction)))
    (let
      ((intersection-object (make-intersection-object)))
      ((octree 'ray-traverse-octree)
        0
        ray
        direction
        intersection-object)
      (let
        ((pixel-color
         (get-pixel-color
           (+ recursion-level 1)
           ((intersection-object 'get-distance))
           ((intersection-object 'get-scene-object))
           ((intersection-object 'get-min-intersection-point))
           ray)))
        (apply-fog
          ((intersection-object 'get-distance))
          pixel-color)))))

(define (get-pixel-color
  recursion-level
  distance scene-object intersection-point ray)
  (if (= distance INFINITY)
    ((scene-background 'get-background))
    (let
      ((shader ((scene-object 'get-shader))))
      (let
        ((object-color ((shader 'get-color)))
         (object-phong ((shader 'get-phong)))
         (object-reflection ((shader 'get-reflection)))
         (no-shadow ((shader 'get-no-shadow)))
         (normal ((scene-object 'get-normal) intersection-point)))
        (if (or (= object-reflection 0.0) ; no reflection
                (= recursion-level 20)) ; set limit on number of reflections
          (get-color-of-all-lights
            no-shadow
            (length lights)
            lights
            intersection-point
            ray object-phong object-color normal (make-point 0.0 0.0 0.0))
          (clip-upper
            (v+
              (v-scale
                (process-reflection
                  recursion-level
                  intersection-point normal ((ray 'get-direction)))
                (reflectance
                  object-reflection
                  (dot normal (normalize ((ray 'get-direction))))))
            (get-color-of-all-lights
              no-shadow
              (length lights)
              lights
              intersection-point
              ray object-phong object-color normal (make-point 0.0 0.0 0.0)))))))))

(define (apply-fog distance color)
  (if (= fog-distance INFINITY)
    color
    (if (>= distance fog-distance)
      fog-color
      (let*
        ((fraction (/ distance fog-distance)))
        (v+ (v-scale color (- 1.0 fraction)) (v-scale fog-color fraction))))))

(define (shade value)
  (inexact->exact (floor (* 256.0 value))))

(define (color-to-int color)
  (let
    ((x (shade ((color 'get-x))))
     (y (shade ((color 'get-y))))
     (z (shade ((color 'get-z)))))
    (+ (* (+ (* z 256) y) 256) x)))
