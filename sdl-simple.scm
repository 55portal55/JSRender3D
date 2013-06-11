; a simple scene

(define clock (get-clock))

(define object-phong
  (make-phong
    0.25 ; ambient
    0.2 ; diffuse
    0.3 ; specular
    15)) ; specular-N

(define ground-phong
  (make-phong
    0.75 ; ambient
    0.3 ; diffuse
    0.0 ; specular
    0)) ; specular-N

(define red
  (make-shader
    (make-point 0.9999 0.0 0.0) ; color
    object-phong
    0.75 ; reflection
    #f)) ; shadows
(define purple
  (make-shader
    (make-point 0.4 0.0 0.9999) ; color
    object-phong
    0.75 ; reflection
    #f)) ; shadows
(define orange
  (make-shader
    (make-point 0.9999 0.4 0.0) ; color
    object-phong
    0.75 ; reflection
    #f)) ; shadows
(define turquoise
  (make-shader
    (make-point 0.0 0.9999 0.6666) ; color
    object-phong
    0.75 ; reflection
    #f)) ; shadows
(define sky-blue (make-point 0.5 0.55 0.9999))
(define white (make-point 0.9999 0.9999 0.9999))
(define green
  (make-shader
    (make-point 0.0 0.9999 0.0) ; color
    ground-phong
    0.0 ; reflection
    #f)) ; shadows
(define yellow
  (make-shader
    (make-point 0.9999 0.9999 0.0) ; color
    ground-phong
    0.0 ; reflection
    #f)) ; shadows

; make checkered tile the brute force way - need a checker shader attribute
(define (make-tile
  a1 a2 b1 b2 c1 c2 d1 d2 e1 e2 f1 f2
  g1 g2 h1 h2 i1 i2 shader-1 shader-2)
  (make-square
    (make-point e1 0.0 e2)
    (make-point b1 0.0 b2)
    (make-point a1 0.0 a2)
    (make-point d1 0.0 d2)
    shader-1)
  (make-square
    (make-point f1 0.0 f2)
    (make-point c1 0.0 c2)
    (make-point b1 0.0 b2)
    (make-point e1 0.0 e2)
    shader-2)
  (make-square
    (make-point h1 0.0 h2)
    (make-point e1 0.0 e2)
    (make-point d1 0.0 d2)
    (make-point g1 0.0 g2)
    shader-2)
  (make-square
    (make-point i1 0.0 i2)
    (make-point f1 0.0 f2)
    (make-point e1 0.0 e2)
    (make-point h1 0.0 h2)
    shader-1))

(define (make-checkered-grid tiles-on-a-side shader-1 shader-2)
  (let 
    ((j-offset -15.0)
     (j-size 30.0)
     (i-offset -15.0)
     (i-size 30.0)
     (tile-size (/ 30.0 tiles-on-a-side)))
    (do ((j 0 (+ j 1))) ((= j tiles-on-a-side))
      (do ((i 0 (+ i 1))) ((= i tiles-on-a-side))
        (make-tile
          (+ j-offset (* j tile-size))
          (+ i-offset (* i tile-size))
          (+ j-offset (* j tile-size) (/ tile-size 2.0))
          (+ i-offset (* i tile-size))
          (+ j-offset (* j tile-size) tile-size)
          (+ i-offset (* i tile-size))

          (+ j-offset (* j tile-size))
          (+ i-offset (* i tile-size) (/ tile-size 2.0))
          (+ j-offset (* j tile-size) (/ tile-size 2.0))
          (+ i-offset (* i tile-size) (/ tile-size 2.0))
          (+ j-offset (* j tile-size) tile-size)
          (+ i-offset (* i tile-size) (/ tile-size 2.0))

          (+ j-offset (* j tile-size))
          (+ i-offset (* i tile-size) tile-size)
          (+ j-offset (* j tile-size) (/ tile-size 2.0))
          (+ i-offset (* i tile-size) tile-size)
          (+ j-offset (* j tile-size) tile-size)
          (+ i-offset (* i tile-size) tile-size)

          shader-1 shader-2)))))

(define (make-icosahedron location shader)
  (let*
    ((t (/ (+ 1.0 (sqrt 5.0)) 2.0))
     (t2 (/ 1.0 (sqrt (+ 1.0 (* t t)))))
     (a (* (+ 0.65 clock) 2.0 pi)))
    (let
      ((v0 (vec-add location
         (rotate-y (vec-scale t2 (make-point t 1.0 0.0)) a)))
       (v1 (vec-add location
         (rotate-y (vec-scale t2 (make-point (- t) 1.0 0.0)) a)))
       (v2 (vec-add location
         (rotate-y (vec-scale t2 (make-point t -1.0 0.0)) a)))
       (v3 (vec-add location
         (rotate-y (vec-scale t2 (make-point (- t) -1.0 0.0)) a)))
       (v4 (vec-add location
         (rotate-y (vec-scale t2 (make-point 1.0 0.0 t)) a)))
       (v5 (vec-add location
         (rotate-y (vec-scale t2 (make-point 1.0 0.0 (- t))) a)))
       (v6 (vec-add location
         (rotate-y (vec-scale t2 (make-point -1.0 0.0 t)) a)))
       (v7 (vec-add location
         (rotate-y (vec-scale t2 (make-point -1.0 0.0 (- t))) a)))
       (v8 (vec-add location
         (rotate-y (vec-scale t2 (make-point 0.0 t 1.0)) a)))
       (v9 (vec-add location
         (rotate-y (vec-scale t2 (make-point 0.0 (- t) 1.0)) a)))
       (v10 (vec-add location
         (rotate-y (vec-scale t2 (make-point 0.0 t -1.0)) a)))
       (v11 (vec-add location
         (rotate-y (vec-scale t2 (make-point 0.0 (- t) -1.0)) a))))
      (make-triangle v4 v8 v0 shader) 
      (make-triangle v10 v5 v0 shader) 
      (make-triangle v9 v4 v2 shader) 
      (make-triangle v5 v11 v2 shader) 
      (make-triangle v8 v6 v1 shader) 
      (make-triangle v7 v10 v1 shader) 
      (make-triangle v6 v9 v3 shader) 
      (make-triangle v11 v7 v3 shader) 
      (make-triangle v8 v10 v0 shader) 
      (make-triangle v10 v8 v1 shader) 
      (make-triangle v11 v9 v2 shader) 
      (make-triangle v11 v9 v3 shader) 
      (make-triangle v0 v2 v4 shader) 
      (make-triangle v2 v0 v5 shader) 
      (make-triangle v3 v1 v6 shader) 
      (make-triangle v1 v3 v7 shader) 
      (make-triangle v4 v6 v8 shader) 
      (make-triangle v6 v4 v9 shader) 
      (make-triangle v7 v5 v10 shader) 
      (make-triangle v5 v7 v11 shader)))) 

(background sky-blue) ; sky

(make-checkered-grid 15 green yellow) ; ground

(let
  ((up (make-point 0.0 1.0 0.0))
   (look-at (make-point 0.0 0.0 0.0))
   (location (make-point 0.0 1.0 -12.0)))
  (make-camera
    0 ; normal perspective
;   1 ; fisheye perspective
    up
    location
    look-at
    45.0)) ; angle

(make-light
  (make-point 1000.0 1000.0 -1000.0) ; location
  white)

(make-fog
  sky-blue ; color
  25.0) ; distance

(make-sphere
  (make-point -2.0 0.8 -4.5) ; center
  0.8 ; radius
  red)

(make-cylinder
  (make-point 0.5 1.7 -2.6) ; top cap
  (make-point 0.0 -1.0 -2.0) ; bottom cap
  0.5 ; radius
  purple)

(make-cylinder
  (make-point -0.5 0.25 -7.0) ; top cap
  (make-point 0.5 0.25 -9.5) ; bottom cap
  0.25 ; radius
  turquoise)

(make-icosahedron (make-point 2.0 0.6 -4.2) orange)
