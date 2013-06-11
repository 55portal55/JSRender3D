(define (sdl-error a b c)
  (display a) (display " ") (display b) (display c) (newline))

(define pi (* (asin 1.0) 2.0))

(define (get-clock)
  (let
  ; ((frame-data (read)))
    ((frame-data (list 0 1))) ; for animations
    (let
      ((frame (car frame-data))
       (frames (cadr frame-data)))
      (/ (exact->inexact frame) (exact->inexact frames)))))

(define (make-point x y z)
  (lambda (selector)
    (case selector
      ((get-x)
       (lambda () x))
      ((get-y)
       (lambda () y))
      ((get-z)
       (lambda () z))
      ((set-x)
       (lambda (a) (set! x a)))
      ((set-y)
       (lambda (a) (set! y a)))
      ((set-z)
       (lambda (a) (set! z a)))
      ((set)
       (lambda (a b c)
         (set! x a)
         (set! y b)
         (set! z c)))
      ((distance)
       (lambda (p2)
         (let*
           ((x2 ((p2 'get-x)))
            (y2 ((p2 'get-y)))
            (z2 ((p2 'get-z)))
            (x2-x1 (- x2 x))
            (y2-y1 (- y2 y))
            (z2-z1 (- z2 z)))
           (sqrt (+ (* x2-x1 x2-x1)
                    (* y2-y1 y2-y1)
                    (* z2-z1 z2-z1))))))
      ((direction-vector)
       (lambda (p2)
         (let*
           ((delta-x (- ((p2 'get-x)) x))
            (delta-y (- ((p2 'get-y)) y))
            (delta-z (- ((p2 'get-z)) z))
            (max-delta (max (abs delta-x) (abs delta-y) (abs delta-z))))
           (make-point (/ delta-x max-delta)
                     (/ delta-y max-delta)
                     (/ delta-z max-delta)))))
      ((zero-adjust)
       (lambda ()
         (let
           ((x x)
            (y y)
            (z z))
           (if (= x 0.0)
             (set! x (+ x EPSILON)))
           (if (= y 0.0)
             (set! y (+ y EPSILON)))
           (if (= z 0.0)
             (set! z (+ z EPSILON)))
           (make-point x y z))))
      ((log-data)
       (lambda ()
         (output x)
         (output y)
         (output z)))
      ((display)
       (lambda ()
         (display x) (display " ")
         (display y) (display " ")
         (display z)))
      (else
       (sdl-error "make-point" "Invalid selector: " selector)))))

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
      ((log-data)
       (lambda ()
         (output ambient)
         (output diffuse)
         (output specular)
         (output specular-N)))
      ((display)
       (lambda ()
         (display ambient) (display " ")
         (display diffuse) (display " ")
         (display specular) (display " ")
         (display specular-N)))
      (else
       (sdl-error "make-phong" "Invalid selector: " selector)))))

(define (make-shader color phong reflection no-shadow)
  (lambda (selector)
    (case selector
      ((get-color)
       (lambda ()
         color))
      ((get-phong)
       (lambda ()
         phong))
      ((get-reflection)
       (lambda ()
         reflection))
      ((get-no-shadow)
       (lambda ()
         no-shadow))
      ((log-data)
       (lambda ()
         ((color 'log-data))
         ((phong 'log-data))
         (output reflection)
         (if no-shadow
           (output 1)
           (output 0))
         (output 0) ; no decal image
         (output 0.0) ; latitude offset
         (output 0.0))) ; longitude offset
      ((display)
       (lambda ()
         ((color 'display)) (newline)
         ((phong 'display)) (newline)
         (display reflection) (newline)
         (if no-shadow
           (display 1)
           (display 0))
         (newline)
         (display 0) (newline) ; no decal image
         (display 0.0) (newline) ; latitude offset
         (display 0.0))) ; longitude offset
      (else
       (sdl-error "make-shader" "Invalid selector: " selector)))))

;;; image files database

(define image-files '())
(define image-file-count 0)

(define (string-assq s l)
  (if (null? l)
    '()
    (if (string=? s (caar l))
      (car l)
      (string-assq s (cdr l))))) 

(define (query-database s database)
  (let
    ((result (string-assq s database)))
    (if (not (null? result))
      (cadr result)
      '())))

(define (update-database s)
  (set! image-files (cons (list s image-file-count) image-files))
  (set! image-file-count (+ image-file-count 1))
  (query-database s image-files))

(define (display-chars l) ; TODO need output - log-data version
  (if (not (null? l))
    (begin
      (display (char->integer (car l)))
      (newline)
      (display-chars (cdr l)))))

(define (display-image-file file n) ; TODO need output - log-data version
  (let
    ((l (string->list (car file))))
    (display n)
    (newline)
    (display (length l))
    (newline)
    (display-chars l)))

(define (display-image-files files n) ; TODO need output - log-data version
  (if (> n 0)
    (begin
      (display-image-file (car files) (- n 1))
      (display-image-files (cdr files) (- n 1)))))

(define (cleanup)
  (let
    ((len (length image-files)))
    (if (> len 0)
      (begin
        (display 7) ; TODO need output - log-data version
        (newline)
        (display len)
        (newline)
        (display-image-files image-files len)
        (newline)))))
        
(define (make-decal-shader
          file-name phong reflection latitude-offset longitude-offset no-shadow)
  (let
    ((color (make-point 0.5 0.0 0.0)))
    (lambda (selector)
      (case selector
        ((get-color)
         (lambda ()
           color))
        ((get-phong)
         (lambda ()
           phong))
        ((get-reflection)
         (lambda ()
           reflection))
        ((get-no-shadow)
         (lambda ()
           no-shadow))
        ((log-data)
         (lambda ()
           ((color 'log-data))
           ((phong 'log-data))
           (output reflection)
           (if no-shadow
             (output 1)
             (output 0))
           (let
             ((image-idx (query-database file-name image-files)))
             (if (null? image-idx)
               (set! image-idx (update-database file-name)))
             (output (+ image-idx 1))) ; decal image
           (output latitude-offset) ; latitude offset
           (output longitude-offset))) ; longitude offset
        ((display)
         (lambda ()
           ((color 'display)) (newline)
           ((phong 'display)) (newline)
           (display reflection) (newline)
           (if no-shadow
             (display 1)
             (display 0))
           (newline)
           (let
             ((image-idx (query-database file-name image-files)))
             (if (null? image-idx)
               (set! image-idx (update-database file-name)))
             (display (+ image-idx 1)) (newline)) ; decal image
           (display latitude-offset) (newline) ; latitude offset
           (display longitude-offset))) ; longitude offset
        (else
         (sdl-error "make-decal-shader" "Invalid selector: " selector))))))

(define (make-camera perspective up location look-at angle)
  (output 1)
  (output perspective)
  ((up 'log-data))
  ((location 'log-data))
  ((look-at 'log-data))
  (output angle))

(define (background color)
  (output 0)
  ((color 'log-data)))

(define (make-light location color)
  (output 2)
  ((location 'log-data))
  ((color 'log-data)))

(define (make-triangle p1 p2 p3 shader)
  (output 3)
  ((p1 'log-data))
  ((p2 'log-data))
  ((p3 'log-data))
  ((shader 'log-data)))

(define (make-sphere center radius shader)
  (output 4)
  ((center 'log-data))
  (output radius)
  ((shader 'log-data)))

(define (make-fog color distance)
  (output 5)
  ((color 'log-data))
  (output distance))

(define (make-cylinder top bottom radius shader)
  (output 6)
  ((top 'log-data))
  ((bottom 'log-data))
  (output radius)
  ((shader 'log-data)))

(define (make-square p1 p2 p3 p4 shader)
  (make-triangle p2 p1 p4 shader)
  (make-triangle p3 p2 p4 shader))

(define (get-x p)
  ((p 'get-x)))

(define (get-y p)
  ((p 'get-y)))

(define (get-z p)
  ((p 'get-z)))

(define (vec-add p1 p2)
  (make-point
    (+ (get-x p1) (get-x p2))
    (+ (get-y p1) (get-y p2))
    (+ (get-z p1) (get-z p2))))

(define (vec-sub p1 p2)
  (make-point
    (- (get-x p1) (get-x p2))
    (- (get-y p1) (get-y p2))
    (- (get-z p1) (get-z p2))))

(define (vec-scale a X)
  (make-point
    (* (get-x X) a)
    (* (get-y X) a)
    (* (get-z X) a)))

(define (distance p1 p2)
  (let*
    ((x2 (get-x p2))
     (y2 (get-y p2))
     (z2 (get-z p2))
     (x1 (get-x p1))
     (y1 (get-y p1))
     (z1 (get-z p1))
     (x2-x1 (- x2 x1))
     (y2-y1 (- y2 y1))
     (z2-z1 (- z2 z1)))
    (sqrt (+ (* x2-x1 x2-x1)
             (* y2-y1 y2-y1)
             (* z2-z1 z2-z1)))))

(define (dot p1 p2)
  (+ (* (get-x p1) (get-x p2))
     (* (get-y p1) (get-y p2))
     (* (get-z p1) (get-z p2))))

(define (cross p1 p2)
  (make-point
    (- (* (get-y p1) (get-z p2)) (* (get-z p1) (get-y p2)))
    (- (* (get-z p1) (get-x p2)) (* (get-x p1) (get-z p2)))
    (- (* (get-x p1) (get-y p2)) (* (get-y p1) (get-x p2)))))

(define (zero-adjust x)
  (if (= x 0.0)
    0.00000000001
    x))

(define (perpendicular u v)
  (vec-sub
    v
    (cross
      (vec-scale
        (/ 1.0 (dot u u))
        (vec-sub v u))
      u)))

(define (normalize p)
  (let
    ((a (get-x p))
     (b (get-y p))
     (c (get-z p)))
    (let
      ((mag (zero-adjust (sqrt (+ (* a a) (* b b) (* c c))))))
      (make-point
        (/ a mag)
        (/ b mag)
        (/ c mag)))))

(define (arg x y)
  (if (> x 0.0)
    (atan (/ y x))
    (if (< x 0.0)
      (if (>= y 0.0)
        (+ (atan (/ y x) pi))
        (- (atan (/ y x) pi)))
      ; x == 0.0
      (if (> y 0.0)
        (/ pi 2.0)
        (if (< y 0.0)
          (- (/ pi 2.0))
          ; y == 0.0
          pi))))) ; really undefined

(define (rotate r phi axis)
  (let*
    ((phi (* (/ phi 360.0) 2.0 pi))
     (cos-phi (cos phi))
     (n (normalize axis)))
    (vec-add
      (vec-scale cos-phi r)
      (vec-add
        (vec-scale (* (dot n r) (- 1.0 cos-phi)) n)
        (vec-scale (sin phi) (cross r n))))))

(define (rotate-x p ang) ; TODO this has a bug in it
  (make-point
    (- (* (get-z p) (cos ang)) (* (get-y p) (sin ang)))
    (get-y p)
    (+ (* (get-y p) (cos ang)) (* (get-y p) (sin ang)))))

(define (rotate-y p ang)
  (make-point
    (- (* (get-x p) (cos ang)) (* (get-z p) (sin ang)))
    (get-y p)
    (+ (* (get-z p) (cos ang)) (* (get-x p) (sin ang)))))

(define (rotate-z p ang)
  (make-point
    (- (* (get-x p) (cos ang)) (* (get-y p) (sin ang)))
    (+ (* (get-y p) (cos ang)) (* (get-x p) (sin ang)))
    (get-z p)))

(define (smooth-flow start stop)
  (let
    ((delta (vec-sub stop start)))
    (vec-add start (vec-scale (* 0.5 (- 1.0 (cos (* pi clock)))) delta))))

(define random-seed 1)

(define (random)
  (set! random-seed (+ (* random-seed 1103515245) 12345))
  (/ (abs (remainder (quotient random-seed 65536) 32768)) 32768.0)) 

(define (random-range a b)
  (let
    ((delta (- b a)))
    (+ a (* (random) delta))))
