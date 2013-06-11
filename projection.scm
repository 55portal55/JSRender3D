(define (equirectangular v u camera-direction)
  (let
    ((theta (* u PI))
     (phi (* v 2.0 PI)))
    (let
      ((x (* (sin theta) (cos phi)))
       (y (* (sin theta) (sin phi)))
       (z (cos theta)))
      ((camera-direction 'set) x z y)
      camera-direction)))

(define (stereographic
   X Y width height camera-direction)
  (set! X (- (* X width) (/ width 2.0)))
  (set! Y (- (* Y height) (/ height 2.0)))
  (let
    ((x (/ (* 2.0 X) (+ 1.0 (* X X) (* Y Y))))
     (y (/ (* 2.0 Y) (+ 1.0 (* X X) (* Y Y))))
     (z (/ (+ -1.0 (* X X) (* Y Y)) (+ 1.0 (* X X) (* Y Y)))))
    ((camera-direction 'set) x z y)
    camera-direction))
