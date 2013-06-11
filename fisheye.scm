(define (make-fisheye camera-location down-left-vec up-left-vec up-right-vec rad-angle)
  (let
    ((dir-x 0.0)
     (dir-y 0.0)
     (dir-z 0.0)
     (pi (* (asin 1.0) 2.0))
     (focal-length 0.0)
     (radius 0.0)
     (diagonal-length 0.0)
     (fisheye-up 0.0)
     (temp (make-point 0.0 0.0 0.0))
     (image-plane-center (make-point 0.0 0.0 0.0))
     (fisheye-intersection-point (make-point 0.0 0.0 0.0))
     (fisheye-center (make-point 0.0 0.0 0.0))
     (image-plane (make-plane down-left-vec up-left-vec up-right-vec '())))
    (set! dir-x ((((image-plane 'get-normal)) 'get-x)))
    (set! dir-y ((((image-plane 'get-normal)) 'get-y)))
    (set! dir-z ((((image-plane 'get-normal)) 'get-z)))
    (set! image-plane-center
      ((image-plane 'intersect-ray)
        camera-location dir-x dir-y dir-z #f)) 
    (set! diagonal-length ((image-plane-center 'distance) down-left-vec))
    (set! focal-length (tan (/ (- pi rad-angle) 2.0))) ; rad-angle from camera
    ((temp 'set) dir-x dir-y dir-z)
    (v-scale! focal-length temp)
    (v-scale! diagonal-length temp)
    (v+! image-plane-center temp fisheye-center)
    (set! radius (sqrt (+ (* diagonal-length diagonal-length)
      (* ((image-plane-center 'distance) fisheye-center)
         ((image-plane-center 'distance) fisheye-center)))))
    ((temp 'set) 0.0 0.0 1.0) ; determine "up"ness of camera
    (let
      ((fisheye-sphere (make-sphere-proper fisheye-center radius '())))
      (set! fisheye-up (dot temp ((image-plane 'get-normal))))
    
      (lambda (selector)
        (case selector
          ((filter)
            (lambda (location image-point)
              (set! fisheye-intersection-point
                ((fisheye-sphere 'intersect-ray-fisheye)
                  image-point dir-x dir-y dir-z fisheye-up)) 
              ; TODO following if stmt a kludge that doesnt appear in CRender
              (if (null? fisheye-intersection-point)
                (set! fisheye-intersection-point image-point))
              (make-ray
                location
                ((fisheye-sphere 'get-normal) fisheye-intersection-point))))
           (else
            (render-error "make-fisheye" "Invalid selector: " selector)))))))
