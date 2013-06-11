(define (make-camera perspective up location look-at view-angle)
  (let
    ((camera-location (make-point 0.0 0.0 0.0))
     (camera-direction (make-point 0.0 0.0 0.0))
     (down (make-point 0.0 0.0 0.0))
     (up-vec (make-point 0.0 0.0 0.0))
     (left-vec (make-point 0.0 0.0 0.0))
     (temp (make-point 0.0 0.0 0.0))
     (down-vec (make-point 0.0 0.0 0.0))
     (right-vec (make-point 0.0 0.0 0.0))
     (up-left-vec (make-point 0.0 0.0 0.0))
     (down-left-vec (make-point 0.0 0.0 0.0))
     (down-right-vec (make-point 0.0 0.0 0.0))
     (up-right-vec (make-point 0.0 0.0 0.0))
     (start-vec (make-point 0.0 0.0 0.0))
     (end-vec (make-point 0.0 0.0 0.0))
     (image-point (make-point 0.0 0.0 0.0))
     (col-delta (make-point 0.0 0.0 0.0))
     (row-delta (make-point 0.0 0.0 0.0)))
    (let*
      ((n-rows (exact->inexact ROW-SIZE))
       (n-cols (exact->inexact COL-SIZE))
       (orig-plane-distance 0.001)
       (aspect (/ (+ ROW-SIZE 0.0) (+ COL-SIZE 0.0)))
       (image-plane-factor
         (* orig-plane-distance (tan (* (/ view-angle 360.0) PI))))
       (camera-perspective perspective)
       (rad-angle (* (/ view-angle 360.0) 2.0 PI)))

      (case camera-perspective
        ((3) ; stereographic
          (set! rad-angle view-angle) ; really width of stereographic projection
          (set! image-plane-factor
            (* orig-plane-distance (tan (* (/ 179.99 360.0) PI)))))
        (else
          (begin
            (set! camera-location location)

            (v-! look-at location camera-direction)
            (set! camera-direction (normalize camera-direction))
            (v-scale! orig-plane-distance camera-direction)

            (if (= camera-perspective 0) ; 0 is pinhole perspective
              (v-scale! -1.0 up)) ; reverse up
            ; with fisheye up remains unreversed

            (set! down (v-scale (v-clone up) -1.0))

            (set! up-vec (perpendicular-to-plane up camera-direction))
            (v-scale! image-plane-factor up-vec)
            (v+! up-vec camera-direction up-vec)

            (set! temp (perpendicular-to-plane up camera-direction))
            (set! left-vec (perpendicular-to-plane temp camera-direction))
            (v-scale! image-plane-factor left-vec)
            (v-scale! aspect left-vec)
            (v+! left-vec camera-direction left-vec)

            (set! down-vec (perpendicular-to-plane down camera-direction))
            (v-scale! image-plane-factor down-vec)
            (v+! down-vec camera-direction down-vec)
      
            (set! temp (perpendicular-to-plane down camera-direction))
            (set! right-vec (perpendicular-to-plane temp camera-direction))
            (v-scale! image-plane-factor right-vec)
            (v-scale! aspect right-vec)
            (v+! right-vec camera-direction right-vec)
 
            (v+! up-vec left-vec temp)
            (v-! temp camera-direction up-left-vec)
 
            (v+! up-vec right-vec temp)
            (v-! temp camera-direction down-left-vec)
 
            (v+! down-vec right-vec temp)
            (v-! temp camera-direction down-right-vec)
 
            (v+! down-vec left-vec temp)
            (v-! temp camera-direction up-right-vec))))

        (set! col-delta
          (v-scale (v- up-right-vec up-left-vec) (/ 1.0 COL-SIZE)))
        (set! row-delta
          (v-scale (v- down-left-vec up-left-vec) (/ 1.0 ROW-SIZE)))

        (if (= camera-perspective 1) ; fisheye
          (set! fisheye
            (make-fisheye
              camera-location
              down-left-vec up-left-vec up-right-vec rad-angle)))

      (lambda (selector)
        (case selector
          ((location)
           (lambda ()
             location))
          ((direction)
           (lambda ()
             camera-direction))
          ((make-ray)
           (lambda (plot-height plot-width)
             (case camera-perspective
               ((2) ; equirectangular
                 (make-ray
                   camera-location
                   (equirectangular
                     (+ (/ plot-width n-cols) (jitter (/ 1.0 n-cols))) 
                     (+ (/ plot-height n-rows) (jitter (/ 1.0 n-rows)))
                     camera-direction))) 
               ((3) ; stereographic
                 (make-ray
                   camera-location
                   (stereographic
                     (+ (/ plot-width n-cols) (jitter (/ 1.0 n-cols))) 
                     (+ (/ plot-height n-rows) (jitter (/ 1.0 n-rows)))
                     rad-angle (* rad-angle (/ n-rows n-cols))
                     camera-direction)))
               (else
                 (begin
                   (if (= plot-width 0) ; start new row
                     (let
                       ((row-fraction (/ plot-height n-rows)))
                       (v-! down-left-vec up-left-vec start-vec)
                       (v-scale! row-fraction start-vec)
                       (v+! up-left-vec start-vec start-vec)
                       (v-! down-right-vec up-right-vec end-vec)
                       (v-scale! row-fraction end-vec)
                       (v+! up-right-vec end-vec end-vec)))
                   (let
                     ((col-fraction (/ plot-width n-cols)))
                     (v-! end-vec start-vec image-point)
                     (v-scale! col-fraction image-point)
                     (v+! start-vec image-point image-point)
                     (v+! image-point camera-location image-point))

                   ; add jitter
                   (v+! image-point
                     (v-scale col-delta (jitter 1.0)) image-point)
                   (v+! image-point 
                     (v-scale row-delta (jitter 1.0)) image-point)

                   (if (= camera-perspective 0) ; pinhole
                     (make-ray
                       camera-location
                       ((((camera-location 'direction-vector) image-point)
                         'zero-adjust)))
                     ; else fisheye
                     ((fisheye 'filter) camera-location image-point)))))))
          (else
           (render-error "make-camera" "Invalid selector: " selector)))))))
