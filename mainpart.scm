
;;; Author: Rick Miskowski - www.richardmiskowski.com

(define (render)
  (init-globals)

  (init-scene)

  (write-dimensions)

  (do ((i 0 (+ i 1))) ((= i ROW-SIZE))
    (do ((j 0 (+ j 1))) ((= j COL-SIZE))
      (let
        ; TODO make-ray: use direction not image-point
        ((ray ((camera 'make-ray) i j)))
        (let
          ((intersection-object (make-intersection-object)))
          ((octree 'ray-traverse-octree)
            0
            ray
            ((camera 'direction))
            intersection-object)
          (set! pixel-color
            (get-pixel-color
              0 ; recursion level
              ((intersection-object 'get-distance))
              ((intersection-object 'get-scene-object))
              ((intersection-object 'get-min-intersection-point))
              ray))
          (plot
            j i
            (apply-fog
              ((intersection-object 'get-distance))
              pixel-color)))))))

(define (runRender)
  (set! number-list '())
  (scheme)
  (if (not scheme-interpretor-error?)
    (begin
      (set! number-list (reverse number-list))
      (render))))

;(display intersection-tests) (newline)
