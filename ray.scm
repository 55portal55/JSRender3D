(define (make-ray origin direction)
  (let*
    ((dir-x ((direction 'get-x)))
     (dir-y ((direction 'get-y)))
     (dir-z ((direction 'get-z)))
     (inverse-direction
       (make-point
         (/ 1.0 dir-x)
         (/ 1.0 dir-y)
         (/ 1.0 dir-z)))
     (ibyj (* dir-x ((inverse-direction 'get-y))))
     (jbyi (* dir-y ((inverse-direction 'get-x))))
     (jbyk (* dir-y ((inverse-direction 'get-z))))
     (kbyj (* dir-z ((inverse-direction 'get-y))))
     (ibyk (* dir-x ((inverse-direction 'get-z))))
     (kbyi (* dir-z ((inverse-direction 'get-x))))
     (xy (- ((origin 'get-y)) (* jbyi ((origin 'get-x)))))
     (xz (- ((origin 'get-z)) (* kbyi ((origin 'get-x)))))
     (yx (- ((origin 'get-x)) (* ibyj ((origin 'get-y)))))
     (yz (- ((origin 'get-z)) (* kbyj ((origin 'get-y)))))
     (zx (- ((origin 'get-x)) (* ibyk ((origin 'get-z)))))
     (zy (- ((origin 'get-y)) (* jbyk ((origin 'get-z)))))
     (classification
       (if (< dir-x 0.0)
         (if (< dir-y 0.0)
           (if (< dir-z 0.0)
             'MMM
             (if (> dir-z 0.0)
               'MMP
               'MMO)) ; dir-z >= 0.0
           (if (< dir-z 0.0) ; dir-y >= 0.0
             (if (= dir-y 0.0)
               'MOM
               'MPM)
             (if (and (= dir-y 0.0) (= dir-z 0.0)) ; dir-z >= 0.0
               'MOO
               (if (= dir-z 0.0)
                 'MPO
                 (if (= dir-y 0.0)
                   'MOP
                   'MPP)))))
         (if (< dir-y 0.0) ; dir-x >= 0.0
           (if (< dir-z 0.0)
             (if (= dir-x 0.0)
               'OMM
               'PMM)
             (if (and (= dir-x 0.0) (= dir-z 0.0)) ; dir-z >= 0.0
               'OMO
               (if (= dir-z 0.0)
                 'PMO
                 (if (= dir-x 0.0)
                   'OMP
                   'PMP))))
           (if (< dir-z 0.0) ; dir-y >= 0.0
             (if (and (= dir-x 0.0) (= dir-y 0.0))
               'OOM
               (if (= dir-x 0.0)
                 'OPM
                 (if (= dir-y 0.0)
                   'POM
                   'PPM)))
             (if (= dir-x 0.0) ; dir-z > 0.0
               (if (= dir-y 0.0)
                 'OOP
                 (if (= dir-z 0.0)
                   'OPO
                   'OPP))
               (if (and (= dir-y 0.0) (= dir-z 0.0))
                 'POO
                 (if (= dir-y 0.0)
                   'POP
                   (if (= dir-z 0.0)
                     'PPO
                     'PPP))))))))
    (classification-ffi-version (cadr (assq classification '(
      (MMM 0)
      (MMP 1)
      (MPM 2)
      (MPP 3)
      (PMM 4)
      (PMP 5)
      (PPM 6)
      (PPP 7)
      (POO 8)
      (MOO 9)
      (OPO 10)
      (OMO 11)
      (OOP 12)
      (OOM 13)
      (OMM 14)
      (OMP 15)
      (OPM 16)
      (OPP 17)
      (MOM 18)
      (MOP 19)
      (POM 20)
      (POP 21)
      (MMO 22)
      (MPO 23)
      (PMO 24)
      (PPO 25)))))
    )
;(display classification-ffi-version) (newline)
    (lambda (selector)
      (case selector
        ((get-origin)
         (lambda () origin))
        ((get-dir-x)
         (lambda () dir-x))
        ((get-dir-y)
         (lambda () dir-y))
        ((get-dir-z)
         (lambda () dir-z))
        ((get-classification)
         (lambda () classification))
;       ((get-classification-ffi-version)
;        (lambda () classification-ffi-version))
        ((get-direction)
         (lambda () direction))
        ((get-inverse-direction)
         (lambda () inverse-direction))
        ((get-ibyj)
         (lambda () ibyj))
        ((get-jbyi)
         (lambda () jbyi))
        ((get-jbyk)
         (lambda () jbyk))
        ((get-kbyj)
         (lambda () kbyj))
        ((get-ibyk)
         (lambda () ibyk))
        ((get-kbyi)
         (lambda () kbyi))
        ((get-xy)
         (lambda () xy))
        ((get-xz)
         (lambda () xz))
        ((get-yx)
         (lambda () yx))
        ((get-yz)
         (lambda () yz))
        ((get-zx)
         (lambda () zx))
        ((get-zy)
         (lambda () zy))
        (else
         (render-error "make-ray" "Invalid selector: " selector))))))
