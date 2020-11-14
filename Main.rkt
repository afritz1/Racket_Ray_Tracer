#lang racket

(require racket/flonum ; For fl functions.
         racket/unsafe/ops) ; For unsafe functions.

; Aaron's new Racket Ray Tracer, 5/6/2015.
; This one is less like my C++ ray tracers and more
; like my C ray tracers. Faster vector math, too!

; Consider unsafe functions! (unsafe-[function-name] ...)
; Executables do run faster than DrRacket by ~5%.

;--------------------------
; Constants
;--------------------------

(define EPSILON 0.0001)
(define PI 3.1415926535897932)
(define MAX-FLOAT64 1.797693134e+308)
(define MAX-FLOAT32 3.402823466e+38)
(define INIT-DEPTH 1)

(define SCREEN-WIDTH 640)
(define SCREEN-HEIGHT 480)
(define SCREEN-ASPECT 
  (exact->inexact (/ SCREEN-WIDTH SCREEN-HEIGHT)))

(define SUPER-SAMPLES 1)

(define WORLD-SIZE 12.0)

(define SPHERE-COUNT 16)
(define CUBOID-COUNT 0)

(define POINT-LIGHT-COUNT 0)
(define DISTANT-LIGHT-COUNT 0)
(define AREA-LIGHT-COUNT 2)

(define LIGHT-SAMPLES 4)
(define MAX-RECURSIONS 5)

;--------------------------
; Camera
;--------------------------

(struct camera (eye fwd rgt up))

(define (look-at eye focus up fovY aspect)
  (let* ((fovY-to-rad (unsafe-fl* fovY (unsafe-fl/ PI 180.0)))
         (img-half-h (unsafe-fltan (unsafe-fl/ fovY-to-rad 2.0)))
         (img-half-w (unsafe-fl* img-half-h aspect))
         (new-fwd (normalize (sub-v focus eye)))
         (new-rgt (scale-by-m (normalize (cross new-fwd up)) img-half-w))
         (new-up (scale-by-m (normalize (cross new-fwd new-rgt)) img-half-h)))
    (camera eye new-fwd new-rgt new-up)))

(define (image-ray camera xx yy)
  (ray (camera-eye camera)
       (let ((a (add-v (camera-eye camera) (camera-fwd camera)))
             (b (add-v (negate (camera-rgt camera)) (camera-up camera)))
             (c (scale-by-m (camera-rgt camera) (unsafe-fl* 2.0 xx)))
             (d (scale-by-m (negate (camera-up camera)) (unsafe-fl* 2.0 yy))))
         (normalize (sub-v (add-v a (add-v b (add-v c d))) 
                           (camera-eye camera))))
       INIT-DEPTH))

;--------------------------
; Custom errors
;--------------------------

(define (not-implemented-error name)
  (error (format "~s is not implemented yet." name)))

;--------------------------
; Lights
; -> Treat like C structs + function pointers.
;--------------------------

(struct light (color samples derived fn))
(struct point-light (point))
(struct distant-light (direction))
(struct area-light (point radius))

; Point Light
(define (make-point-light)
  (light (rand-color) 1
         (point-light (rand-point WORLD-SIZE))
         get-point-light-direction))

(define (get-point-light-direction light shape-point)
  (normalize (sub-v (point-light-point (light-derived light)) 
                    shape-point)))

; Distant Light
(define (make-distant-light)
  (light (rand-color) 1
         (distant-light (normalize (rand-point 1.0)))
         get-distant-light-direction))

(define (get-distant-light-direction light shape-point)
  (distant-light-direction (light-derived light)))

; Area Light
(define (make-area-light)
  (light (rand-color) LIGHT-SAMPLES
         (area-light (rand-point WORLD-SIZE) 
                     (+ 1.0 (random)))
         get-area-light-direction))

(define (get-area-light-direction light shape-point)
  (let* ((point (area-light-point (light-derived light)))
         (radius (area-light-radius (light-derived light)))
         (diff (scale-by-v 
                (normalize 
                 (flvector (unsafe-fl- (unsafe-fl* (random) 2.0) 1.0)
                           (unsafe-fl- (unsafe-fl* (random) 2.0) 1.0)
                           (unsafe-fl- (unsafe-fl* (random) 2.0) 1.0)))
                (flvector (unsafe-fl* radius (random))
                          (unsafe-fl* radius (random))
                          (unsafe-fl* radius (random))))))
    (normalize (sub-v (add-v point diff) shape-point))))

;--------------------------
; Materials
;--------------------------

(struct material (shadows derived fn))
(struct flat (color))
(struct phong (color specular-color 
                     ambient diffuse 
                     specular shiny 
                     reflective))

; Flat Material
(define (make-flat-material)
  (material #f 
            (flat (rand-color)) 
            get-flat-color-at))

(define (get-flat-color-at material point normal ray world)
  (flat-color (material-derived material)))

; Phong Material
(define (make-phong-material)
  (material #t
            (phong (rand-color) (flvector 1.0 1.0 1.0)
                   0.20 1.0
                   0.50 16.0
                   #t)
            get-phong-color-at))

(define (get-phong-color-at material point normal view-ray world)
  (let* ((phong (material-derived material))
         (vn-dot (dot-v (negate (ray-direction view-ray)) normal))
         (vn-sign (if (unsafe-fl> vn-dot 0.0) 
                      1.0 
                      (if (unsafe-fl< vn-dot 0.0) -1.0 0.0)))
         (local-normal (scale-by-m normal vn-sign)))
    ; Returned color = Ambient + Reflection + Light.
    (add-v 
     ; Ambient
     (scale-by-m (phong-color phong) (phong-ambient phong))
     (add-v            
      ; Reflection
      (if (and (phong-reflective phong)
               (unsafe-fx<= (ray-depth view-ray) MAX-RECURSIONS))
          (let* ((vn-reflect (normalize (reflect (negate (ray-direction view-ray))
                                                 local-normal)))
                 (reflect-ray
                  (ray (add-v point (scale-by-m vn-reflect EPSILON))
                       vn-reflect
                       (unsafe-fx+ (ray-depth view-ray) 1)))
                 (reflect-try (closest-hit reflect-ray (world-shapes world))))
            (if (unsafe-fl>= (intersection-t reflect-try) MAX-FLOAT32)
                (scale-by-m (world-back-color world) (phong-specular phong))
                (scale-by-m ((material-fn (shape-material (intersection-shape reflect-try)))
                             (shape-material (intersection-shape reflect-try))
                             (intersection-point reflect-try)
                             (intersection-normal reflect-try)
                             reflect-ray
                             world)
                            (phong-specular phong))))
          (flvector 0.0 0.0 0.0)) ; If no reflection.
      ; Light
      (let light-loop ((lights (world-lights world))
                       (total-light (flvector 0.0 0.0 0.0)))
        (if (null? lights) 
            total-light ; Added to the total Phong color.
            (light-loop 
             (cdr lights)
             (add-v 
              total-light
              (scale-by-m 
               (let* ((light (car lights))
                      (amb-and-diff 
                       (scale-by-v (phong-color phong)
                                   (light-color light))))
                 (let sample-loop ((sampled-light (flvector 0.0 0.0 0.0))
                                   (samples-done 0))
                   (if (unsafe-fx= samples-done (light-samples light))
                       sampled-light
                       (let* ((light-dir ((light-fn light) light point))
                              (shadow-ray 
                               (ray (add-v point (scale-by-m light-dir EPSILON))
                                    light-dir
                                    INIT-DEPTH))
                              (ln-dot (dot-v light-dir local-normal))
                              (ln-reflect (normalize (reflect light-dir local-normal)))
                              (ln-rflct-v-dot (dot-v ln-reflect (negate (ray-direction view-ray))))
                              (shadow-try (closest-hit shadow-ray (world-shapes world))))                       
                         (sample-loop
                          (add-v sampled-light
                                 (if (or (unsafe-fl>= (intersection-t shadow-try) MAX-FLOAT32)
                                         (not (material-shadows (shape-material 
                                                                 (intersection-shape shadow-try)))))
                                     (add-v (scale-by-m (scale-by-m amb-and-diff 
                                                                    (phong-diffuse phong))
                                                        (unsafe-flmax 0.0 ln-dot))
                                            (scale-by-m (scale-by-m (scale-by-v (light-color light)
                                                                                (phong-specular-color phong))
                                                                    (phong-specular phong))
                                                        (unsafe-flexpt (unsafe-flmax 0.0 ln-rflct-v-dot)
                                                                       (phong-shiny phong))))
                                     (flvector 0.0 0.0 0.0)))
                          (+ samples-done 1)))))) (/ 1.0 (light-samples (car lights))))))))))))

;--------------------------
; Ray
;--------------------------

(struct ray (point direction depth))

(define (point-at ray distance)
  (add-v (ray-point ray) (scale-by-m (ray-direction ray) distance)))

(define (closest-hit ray shape-list)
  (let loop ((nearest-hit (intersection +inf.0
                                        (flvector 0.0 0.0 0.0)
                                        (flvector 0.0 0.0 0.0)
                                        null))
             (shapes shape-list))
    (if (null? shapes)
        nearest-hit
        (let ((current-attempt ((shape-fn (unsafe-car shapes)) 
                                (unsafe-car shapes) ray)))
          (if (unsafe-fl< (intersection-t current-attempt)
                          (intersection-t nearest-hit))
              (loop current-attempt (unsafe-cdr shapes))
              (loop nearest-hit (unsafe-cdr shapes)))))))

;--------------------------
; Shape classes
;--------------------------

(struct shape (point material derived fn))
(struct intersection (t point normal shape))
(struct sphere (radius))
(struct cuboid (length width height))

; Sphere Shape
(define (make-sphere-shape)
  (shape (rand-point WORLD-SIZE)
         (make-phong-material)
         (sphere (+ 1.0 (random)))
         hit-sphere))

(define (hit-sphere shape ray)
  (let* ((sphere (shape-derived shape))
         (ray-dir (ray-direction ray))
         (ray-pt (ray-point ray))
         (diff (sub-v ray-pt (shape-point shape)))
         (radius (sphere-radius sphere))
         (radius-recip (unsafe-fl/ 1.0 radius))
         (a (dot-v ray-dir ray-dir))
         (b (unsafe-fl* 2.0 (dot-v ray-dir diff)))
         (c (unsafe-fl- (dot-v diff diff) (unsafe-fl* radius radius)))
         (disc (unsafe-flsqrt (unsafe-fl- (unsafe-fl* b b) 
                                          (unsafe-fl* 4.0 (unsafe-fl* a c)))))
         (t1 (unsafe-fl* (unsafe-fl+ (- b) disc) (unsafe-fl* 0.5 a)))
         (t2 (unsafe-fl* (unsafe-fl- (- b) disc) (unsafe-fl* 0.5 a)))
         (new-t (if (unsafe-fl> t2 0.0) t2 (if (unsafe-fl> t1 0.0) t1 +inf.0)))
         (new-pt (point-at ray new-t)))
    (intersection new-t
                  new-pt
                  (scale-by-m (sub-v (shape-point shape) new-pt)
                              radius-recip)
                  shape)))

; Cuboid Shape
(define (make-cuboid-shape)
  (shape (rand-point WORLD-SIZE)
         (make-flat-material)
         (cuboid (+ 0.5 (random))
                 (+ 0.5 (random))
                 (+ 0.5 (random)))
         hit-cuboid))

(define (make-plane-shape point)
  (shape point
         (make-flat-material)
         (cuboid MAX-FLOAT32
                 0.0
                 MAX-FLOAT32)
         hit-cuboid))

; To do. I haven't figured out the right functional
; way to transfer this over from C yet.
(define (hit-cuboid shape ray)
  (let* ((cuboid (shape-derived shape))
         (tx1 (unsafe-fl/ (unsafe-fl- 
                           (unsafe-fl+ 
                            (- (cuboid-width cuboid)) 
                            (unsafe-flvector-ref (shape-point shape) 0)) 
                           (unsafe-flvector-ref (ray-point ray) 0)) 
                          (unsafe-flvector-ref (ray-direction ray) 0)))
         (tx2 (unsafe-fl+ tx1 (unsafe-fl/ 
                               (unsafe-fl* 2.0 (cuboid-width cuboid))
                               (unsafe-flvector-ref (ray-direction ray) 0)))))
    ; not done!
    (intersection +inf.0 null null null)))
         
;--------------------------
; Vector functions
; -> Create vectors with (flvector a. b. c.).
; -> Access vectors with (unsafe-flvector-ref [vector] [index]).
; -> Casting with exact->inexact is slow!
;--------------------------

(define (rand-color)
  (flvector (random) (random) (random)))

(define (rand-point radius)
  (flvector (unsafe-fl* radius (unsafe-fl- (unsafe-fl* 2.0 (random)) 1.0))
            (unsafe-fl* radius (unsafe-fl- (unsafe-fl* 2.0 (random)) 1.0))
            (unsafe-fl* radius (unsafe-fl- (unsafe-fl* 2.0 (random)) 1.0))))

(define (dot-v v1 v2)
  (+ (unsafe-fl* (unsafe-flvector-ref v1 0) 
                 (unsafe-flvector-ref v2 0))
     (unsafe-fl* (unsafe-flvector-ref v1 1) 
                 (unsafe-flvector-ref v2 1))
     (unsafe-fl* (unsafe-flvector-ref v1 2) 
                 (unsafe-flvector-ref v2 2))))

(define (dot-m v1 m)
  (+ (unsafe-fl* (unsafe-flvector-ref v1 0) m)
     (unsafe-fl* (unsafe-flvector-ref v1 1) m)
     (unsafe-fl* (unsafe-flvector-ref v1 2) m)))

(define (cross v1 v2)
  (let ((x1 (unsafe-flvector-ref v1 0))
        (y1 (unsafe-flvector-ref v1 1))
        (z1 (unsafe-flvector-ref v1 2))
        (x2 (unsafe-flvector-ref v2 0))
        (y2 (unsafe-flvector-ref v2 1))
        (z2 (unsafe-flvector-ref v2 2)))
    (flvector (unsafe-fl- (unsafe-fl* y1 z2) (unsafe-fl* z1 y2))
              (unsafe-fl- (unsafe-fl* x1 z2) (unsafe-fl* z1 x2))
              (unsafe-fl- (unsafe-fl* x1 y2) (unsafe-fl* y1 x2)))))

(define (length-v v)
  (let ((x (unsafe-flvector-ref v 0))
        (y (unsafe-flvector-ref v 1))
        (z (unsafe-flvector-ref v 2)))
    (unsafe-fl/ 1.0 (unsafe-flsqrt 
                     (+ (unsafe-fl* x x) 
                        (unsafe-fl* y y) 
                        (unsafe-fl* z z))))))

(define (normalize v)
  (let* ((x (unsafe-flvector-ref v 0))
         (y (unsafe-flvector-ref v 1))
         (z (unsafe-flvector-ref v 2))
         (len (unsafe-fl/ 1.0 (unsafe-flsqrt 
                               (+ (unsafe-fl* x x) 
                                  (unsafe-fl* y y) 
                                  (unsafe-fl* z z))))))
    (flvector (unsafe-fl* x len) 
              (unsafe-fl* y len) 
              (unsafe-fl* z len))))

(define (reflect v n)
  (let* ((vn-dot2 (unsafe-fl* 2.0 (dot-v v n)))
         (dot-sign (if (unsafe-fl>= vn-dot2 0.0) 1.0 -1.0)))
    (flvector (unsafe-fl- (unsafe-fl* (unsafe-fl* dot-sign (unsafe-flvector-ref n 0)) vn-dot2) 
                          (unsafe-flvector-ref v 0))
              (unsafe-fl- (unsafe-fl* (unsafe-fl* dot-sign (unsafe-flvector-ref n 1)) vn-dot2) 
                          (unsafe-flvector-ref v 1))
              (unsafe-fl- (unsafe-fl* (unsafe-fl* dot-sign (unsafe-flvector-ref n 2)) vn-dot2) 
                          (unsafe-flvector-ref v 2)))))

(define (add-v v1 v2)
  (flvector (unsafe-fl+ (unsafe-flvector-ref v1 0)
                        (unsafe-flvector-ref v2 0))
            (unsafe-fl+ (unsafe-flvector-ref v1 1)
                        (unsafe-flvector-ref v2 1))
            (unsafe-fl+ (unsafe-flvector-ref v1 2)
                        (unsafe-flvector-ref v2 2))))

(define (sub-v v1 v2)
  (flvector (unsafe-fl- (unsafe-flvector-ref v1 0)
                        (unsafe-flvector-ref v2 0))
            (unsafe-fl- (unsafe-flvector-ref v1 1)
                        (unsafe-flvector-ref v2 1))
            (unsafe-fl- (unsafe-flvector-ref v1 2)
                        (unsafe-flvector-ref v2 2))))

(define (negate v)
  (flvector (- (unsafe-flvector-ref v 0))
            (- (unsafe-flvector-ref v 1))
            (- (unsafe-flvector-ref v 2))))

(define (clamp v)
  (let ((x (unsafe-flvector-ref v 0))
        (y (unsafe-flvector-ref v 1))
        (z (unsafe-flvector-ref v 2)))    
    (flvector (if (unsafe-fl> x 1.0) 1.0 (if (unsafe-fl< x 0.0) 0.0 x))
              (if (unsafe-fl> y 1.0) 1.0 (if (unsafe-fl< y 0.0) 0.0 y))
              (if (unsafe-fl> z 1.0) 1.0 (if (unsafe-fl< z 0.0) 0.0 z)))))

(define (scale-by-v v1 v2)
  (flvector (unsafe-fl* (unsafe-flvector-ref v1 0)
                        (unsafe-flvector-ref v2 0))
            (unsafe-fl* (unsafe-flvector-ref v1 1)
                        (unsafe-flvector-ref v2 1))
            (unsafe-fl* (unsafe-flvector-ref v1 2)
                        (unsafe-flvector-ref v2 2))))

(define (scale-by-m v m)
  (flvector (unsafe-fl* (unsafe-flvector-ref v 0) m)
            (unsafe-fl* (unsafe-flvector-ref v 1) m)
            (unsafe-fl* (unsafe-flvector-ref v 2) m)))

(define (to-argb v)
  (bytes 255
         (exact-round (unsafe-fl* 255.0 (unsafe-flvector-ref v 0)))
         (exact-round (unsafe-fl* 255.0 (unsafe-flvector-ref v 1)))
         (exact-round (unsafe-fl* 255.0 (unsafe-flvector-ref v 2)))))

(define (print-vec name v)
  (printf "~s:\n  ~s\n  ~s\n  ~s\n" 
          name 
          (unsafe-flvector-ref v 0)
          (unsafe-flvector-ref v 1)
          (unsafe-flvector-ref v 2)))

;--------------------------
; World class
;--------------------------

(struct world (shapes lights camera back-color))

(define default-camera
  (look-at (flvector 0.0 0.0 (+ WORLD-SIZE 3.0))
           (flvector 0.0 -1.0 0.0)
           (flvector 0.0 1.0 0.0)
           65.0 SCREEN-ASPECT))

(define default-ground
  (let ((dist 1000000.0))
    (list
     (shape (flvector 0.0 (- (- WORLD-SIZE) dist) 0.0)
            (make-phong-material)
            (sphere dist)
            hit-sphere))))

(define (blend-back-color light-list)
  (let loop ((v (flvector 0.0 0.0 0.0))
             (lights light-list))
    (if (null? lights)
        (scale-by-m v (/ 1.0 (length light-list)))
        (loop (add-v v (light-color (car lights)))
              (cdr lights)))))

(define (make-world)
  (let ((shapes 
         (let ((spheres (for/list ((x SPHERE-COUNT)) (make-sphere-shape)))
               (cuboids (for/list ((x CUBOID-COUNT)) (make-cuboid-shape))))
           (append default-ground spheres cuboids)))
        (lights
         (let ((point-lights (for/list ((x POINT-LIGHT-COUNT)) (make-point-light)))
               (distant-lights (for/list ((x DISTANT-LIGHT-COUNT)) (make-distant-light)))
               (area-lights (for/list ((x AREA-LIGHT-COUNT)) (make-area-light))))
           (append point-lights distant-lights area-lights))))
    (world shapes
           lights
           default-camera
           (blend-back-color lights))))

(define (color-at world xx yy)
  (let* ((scene-ray (image-ray (world-camera world) xx yy))
         (nearest-hit (closest-hit scene-ray (world-shapes world)))
         (color
          (if (unsafe-fl>= (intersection-t nearest-hit) MAX-FLOAT32)
              (world-back-color world)
              (let ((mat (shape-material (intersection-shape nearest-hit))))
                ((material-fn mat) 
                 mat 
                 (intersection-point nearest-hit)
                 (intersection-normal nearest-hit)
                 scene-ray world)))))
    (clamp color)))

;--------------------------
; Main
;--------------------------

; Bitmaps can be scaled? Look at backing scale and "unscaled".

(require racket/draw) ; For bitmaps

(define (main)
  (define pic (make-bitmap SCREEN-WIDTH SCREEN-HEIGHT))
  (define world (make-world))
  (define width-recip (/ 1.0 SCREEN-WIDTH))
  (define height-recip (/ 1.0 SCREEN-HEIGHT))
  (for ((x SCREEN-WIDTH))
    (displayln (list x '/ SCREEN-WIDTH))
    (for ((y SCREEN-HEIGHT))
      (send pic set-argb-pixels
            x y 1 1
            (to-argb (color-at world
                               (unsafe-fl* (unsafe-fx->fl x) width-recip)
                               (unsafe-fl* (unsafe-fx->fl y) height-recip))))))
  (send pic save-file "image.png" 'png)
  pic)      

(displayln "Type (main) or (time (main)) to begin.")
