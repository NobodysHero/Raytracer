;;; raytracer-gpu.lisp

(in-package :raytracer-gpu)

(when (cepl.lifecycle:uninitialized-p) 
  (cepl:repl))

(defparameter *gpu-memory* 100)		;in vec4

(defparameter *data* nil)
(defparameter *data-ubo* nil)

(defparameter *vertices* (make-gpu-array (list (v! -1.5 -1.5)
                                               (v! 0.5 -0.5)
                                               (v! 0.5 0.5)
                                               (v! -0.5 -0.5))
                                         :element-type :vec2))
(defparameter *vert-stream* (make-buffer-stream *vertices*))

(defparameter *view* nil)
(defparameter *camera* nil)

(defparameter *spheres* nil)
(defparameter *spheres-ubo* nil)

(defvar *structs* nil)
(defvar *materials* nil)

;;; Camera container

(defstruct-g camera
  (origin :vec3 :accessor origin)
  (lower-left :vec3 :accessor ll)
  (right :vec3 :accessor right)
  (up :vec3 :accessor up))

(defun setup-camera (vfov aspect &key (lookfrom (v! 0 0 0)) (lookat (v! 0 0 -1)) (up (v! 0 1 0))) ;vfov is angle top to bottom in deg
  (let* ((theta (coerce (* vfov (/ pi 180)) 'single-float))
	 (half-height (tan (/ theta 2)))
	 (half-width (* aspect half-height))
	 (w (rtg-math.vectors:normalize (rtg-math.vectors:- lookfrom lookat)))
	 (u (rtg-math.vectors:normalize (rtg-math.vectors:cross up w)))
	 (v (rtg-math.vectors:cross w u)))
    (setf *camera* (make-camera :origin lookfrom
                                :lower-left (rtg-math.vectors:- lookfrom
                                                                (rtg-math.vectors:* u half-width)
                                                                (rtg-math.vectors:* v half-height)
                                                                w)
                                :right (rtg-math.vectors:* u (* 2 half-width))
                                :up (rtg-math.vectors:* v (* 2 half-height))))))
;;; Sphere gpu-structure

(defstruct-g (sphere-data :layout std-140)
  (pos (:vec4 4))
  (color (:vec4 4)))

(defstruct-g (render :layout std-140)
  (data (:vec4 100)))

(defmacro-g radius (sphere)
  `(w ,sphere))

(defmacro-g pos (sphere)
  `(swizzle ,sphere :xyz))

(defmacro-g type (material)
  `(x ,material))

(defmacro-g color (material)
  `(swizzle ,material :yzw))

;(defclass gpu-data ()  (start-index )  (length :initform 0 :init))

;(defclass material (gpu-data)  (material-id :accessor index :initform ()))


(defun-g normal ((sp :vec4) (p :vec3))
  (/ (- p (pos sp)) (radius sp)))

(defun-g color-normal ((n :vec3))
  (/ (+ n 1) 2))

(defun-g hits ((from :vec3) (direction :vec3) (sp :vec4))
  (let* ((om (- (pos sp) from))
         (q (- (expt (radius sp) 2)
               (dot om om)))
         (p (dot direction om))
         (disc (+ q (* p p))))
    (if (< disc 0)
        -1.0
        (let* ((sqrt-disc (sqrt disc))
               (near (- p sqrt-disc))
               (far (+ p sqrt-disc)))
          (cond
            ((< 0.001 near) near)
            ((< 0.001 far) far)
            (t -1.0))))))

(defun-g background ((direction :vec3))
  (let ((s (* 0.5 (+ 1 (y direction)))))
    (+ (* (- 1 s) (v! 1 1 1)) (* s (v! 0.5 0.7 1))))
  (v! 0.1 0.15 0.1)
  ;(v! 0 0 0)
  )

;;; methods for random numbers

(defun-g rand2 ((seed :int))
  (let ((next (mod (* seed 16807) 2147483647)))
    (values (/ (float (- next 1)) 2147483646) (int next))))

(defun-g rand-vec2 ((seed :int))
  (multiple-value-bind (v1 seed1) (rand2 seed)
    (multiple-value-bind (v2 seed2) (rand2 seed1)
      (values (v! v1 v2) seed2))))

(defun-g rand-vec3 ((seed :int))
  (multiple-value-bind (v1 seed1) (rand2 seed)
    (multiple-value-bind (v2 seed2) (rand2 seed1)
      (multiple-value-bind (v3 seed3) (rand2 seed2)
        (values (v! v1 v2 v3) seed3)))))

;; (defun-g rand ((co :vec2))
;;   (fract (* (sin (dot co (v! 12.9898 78.233))) 43758.5453)))

;; (defun-g rand ((co :float))
;;   (fract (* (sin (* co 54.321)) 43758.5453)))

(defmacro-g next-random (seed-var)
  (with-gensyms (val nseed)
    `(multiple-value-bind (,val ,nseed) (rand2 ,seed-var)
       (setf ,seed-var ,nseed)
       ,val)))

;;; main raytracer shader

(defun-g reflect-probability ((cosine :float) (ref-index :float))
  (let* ((r0 (expt (/ (- 1.0 ref-index) (+ 1.0 ref-index)) 2)))
    (+ r0 (* (- 1 r0)
             (expt (- 1.0 cosine) 5)))))

(defun-g lambertian ((direction :vec3) (normal :vec3) (seed :int))
  (multiple-value-bind (rnd-vec3 nseed) (rand-vec3 seed)
    (values (normalize (+ normal (- (* 2 rnd-vec3) 1))) nseed)))

(defun-g refract ((material :vec4) (normal :vec3) (direction :vec3) (seed :int))
  (cond
    ((= 0.0 (type material));lambertian
     (multiple-value-bind (new-direction nseed) (lambertian direction normal seed)
       (values t (color material) new-direction nseed)))
    ((= 1.0 (type material));metal
     (let ((proj (dot normal direction)))
       (if (> proj 0)
           (values nil (v! 0 0 0) (v! 0 0 0) seed)
           (values t (color material) (- direction (* 2 proj normal)) seed))))
    ((= 2.0 (type material));glass
     (let* ((dt (dot normal direction))
            (sign (if (< dt 0) -1.0 1.0))
            (index-from-to (if (< sign 0) (/ 1.0 (y material)) (y material)))
            (disc (- 1 (* (* index-from-to index-from-to) (- 1 (* dt dt))))))
       (cond
         ((or (< disc 0.0) (< (rand2 seed)
                              (reflect-probability (* sign dt) (y material))))
          (values t (v! 1 1 1) (- direction (* 2.0 dt normal))
                  seed))
         ((>= disc 0.0)	;check for refraction
          (values t (v! 1 1 1) (normalize (+ (* index-from-to direction)
                                             (* normal (- (* sign (sqrt disc))
                                                          (* index-from-to dt)))))
                  seed))
         (t (values nil (v! 0 0 0) (v! 0 0 0) seed)))))
    ((= 3.0 (type material));light source
     (values nil (color material) (v! 0 0 0) seed))
    (t (values nil (v! 0 0 0) (v! 0 0 0) seed))))

(defun-g render-frag ((uv :vec2) &uniform (spheres sphere-data :ubo) (cam camera) (time :float) (samples :int))
  (let ((color (v! 0 0 0))
        (seed (int (+ (* 1000 (x uv)) (* 1000000 (y uv)))))
        (rnd-v2 (v! 0 0))
        (last-rndx (+ (x uv) time))
        (last-rndy (+ (y uv) (fract (* 1000.0 time time))))
        (variance (v! 0.0025 0.0025)))
    (for (j 0) (< j samples) (++ j)
         (multiple-value-bind (vec nseed) (rand-vec2 seed)
           (setf rnd-v2 vec
                 seed nseed))
         (let* ((new-uv (+ uv (* variance rnd-v2)))
                (direction (normalize (+ (ll cam)
                                         (* (x new-uv)  (right cam))
                                         (* (y new-uv) (up cam))
                                         (- (origin cam)))))
                (orig (v! 0 0 0))
                (albedo (v! 1 1 1)))
           (next-random seed)
           
           (setf orig (origin cam))
           (for (depth 0) (< depth 50) (++ depth)
                (let* ((nearest-dist MOST-POSITIVE-SINGLE-FLOAT)
                       (nearest-sphere -1))
                  (for (i 0) (< i 4) (++ i)
                       (let ((dist (hits orig direction (aref (sphere-data-pos spheres) i))))
                         (if (and (< 0.001 dist) (< dist nearest-dist))
                             (setf nearest-dist dist
                                   nearest-sphere i))))
                  (if (<= 0 nearest-sphere)
                      (let ((hitpoint (+ orig (* nearest-dist direction))))
                        (multiple-value-bind (cont new-color dir nseed)
                            (refract (aref (sphere-data-color spheres) nearest-sphere)
                                     (normal (aref (sphere-data-pos spheres) nearest-sphere)                                                         hitpoint)
                                     direction seed)
                          
                          (setf seed nseed)
                          (unless cont
                            (setf color (+ color (* albedo new-color)))
                            (break))
                          (setf albedo (* albedo new-color))
                          (setf orig hitpoint
                                direction dir)))
                      (progn (setf color (+ color (* albedo (background direction))))
                             (break)))))))
    (v! (/ color samples) 0.0)))


(defpipeline-g simple-raytracer ()
  :fragment (render-frag :vec2))

;;; main program utilities

(defun init ()
  (setf *spheres* (make-gpu-array 
                   (list (list (list (v! 0.0 0 -1  0.5)
                                     (v! 0.0 -100.5 -1 100.0)
                                     (v! 1.0 0 -1 0.5)
                                     (v! -1.0 0 -1  0.5))
                               (list (v! 2 1.5 0.0 0.0)
                                     (v! 0 0.8 0.8 0.0)
                                     (v! 1 0.8 0.6 0.2)
                                     (v! 3 2 2 0))))
                   :element-type 'sphere-data :dimensions 1 :access-style :dynamic-draw))

  (setf *spheres-ubo* (make-ubo-from-array *spheres* 0 'sphere-data))

  (setup-camera 30 2 :lookfrom (v! 0.0 2.0 2))
  (setf *view* (make-viewport '(800 400)))
  (setf (cepl.context:surface-dimensions (first (cepl.context:surfaces))) '(800 400)))

(let ((lastframe (get-internal-real-time))
      (start (get-internal-real-time))
      (running nil))
  (defun update ()
    (let* ((now (get-internal-real-time))
           (delta (- now lastframe)))
      (declare (ignorable delta))
      (setf lastframe now)
      (let ((omega (* 0.001 (- now start))))
        (with-gpu-array-as-c-array (arr *spheres* :access-type :write-only)
          (setf (aref-c (sphere-data-pos (aref-c arr 0)) 2)
                (v! (* 1 (cos omega)) 0 (- (sin omega) 1) 0.5))
          (setf (aref-c (sphere-data-pos (aref-c arr 0)) 3)
                (v! (* -1 (cos omega)) 0 (- 0 (sin omega) 1) 0.5))))))
  (defun reset ()
    (setf start (get-internal-real-time)))

  (defun start ()
    (setf running t
          start (get-internal-real-time))
    (loop while running do
          (continuable 
            (frame)
            (update-repl-link))))
  (defun stop ()
    (setf running nil)))

(defun frame (&optional (samples 100))
  (with-viewport *view*
    (clear)
    (update)
    (map-g #'simple-raytracer *vert-stream* :spheres *spheres-ubo*
                                            :cam *camera*
                                            :time (/ (get-internal-real-time) 1000000.0)
                                            :samples samples)
                                        ;(map-g #'simple *vert-stream*)
    (swap)
    (step-host)))


(defun save-image (path array)
  (let ((width (array-dimension array 0))
        (height (array-dimension array 1)))
    (with-open-file (out path :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (format out "P3~%~a ~a~%255~%" width height)
      (loop for j from (1- height) downto 0 do
            (loop for i below width do
                  (let ((vec (aref array i j)))
                    (format out "~a ~a ~a~%"
                            (floor (aref vec 0))
                            (floor (aref vec 1))
                            (floor (aref vec 2))))))
      (format t "Successfully written to: ~a.~%" path))))

(defun save-image-from-gpu-array (path garray)
  (destructuring-bind (width height) (gpu-array-dimensions garray)
    (with-open-file (out path :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (format out "P3~%~a ~a~%255~%" width height)
      (let ((carr (pull1-g garray)))
        (loop for j from (1- height) downto 0 do
              (loop for i below width do
                    (let ((vec (aref-c carr i j)))
                      (format out "~a ~a ~a~%"
                              (floor (aref vec 0))
                              (floor (aref vec 1))
                              (floor (aref vec 2)))))))
      (format t "Successfully written to: ~a.~%" path))))


(defparameter *default-path* "C:/Users/bradr/Desktop/raytracer/")
(defun render-and-save (filename &key (samples 1000) (path *default-path*) (resolution '(1600 800)))
  (let ((file (format nil "~a~a.ppm" path filename))
        (view (make-viewport resolution)))
    (with-viewport view
      (let ((framebuffer (make-fbo 0)))
        (with-fbo-bound (framebuffer)
          (clear)
          (map-g #'simple-raytracer *vert-stream* :spheres *spheres-ubo*
                                                  :cam *camera*
                                                  :time (/ (get-internal-real-time) 1000000.0)
                                                  :samples samples)
          (save-image file (make-array resolution :initial-contents (pull-g (attachment framebuffer 0)))))
        (free framebuffer)))))

(defun make-movie (path num-frames &key (resolution '(1600 800)) (samples 1000))
  (let ((view (make-viewport resolution)))
    (with-viewport view
      (let ((framebuffer (make-fbo 0)))
        (with-fbo-bound (framebuffer)
          (loop for frame below num-frames do
                (update-frame frame)
                (clear)
                (map-g #'simple-raytracer *vert-stream* :spheres *spheres-ubo*
                                                        :cam *camera*
                                                        :time (/ (get-internal-real-time) 1000000.0)
                                                        :samples samples)
                (save-image-from-gpu-array (format nil "~aframe~d.ppm" path frame)
                                           (attachment framebuffer 0))))
        (free framebuffer)))))


(defun update-frame (frame)
  (let ((omega (* (/ (* 2 PI) 120) frame)))
    (with-gpu-array-as-c-array (arr *spheres* :access-type :write-only)
      (setf (aref-c (sphere-data-pos (aref-c arr 0)) 2)
            (v! (* 1 (cos omega)) 0 (- (sin omega) 1) 0.5))
      (setf (aref-c (sphere-data-pos (aref-c arr 0)) 3)
            (v! (* -1 (cos omega)) 0 (- 0 (sin omega) 1) 0.5)))))
