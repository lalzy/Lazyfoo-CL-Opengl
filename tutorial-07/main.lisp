;;;; Tutorial 07: Clipping Textures

(defpackage #:tutorial-07
  (:nicknames #:lesson-07 #:lazy-foo-07)
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-07)

(defparameter *fps* 60)
(defparameter *Width* 640)
(defparameter *height* 480)

(defparameter *texture-id* 0)
(defparameter *texture-width* 0)
(defparameter *texture-height* 0)

(defparameter *arrow-clips* (make-array 4))

(defun free-texture ()
  (unless (= *texture-id* 0)
    (gl:delete-texture *texture-id*)
    (setf *texture-id* 0
	  *texture-width* 0
	  *texture-height* 0)))

(defun load-texture-from-pixels (pixels width height &optional (target :RGB))
  (free-texture)

  (setf *texture-id* (gl:gen-texture)
	*texture-width* width
	*texture-height* height)

  (gl:bind-texture :texture-2d *texture-id*)
  (gl:tex-image-2d :texture-2d 0 target width height 0 target :unsigned-byte pixels)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl:tex-parameter :texture-2d :texture-min-filter :linear)

  (gl:bind-texture :texture-2d 0))

;; Using pngload instead of devil for image.
(defun load-texture-from-file (path)
  (let ((image (pngload:load-file path :flatten t))) ;; Flatten because CL-Opengl expect a flat list of colors\pixels.
    (load-texture-from-pixels (pngload:data image) (pngload:width image) (pngload:height image)
			      (if (string-equal (pngload:color-type image) :truecolour-alpha) :rgba :rgb))))

(defun texture-render (x y &optional rect) 
  (unless (= *texture-id* 0)
    (gl:load-identity)
    
    (let ((tex-top 0.)
	  (tex-bottom 1.)
	  (tex-left 0.)
	  (tex-right 1.)

	  (quad-width *texture-width*)
	  (quad-height *texture-height*))
      
      (when rect
	(setf tex-left (/ (rect-x rect) *texture-width*)
	      tex-right (/ (+ (rect-x rect) (rect-w rect)) *texture-width*)
	      tex-top (/ (rect-y rect) *texture-height*)
	      tex-bottom (/ (+ (rect-y rect) (rect-h rect)) *texture-height*)
	      quad-width (rect-w rect)
	      quad-height (rect-h rect)))

      
      (gl:translate x y 0)

      (gl:bind-texture :texture-2d *texture-id*)

      (gl:with-primitive :quads
	(gl:tex-coord tex-left tex-top) (gl:vertex 0 0)
	(gl:tex-coord tex-right tex-top) (gl:vertex quad-width 0)
	(gl:tex-coord tex-right tex-bottom) (gl:vertex quad-width quad-height)
	(gl:tex-coord tex-left tex-bottom) (gl:vertex 0 quad-height)))))

(defun init-gl ()
  (gl:viewport 0. 0. *width* *height*)

  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 *width* *height* 0 1 -1)

  
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (gl:clear-color 0 0 0 1)
  
  (gl:enable :texture-2d))

(defun update ())

(defun render ( )
  (gl:clear :color-buffer-bit)

  (texture-render 0. 0. (aref *arrow-clips* 0))
  (texture-render (- *width* (rect-w (aref *arrow-clips* 1)))
		  0. (aref *arrow-clips* 1))
  (texture-render 0. (- *height* (rect-h (aref *arrow-clips* 2))) (aref *arrow-clips* 2))
  (texture-render (- *width* (rect-w (aref *arrow-clips* 3))) (- *height* (rect-h (aref *arrow-clips* 3))) (aref *arrow-clips* 3))
  
  (sdl:update-display))


(defun load-media (path)
  (setf (aref *arrow-clips* 0) (make-rect :x 0.   :y 0.   :w 128. :h 128.)
	(aref *arrow-clips* 1) (make-rect :x 128. :Y  0.  :w 128. :h 128.)
	(aref *arrow-clips* 2) (make-rect :x 0.   :y 128. :w 128. :h 128.)
	(aref *arrow-clips* 3) (make-rect :x 0.   :y 128. :w 128. :h 128.)
	)
  
  (load-texture-from-file path))


(defun handle-keys (key &optional x y))


(defun main (&aux (title "Tutorial 07: Clipping Textures")
	       (path (concatenate 'string (namestring (asdf:system-relative-pathname :opengl-tutorials "tutorial-07/assets/")) "arrows.png")))
  (bt:make-thread
   (lambda ()
     (sdl:with-init ()
       (sdl:window *width* *height* :title-caption title :flags '(sdl:sdl-opengl))
       (setf (sdl:frame-rate) *fps*)

       (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)

       (sdl:enable-key-repeat 100 1)

       (init-gl)

       (load-media path)
       
	 (sdl:with-events ()
	   (:quit-event () t)

	   (:key-down-event (:key key)
			    (handle-keys key ))
	   
	   (:idle ()
		  (update)
		  (render)))
       (free-texture)))
     :name title))



