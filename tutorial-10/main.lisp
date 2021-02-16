;;;; Tutorial 10: Color Keying and Blending

(defpackage #:tutorial-10
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-10)

(defparameter *fps* 60)
(defparameter *Width* 640)
(defparameter *height* 480)

(defparameter *pixels* nil)
(defparameter *texture-id* 0)
(defparameter *texture-width* 0)
(defparameter *texture-height* 0)


(defun free-texture ()
  (when *pixels*
    (cffi:foreign-free *pixels*)
    (setf *pixels* nil))
  
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

;; Does not work on RGB images currently
(defun load-texture-from-file-with-color-key (path red green blue &optional (alpha 0))
  (let* ((image (pngload:load-file path :flatten t))
	 (pixels (pngload:data image)))

    (loop  :for i :below (length pixels) :by 4 :do
      (when (and (= (elt pixels i) red)
		   (= (elt pixels (+ i 1)) green)
		   (= (elt pixels (+ i 2)) blue)
		   (or (= alpha 0)
		       (= (elt pixels (+ i 3)) alpha)))

	;; Since we're working with a flat list of all the colors, and not pixels as arrays
	;; we only need to change the alpha channel
	  (setf (elt pixels (+ i 3)) #x00)))
    (load-texture-from-pixels pixels (pngload:width image) (pngload:height image) :RGBA)))

(defun texture-render (x y) 
  (unless (= *texture-id* 0)
    (gl:load-identity)
    
    (let ((tex-top 0.)
	  (tex-bottom 1.)
	  (tex-left 0.)
	  (tex-right 1.)

	  (quad-width *texture-width*)
	  (quad-height *texture-height*))
      
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
  
  (gl:enable :texture-2d)

  (gl:enable :BLEND)
  (gl:disable :depth-test)
  (gl:blend-func :src-alpha :one-minus-src-alpha)

  (let ((gl-error (gl:get-error)))
    (unless (string= gl-error 'zero)
      (error gl-error))))

(defun update ())

(defun render ( )
  (gl:clear :color-buffer-bit)
  (gl:color 1. 1. 1. 0.5)
  
  (let ((x (round (- *width* *texture-width*) 2))
	(y (round (- *height* *texture-height*) 2)))
    (texture-render x y))
 
  (sdl:update-display))

(defun handle-keys (key &optional x y))


(defun load-media (path)
  (load-texture-from-file-with-color-key path 0 255 255 0))

(defun main (&aux (title "Tutorial 10: Color Keying and Blending")
	       (path (concatenate 'string (namestring (asdf:system-relative-pathname :opengl-tutorials "tutorial-10/assets/")) "circle.png")))
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



