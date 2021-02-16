;;;; Tutorial 13: Matrix Transformations

(defpackage #:tutorial-13
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-13)

(defparameter *fps* 60)
(defparameter *Width* 640)
(defparameter *height* 480)

(defparameter *pixels* nil)
(defparameter *texture-id* 0)
(defparameter *texture-width* 0)
(defparameter *texture-height* 0)

(defparameter *angle* 0)
(defparameter *transformation-combo* 0)

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

;; Using pngload instead of devil for image.
(defun load-texture-from-file (path)
  (let ((image (pngload:load-file path :flatten t))) ;; Flatten because CL-Opengl expect a flat list of colors\pixels.
    (load-texture-from-pixels (pngload:data image) (pngload:width image) (pngload:height image)
			      (if (string-equal (pngload:color-type image) :truecolour-alpha) :rgba :rgb))))

(defun texture-render (x y) 
  (unless (= *texture-id* 0)
    
    (let ((tex-top 0.)
	  (tex-bottom 1)
	  (tex-left 0.)
	  (tex-right 1)

	  (quad-width *texture-width*)
	  (quad-height *texture-height*))
      

      (gl:translate  x  y 0)
      
      
      (gl:bind-texture :texture-2d *texture-id*)

      (gl:with-primitive :quads
	(gl:tex-coord tex-left tex-top)     (gl:vertex 0          0)
	(gl:tex-coord tex-right tex-top)    (gl:vertex quad-width 0)
	(gl:tex-coord tex-right tex-bottom) (gl:vertex quad-width quad-height)
	(gl:tex-coord tex-left tex-bottom)  (gl:vertex 0          quad-height)))))

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

(defun update ()
  (incf *angle* (round 360 *fps*))

  (when (> *angle* 360)
    (setf *angle* 0)))

(defun render ( )
  (gl:clear :color-buffer-bit)
  (gl:load-identity)


  (case *transformation-combo*
    (0
     (gl:translate (round *width* 2) (round *Height* 2) 0)
     (gl:rotate *angle* 0 0 1)
     (gl:scale 2 2 0)
     (gl:translate (round *texture-width* -2) (round *texture-height* -2) 0))
    
    (1
     (gl:translate (round *width* 2) (round *height* 2) 0)
     (gl:rotate *angle* 0 0 1)
     (gl:translate (round *width* -2) (round *height* -2) 0)
     (gl:scale 2 2 0))
    
    (2
     (gl:scale 2 2 0)
     (gl:translate (round *width* 2) (round *height* 2) 0)
     (gl:rotate *angle* 0 0 1)
     (gl:translate (round *width* -2) (round *height* -2) 0))
    
    (3
     (gl:translate (round *width* 2) (round *height* 2) 0)
     (gl:rotate *angle* 0 0 1)
     (gl:scale 2 2 0))
    
    (4
     (gl:rotate *angle* 0 0 1)
     (gl:translate (round *width* 2) (round *height* 2) 0)
     (gl:scale 2 2 0))
     (gl:translate (round *width* -2) (round *height* -2) 0))
  
  (texture-render 0 0)

  
  (sdl:update-display))

(defun handle-keys (key &optional x y)
  (when (string-equal key :sdl-key-q)
    (setf *angle* 0
	  *transformation-combo* (if (= *transformation-combo* 4) 0 (1+ *transformation-combo*)))))



(defun load-media (path)
  (load-texture-from-file path))

(defun main (&aux (title "Tutorial 13: Matrix Transformations")
	       (path (concatenate 'string (namestring (asdf:system-relative-pathname :opengl-tutorials "tutorial-13/assets/")) "texture.png")))
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



