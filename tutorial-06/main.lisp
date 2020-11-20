;;;; Tutorial 06 - Loading a texture

(defpackage #:tutorial-06
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-06)

(defparameter *fps* 60)
(defparameter *Width* 640)
(defparameter *height* 480)

(defparameter *texture-id* 0)
(defparameter *texture-width* 0)
(defparameter *texture-height* 0)

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

(defun texture-render (x y) 
  (unless (= *texture-id* 0)
    (gl:load-identity)
    
    (gl:translate x y 0)

    (gl:bind-texture :texture-2d *texture-id*)

    (gl:with-primitive :quads
      (gl:tex-coord 0. 0.) (gl:vertex 0 0)
      (gl:tex-coord 1. 0.) (gl:vertex *texture-width* 0)
      (gl:tex-coord 1. 1.) (gl:vertex *texture-width* *texture-height*)
      (gl:tex-coord 0. 1.) (gl:vertex 0 *texture-height*))))

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

  (let ((x (round (- *width* *texture-width*) 2))
	(y (round (- *height* *texture-height*) 2)))
    (texture-render x y))
  
  (sdl:update-display))


(defun handle-keys (key &optional x y))


(defun main (&aux  (title "Tutorial 06 - Loading a texture")
	       (path (concatenate 'string (namestring (asdf:system-relative-pathname :opengl-tutorials "tutorial-06/assets/")) "texture.png")))
  (bt:make-thread
   (lambda ()
     (sdl:with-init ()
       (sdl:window *width* *height* :title-caption title :flags '(sdl:sdl-opengl))
       (setf (sdl:frame-rate) *fps*)

       (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)

       (sdl:enable-key-repeat 100 1)

       (init-gl)

       (load-texture-from-file path) ; We don't need load-media anymore as it'd just be a call to load-texture-from-file anyway
       
	 (sdl:with-events ()
	   (:quit-event () t)

	   (:key-down-event (:key key)
			    (handle-keys key ))
	   
	   (:idle ()
		  (update)
		  (render)))
       (free-texture)))
     :name title))



