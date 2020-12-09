;;;; Tutorial 05: Texture Mapping and Pixel Manipilation

(defpackage #:tutorial-05
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-05)

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

(defun load-media ()
  (let* ((width 128)
	 (height 128)
	 (colour-count (* (* width height) 3))
	 (checkerboard (make-array colour-count))) ; CL-Opengl expects a single vector of all colors in the texture in order.

    (loop :for i :from 0 :below colour-count :do
      (if (> (logxor (logand (round i 128) 32)
		     (logand (mod i 128) 32)) 0)
	  (progn
	    (setf (aref checkerboard i) #xFF) (incf i)
	    (setf (aref checkerboard i) #xFF) (incf i)
	    (setf (aref checkerboard i) #xFF))
	  (progn
	    (setf (aref checkerboard i) #xFF) (incf i)
	    (setf (aref checkerboard i) #x00) (incf i)
	    (setf (aref checkerboard i) #x00))))
    (load-texture-from-pixels checkerboard width height)))

(defun update ())

(defun render ( )
  (gl:clear :color-buffer-bit)

  (let ((x (round (- *width* *texture-width*) 2))
	(y (round (- *height* *texture-height*) 2)))
    (texture-render x y))
  
  (sdl:update-display))



(defun handle-keys (key &optional x y))


(defun main (&aux (title "Tutorial 05: Texture Mapping and Pixel Manipilation"))
  (bt:make-thread
   (lambda ()
     (sdl:with-init ()
       (sdl:window *width* *height* :title-caption title :flags '(sdl:sdl-opengl))
       (setf (sdl:frame-rate) *fps*)

       (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)

       (sdl:enable-key-repeat 100 1)

       (init-gl)

       (load-media)
       
	 (sdl:with-events ()
	   (:quit-event () t)

	   (:key-down-event (:key key)
			    (handle-keys key ))
	   
	   (:idle ()
		  (update)
		  (render)))
       (free-texture)))
     :name title))



