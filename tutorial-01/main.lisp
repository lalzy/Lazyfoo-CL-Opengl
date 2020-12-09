;;;; Tutorial 01: Hello OpenGL

(defpackage #:tutorial-01
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-01)

(defparameter *fps* 60)
(defparameter *Width* 640)
(defparameter *height* 480)


(defun init-gl ()
  (gl:matrix-mode :projection)
  (gl:load-identity)

  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (gl:clear-color 0 0 0 1))


(defun update ())


(defun render ()
  (gl:clear :color-buffer-bit)
  
  (gl:with-primitive :quads
    (gl:vertex -.5 -.5)
    (gl:vertex .5 -.5)
    (gl:vertex .5 .5)
    (gl:vertex -.5 .5))
 
  (sdl:update-display))


(defun handle-keys (key &optional x y))


(defun main (&aux (title "Tutorial 01: Hello OpenGL"))
  (bt:make-thread
   (lambda ()
     (sdl:with-init ()
       (sdl:window *width* *height* :title-caption title :flags '(sdl:sdl-opengl))
       (setf (sdl:frame-rate) *fps*)

       (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)

       (sdl:enable-key-repeat 100 1)

       (init-gl)

	 (sdl:with-events ()
	   (:quit-event () t)

	   (:key-down-event (:key key)
			    (handle-keys key ))
	   
	   (:idle ()
		  (update)
		  (render)))))
     :name title))



