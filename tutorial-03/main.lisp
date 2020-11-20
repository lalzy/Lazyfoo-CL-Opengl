;;;; Tutorial - The Viewport

(defpackage #:tutorial-03
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-03)

(defparameter *fps* 60)
(defparameter *Width* 640)
(defparameter *height* 480)


(defparameter *viewport-mode* :full)

(defun init-gl ()
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0. *width* *height* 0. 1. -1.)

  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (gl:clear-color 0 0 0 1))


(defun update ())

(defun render ()
  (gl:clear :color-buffer-bit)

  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (gl:translate (round *width* 2.) (round *height* 2.) 0.)

  (flet ((make-rectangle (color divident)
	   (gl:with-primitive :quads
	     (gl:color (aref color 0) (aref color 1) (aref color 2))
	     
	     (gl:vertex (round (- *width*) divident) (round  (- *height*) divident))
	     (gl:vertex (round  *width* divident) (round  (- *height*) divident))
	     (gl:vertex (round  *width* divident) (round  *height* divident))
	     (gl:vertex (round (- *width*) divident)  (round *height* divident)))))

    
    (case *viewport-mode*
      (:FULL
       (gl:viewport 0 0 *width* *height*)
       (make-rectangle #(1. 0. 0.) 2.))
      (:half-center
       (gl:viewport (round  *width* 4) (round  *height* 4) (round  *width* 2) (round  *height* 2))
       (make-rectangle #(0. 1. 0.) 2.))
      (:half-top
       (gl:viewport (round *width* 4) (round *height* 2) (round *width* 2) (round *Height* 2))
       (make-rectangle #(0. 0. 1.) 2.))
      (:quad
       (gl:viewport 0 0 (round *width* 2) (round *height* 2))
       (make-rectangle #(1. 0. 0.) 4.)
       
       (gl:viewport (round *width* 2) 0 (round *width* 2) (round *height* 2))
       (make-rectangle #(0. 1. 0.) 4.)

       (gl:viewport 0 (round *height* 2) (round *width* 2) (round *height* 2))
       (make-rectangle #(0. 0. 1.) 4.)

       (gl:viewport (round *width* 2) (round *height* 2) (round *width* 2) (round *Height* 2))
       (make-rectangle #(1. 1. 0.) 4.))
      (:radar
       (gl:viewport 0 0 *width* *height*)
       (make-rectangle #(1. 1. 1.) 8.)
       (make-rectangle #(0. 0. 0.) 16.)

       (gl:viewport (round *width* 2) (round *Height* 2) (round *width* 2) (round *height* 2))

       (make-rectangle #(1. 1. 1.) 8.)
       (make-rectangle #(0. 0. 0.) 16.))))
  
  (sdl:update-display))


(defun handle-keys (key &optional x y)
  (case key
    (:sdl-key-q
     (setf *viewport-mode*
	   (case *viewport-mode*
	     (:full :half-center)
	     (:half-center :half-top)
	     (:half-top :quad)
	     (:quad :radar)
	     (:radar :full))))))


(defun main (&aux (title "Tutorial 03 - The Viewport"))
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


