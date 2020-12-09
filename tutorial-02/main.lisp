;;;; Tutorial 02: Matrices and Coloring Polygons

(defpackage #:tutorial-02
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-02)

(defparameter *fps* 60)
(defparameter *Width* 640)
(defparameter *height* 480)


(defparameter *color-mode* 0)
(defparameter *projection-scale* 1.)

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


  (gl:translate (/ *width* 2.) (/ *height* 2.) 0.)

  (if (= *color-mode* 0)
      (progn
	(gl:with-primitive :quads
	(gl:color 0. 1. 1.)
	(gl:vertex -50. -50.)
	(gl:vertex 50. -50.)
	(gl:vertex 50. 50.)
	(gl:vertex -50. 50.)))
      (progn
	(gl:with-primitive :quads
	  (gl:color 1. 0. 0.) (gl:vertex -50. -50.)
	  (gl:color 1. 1. 0.) (gl:vertex 50. -50.)
	  (gl:color 0. 1. 0.) (gl:vertex 50. 50.)
	  (gl:color 0. 0. 1.) (gl:vertex -50. 50.))))
  
  (sdl:update-display))


(defun handle-keys (key &optional x y)
  (case key
    (:sdl-key-q
     (setf *color-mode*
	   (case *color-mode*
	     (0 1)
	     (1 0))))
    (:sdl-key-e
     (setf *projection-scale*
	   (case *projection-scale*
	     (1. 2.)
	     (2. 0.5)
	     (0.5 1.)))))

  
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 (* *width* *projection-scale*) (* *height* *projection-scale*) 0 1 -1))

(defun main (&aux (title "Tutorial 02: Matrices and Coloring Polygons"))
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


