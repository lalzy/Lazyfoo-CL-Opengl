;;;; Tutorial 04: Scrolling and the Matrix Stack

(defpackage #:tutorial-04
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-04)

(defparameter *fps* 60)
(defparameter *Width* 640)
(defparameter *height* 480)

(defparameter *camera* #(0. 0.))


(defun init-gl ()
  (gl:viewport 0. 0. *width* *height*)

  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 *width* *height* 0 1 -1)

  
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (gl:push-matrix)
  
  (gl:clear-color 0 0 0 1))


(defun update ())


(defun render ()
  (gl:clear :color-buffer-bit)

  (gl:matrix-mode :modelview)
  (gl:pop-matrix)

  (gl:push-matrix)
  
  (flet ((make-rectangle (color)
	   (gl:with-primitive :quads
	     (gl:color (aref color 0) (aref color 1) (aref color 2))
	     (gl:vertex (round (- *width*) 4) (round (- *height*) 4))
	     (gl:vertex (round *width* 4) (round (- *height*) 4))
	     (gl:vertex (round *width* 4) (round *height* 4))
	     (gl:vertex (round (- *width*) 4) (round *height* 4)))))

    ;; Red Quad
    (gl:translate  (round *width* 2) (round *height* 2) 0)
    (make-rectangle #(1. 0. 0.))

    ;; Green quad
    (gl:translate *width* 0 0)
    (make-rectangle #(0. 1. 0.))

    ;; Blue quad
    (gl:translate 0 *height* 0)
    (make-rectangle #(0. 0. 1.))

    ;; Yellow quad
    (gl:translate (- *width*) 0 0)
    (make-rectangle #(1. 1. 0.)))
 
  (sdl:update-display))


(defun handle-keys (key &optional x y)
  (when (string-equal key :sdl-key-w) (incf (aref *camera* 1) 16.))
  (when (string-equal key :sdl-key-s) (decf (aref *camera* 1) 16.))
  (when (string-equal key :sdl-key-a) (incf (aref *camera* 0) 16.))
  (when (string-equal key :sdl-key-d) (decf (aref *camera* 0) 16.))

  (gl:matrix-mode :modelview)
  (gl:pop-matrix)
  (gl:load-identity)

  (gl:translate (aref *camera* 0) (aref *camera* 1) 0)
  (gl:push-matrix))


(defun main (&aux (title "Tutorial 04: Scrolling and the Matrix Stack"))
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



