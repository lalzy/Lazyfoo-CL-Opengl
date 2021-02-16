;;;; Tutorial 16: Vertex Arrays

(defpackage #:tutorial-16
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-16)

(defparameter *fps* 60)
(defparameter *Width* 640)
(defparameter *height* 480)

(defparameter *gl-verticies* nil) ; Foreign pointer


(defparameter *verticies-data* (make-array 8 :initial-contents
					   (list (* *width* (float (/ 1 4)))
						 (* *height* (float (/ 1 4)))
						 (* *width* (float (/ 3 4)))
						 (* *height* (float (/ 1 4)))
						 (* *width* (float (/ 3 4)))
						 (* *height* (float (/ 3 4)))
						 (* *width* (float (/ 1 4)))
						 (* *height* (float (/ 3 4))))))

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
  ;(texture-render 0 0)

  (gl:enable-client-state :VERTEX-ARRAY)

  ;; Allocate a foreign pointer to keep the verticies cordinates, which we'll pass to the vertex-pointer
  (setf *gl-verticies* 
	(cffi:foreign-alloc :float :count 8))

  ;; Fill the verticies cordinate data into the array pointer.
  (loop :for i :below 8 :do
    (setf (cffi:mem-aref *gl-verticies* :float i) (aref *verticies-data* i)))
  
  (%gl:vertex-pointer 2 :float 0 *gl-verticies*)
  (gl:draw-arrays :quads 0 4)
  
  (gl:disable-client-state :vertex-array)
 
  
  (sdl:update-display))

(defun handle-keys (key &optional x y))

(defun load-media (&optional path)
  ;; We're defining the verticies cordinates in the global variable instead
  
  ;;(load-texture-from-file path)
  )

(defun main (&aux (title "Tutorial 16: Vertex Arrays"))
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
       
       (cffi:foreign-free *gl-verticies*)))
     :name title))



