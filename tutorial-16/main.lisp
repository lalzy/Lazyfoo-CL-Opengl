;;;; Tutorial 16: Vertex Arrays

(defpackage #:tutorial-16
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-16)

(defparameter *fps* 60)
(defparameter *Width* 640)
(defparameter *height* 480)


(cffi:defcstruct VertexPos2D
  (x :float)
  (y :float))


(defparameter *quad-verticies* nil)


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

  (gl:enable-client-state :VERTEX-ARRAY)

  (%gl:vertex-pointer 2 :float 0 *quad-verticies*)
  (gl:draw-arrays :quads 0 4)
  
  (gl:disable-client-state :vertex-array)
 
  
  (sdl:update-display))

(defun handle-keys (key &optional x y))

(defun set-slot (array-pointer index struct-name slot value)
  (setf (cffi:foreign-slot-value (cffi:mem-aptr array-pointer `(:struct ,struct-name) index) `(:struct ,struct-name) slot) value))

(defun load-media (&optional path)
  (setf *quad-verticies* (cffi:foreign-alloc '(:STRUCT VertexPos2D) :count 4))

  (set-slot *quad-verticies* 0 'vertexpos2D 'x (* *width* (float (/ 1 4))))
  (set-slot *quad-verticies* 0 'vertexpos2D 'y (* *height* (float (/ 1 4))))

  (set-slot *quad-verticies* 1 'vertexpos2D 'x (* *width* (float (/ 3 4))))
  (set-slot *quad-verticies* 1 'vertexpos2D 'y (* *height* (float (/ 1 4))))

  (set-slot *quad-verticies* 2 'vertexpos2D 'x (* *width* (float (/ 3 4))))
  (set-slot *quad-verticies* 2 'vertexpos2D 'y (* *height* (float (/ 3 4))))
  
  (set-slot *quad-verticies* 3 'vertexpos2D 'x (* *width* (float (/ 1 4))))
  (set-slot *quad-verticies* 3 'vertexpos2D 'y (* *height* (float (/ 3 4)))))

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
       
       (cffi:foreign-free *quad-verticies*)))
     :name title))



