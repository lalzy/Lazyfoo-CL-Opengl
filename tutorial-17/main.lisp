;;;; Tutorial 17: Vertex Buffer Objects

(defpackage #:tutorial-17
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-17)

(defparameter *fps* 60)
(defparameter *Width* 640)
(defparameter *height* 480)

(defparameter *gl-verticies* nil) ; Foreign pointer
(defparameter *gl-indices* nil) ; Foreign pointer

(defparameter *vertex-buffer* nil)
(defparameter *index-buffer* nil)

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

  (gl:bind-buffer :array-buffer *vertex-buffer*)
  (%gl:vertex-pointer 2 :float 0 (cffi:null-pointer))

  (gl:bind-buffer :element-array-buffer *index-buffer*)
  (%gl:draw-elements :quads 4 :unsigned-int (cffi:null-pointer))
  
  (gl:disable-client-state :vertex-array)
 
  
  (sdl:update-display))

(defun handle-keys (key &optional x y))

(defun load-media (&optional path)
  ;; Allocate a foreign pointers
  (setf *gl-indices* (cffi:foreign-alloc :int :initial-contents '(0 1 2 3))
	*gl-verticies* (cffi:foreign-alloc :float :initial-contents (list (* *width* (float (/ 1 4)))
									  (* *height* (float (/ 1 4)))
									  (* *width* (float (/ 3 4)))
									  (* *height* (float (/ 1 4)))
									  (* *width* (float (/ 3 4)))
									  (* *height* (float (/ 3 4)))
									  (* *width* (float (/ 1 4)))
									  (* *height* (float (/ 3 4))))))
  
  (setf *vertex-buffer* (gl:gen-buffer)
	*index-buffer* (gl:gen-buffer))
  
  ;; Create VBO
  (gl:bind-buffer :array-buffer *vertex-buffer*)
  (%gl:buffer-data :array-buffer (* 8 (cffi:foreign-type-size :float)) *gl-verticies* :static-draw)

  ;; Create IBO
  (gl:bind-buffer :element-array-buffer *index-buffer*)
  (%gl:buffer-data :element-array-buffer (* 4 (cffi:foreign-type-size '%gl:uint)) *gl-indices* :static-draw))


(defun main (&aux (title "Tutorial 17: Vertex Buffer Objects"))
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

       (gl:delete-buffers '(*vertex-buffer* *index-buffer*))
       (cffi:foreign-free *gl-verticies*)
       (cffi:foreign-free *gl-indices*)))
     :name title))


