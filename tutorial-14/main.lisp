;;;; Tutorial 14: Repeating Textures

(defpackage #:tutorial-14
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-14)

(defparameter *fps* 60)
(defparameter *Width* 640)
(defparameter *height* 480)

(defparameter *pixels* nil)
(defparameter *texture-id* 0)
(defparameter *texture-width* 0)
(defparameter *texture-height* 0)

(defparameter *tex-x* 0)
(defparameter *tex-y* 0)
(defparameter *texture-wrap-type* 0)

(defparameter *default-texture-wrap* :repeat)

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
  (gl:tex-parameter :texture-2d :texture-wrap-s *default-texture-wrap*)
  (gl:tex-parameter :texture-2d :texture-wrap-t *default-texture-wrap*)

  (gl:bind-texture :texture-2d 0))

;; Using pngload instead of devil for image.
(defun load-texture-from-file (path)
  (let ((image (pngload:load-file path :flatten t))) ;; Flatten because CL-Opengl expect a flat list of colors\pixels.
    (load-texture-from-pixels (pngload:data image) (pngload:width image) (pngload:height image)
			      (if (string-equal (pngload:color-type image) :truecolour-alpha) :rgba :rgb))))

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
  (incf *tex-x*)
  (incf *tex-y*)

  (when (>= *tex-x* *texture-width*)
    (setf *tex-x* 0))
  (when (>= *tex-y* *texture-height*)
    (setf *tex-y* 0)))

(defun render ( )
  (gl:clear :color-buffer-bit)

  (let ((texture-right (round *width* *texture-width*))
	(texture-bottom (round *height* *texture-height*)))
    (gl:bind-texture :texture-2d *texture-id*)
    (gl:matrix-mode :texture)
    (gl:load-identity)

    (gl:translate (round *tex-x* *texture-width*) (round *tex-y* *texture-height*) 0)

    (gl:begin :quads)
    (gl:tex-coord 0             0)              (gl:vertex 0       0)
    (gl:tex-coord texture-right 0)              (gl:vertex *width* 0)
    (gl:tex-coord texture-right texture-bottom) (gl:vertex *width* *height*)
    (gl:tex-coord 0             texture-bottom) (gl:vertex 0       *height*)
    (gl:end))
  
  (sdl:update-display))

(defun handle-keys (key &optional x y)
  (when (string-equal key :sdl-key-q)
    (incf *texture-wrap-type*)

    (when (>= *texture-wrap-type* 2)
      (setf *texture-wrap-type* 0))

    (gl:bind-texture :texture-2d *texture-id*)
    (case *texture-wrap-type*
      (0
       (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
       (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
       (format t "~a: Repeat~%" *texture-wrap-type*))
      (1
       (gl:tex-parameter :texture-2d :texture-wrap-s :clamp)
       (gl:tex-parameter :texture-2d :texture-wrap-t :clamp)
       (format t "~a: Clamp~%" *texture-wrap-type*)))))


(defun load-media (path)
  (load-texture-from-file path))

(defun main (&aux (title "Tutorial 14: Repeating Textures")
	       (path (concatenate 'string (namestring (asdf:system-relative-pathname :opengl-tutorials "tutorial-14/assets/")) "texture.png")))
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



