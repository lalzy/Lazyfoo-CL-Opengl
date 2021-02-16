;;;; Tutorial 16: Vertex Arrays

(defpackage #:tutorial-16
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-16)

(defparameter *fps* 60)
(defparameter *Width* 640)
(defparameter *height* 480)

(defparameter *pixels* nil)
(defparameter *texture-id* 0)
(defparameter *texture-width* 0)
(defparameter *texture-height* 0)
(defparameter *gl-filtering* :LINEAR)


(defparameter *tex-x* 0)
(defparameter *tex-y* 0)
(defparameter *texture-wrap-type* 0)

(defparameter *default-texture-wrap* :repeat)

(defparameter *angle* 0)
(defparameter *transformation-combo* 0)


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

(defstruct rect
  (x 0)
  (y 0)
  (w 0)
  (h 0))

(defun lock (&aux (channels 4))
  (when (and (not *pixels*) (not (= *texture-id* 0)))
    (setf *pixels* (cffi:foreign-alloc '%gl:ubyte
				       :count (* *texture-width* *texture-height* channels)))
    
    (loop for i from 0 below (* *texture-width* *texture-height* channels) do
      (setf (cffi:mem-aref *pixels* '%gl:ubyte i) #x00))
    
    (gl:bind-texture :texture-2D *texture-id*)

    (%gl:get-tex-image :texture-2d 0 :rgba :unsigned-byte *pixels*)

    (gl:bind-texture :texture-2d 0)))

(defun unlock ()
  (when (and *pixels* (not (= *texture-id* 0)))
    (gl:bind-texture :texture-2D *texture-id*)

    (%gl:tex-sub-image-2D :texture-2D 0 0 0 *texture-width* *texture-height* :rgba :unsigned-byte *pixels*)

    (cffi:foreign-free *pixels*)
    (setf *pixels* nil)
    
    (gl:bind-texture :texture-2D 0)))

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

;; Does not work on RGB images currently
(defun load-texture-from-file-with-color-key (path red green blue &optional (alpha 0))
  (let* ((image (pngload:load-file path :flatten t))
	 (pixels (pngload:data image)))

    (loop  :for i :below (length pixels) :by 4 :do
      (when (and (= (elt pixels i) red)
		   (= (elt pixels (+ i 1)) green)
		   (= (elt pixels (+ i 2)) blue)
		   (or (= alpha 0)
		       (= (elt pixels (+ i 3)) alpha)))

	;; Since we're working with a flat list of all the colors, and not pixels as arrays
	;; we only need to change the alpha channel
	  (setf (elt pixels (+ i 3)) #x00)))
    (load-texture-from-pixels pixels (pngload:width image) (pngload:height image) :RGBA)))

(defun texture-render (x y &key rect) 
  (unless (= *texture-id* 0)
    
    (let ((tex-top 0.)
	  (tex-bottom 1)
	  (tex-left 0.)
	  (tex-right 1)

	  (quad-width *texture-width*)
	  (quad-height *texture-height*))
      
      (when rect
	(setf tex-left (/ (rect-x rect) *texture-width*)
	      tex-right (/ (+ (rect-x rect) (rect-w rect)) *texture-width*)
	      tex-top (/ (rect-y rect) *texture-height*)
	      tex-bottom (/ (+ (rect-y rect) (rect-h rect)) *texture-height*)
	      quad-width  (rect-w rect)
	      quad-height (rect-h rect)))

      (gl:translate  x  y 0)
      
      
      (gl:bind-texture :texture-2d *texture-id*)

      (gl:with-primitive :quads
	(gl:tex-coord tex-left tex-top)     (gl:vertex 0          0)
	(gl:tex-coord tex-right tex-top)    (gl:vertex quad-width 0)
	(gl:tex-coord tex-right tex-bottom) (gl:vertex quad-width quad-height)
	(gl:tex-coord tex-left tex-bottom)  (gl:vertex 0          quad-height)))))

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

(defun set-rect (rect x y w h)
  (setf (rect-x rect) x
	(rect-y rect) y
	(rect-w rect) w
	(rect-h rect) h))

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

(defun get-channels-from-format (format)
  (case format
    (:RGB 3)
    (:RGBA 4)))

(defun get-pixel (x y &optional (format :RGBA))
  (let* ((color-channels (get-channels-from-format format))
	 (row (* y (* *texture-width* color-channels)))
	(col (* x color-channels))
	(mempos (+ row col)))
    (loop :for i :below color-channels :collect
			  (cffi:mem-aref *pixels* '%gl:ubyte (+ mempos i)))))

(defun set-pixel (x y pixel &optional (format :RGBA))
  (let* ((color-channels (get-channels-from-format format))
	 (row (* y (* *texture-width* color-channels)))
	(col (* x color-channels))
	(mempos (+ row col)))
    (loop :for i :below color-channels
	  :do
	     (setf (cffi:mem-aref *pixels* '%gl:ubyte (+ mempos i)) (aref pixel i)))))

(defun load-media (&optional path)
  ;; We're defining the verticies cordinates in the global variable instead
  
  ;;(load-texture-from-file path)
  )

(defun main (&aux (title "Tutorial 16: Vertex Arrays")
	       ;;(path (concatenate 'string (namestring (asdf:system-relative-pathname :opengl-tutorials "tutorial-16/assets/")) "texture.png"))
	       )
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
       
       (cffi:foreign-free *gl-verticies*)
       (free-texture)))
     :name title))



