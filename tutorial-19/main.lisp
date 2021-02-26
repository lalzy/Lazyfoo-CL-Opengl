;;;; Tutorial 19: Sprite Sheets

(defpackage #:tutorial-19
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-19)

(defparameter *fps* 60)
(defparameter *Width* 640)
(defparameter *height* 480)

(defparameter *texture-id* 0)
(defparameter *texture-width* 0)
(defparameter *texture-height* 0)

(defparameter *Clips* (make-array 0 :adjustable t :fill-pointer 0))

(defparameter *vertex-data-buffer* nil)
(defparameter *index-buffers* nil)

(cffi:defcstruct VertexPos2D
  (x :float)
  (y :float))

(cffi:defcstruct LTexCoord
  (s :float)
  (t :float))

(cffi:defcstruct LVertexData2D
  (position  (:struct VertexPos2D))
  (texCoord (:struct LTexCoord)))

(defstruct rect (x 0) (y 0) (w 0) (h 0))

(defun add-clip-sprite (new-clip)
  (vector-push-extend new-clip *clips*))

(defun set-slots (array-pointer index main-struct main-struct-slot data-struct data-struct-slot value)
  (setf (cffi:foreign-slot-value
	 (cffi:foreign-slot-pointer
	  (cffi:mem-aptr array-pointer `(:struct ,main-struct) index)
	  `(:struct ,main-struct)  main-struct-slot)
	 `(:struct ,data-struct) data-struct-slot)
	value))

(defun generate-data-buffer ()
  (if (and (/= *texture-id* 0) (> (length *clips*) 0))
    (let* ((total-sprites (length *clips*))
	   (vertex-data (cffi:foreign-alloc '(:struct lvertexdata2d) :count (* total-sprites 4)))
	   )
      
      (setf *vertex-data-buffer* (gl:gen-buffer)
	    *index-buffers*  (gl:gen-buffers total-sprites))
      
      (loop :with sprite-indices = (cffi:foreign-alloc '%gl:uint :count 4)
	    :for i :below total-sprites :do
	      (setf (cffi:mem-aref sprite-indices '%gl:uint 0) (+ (* i 4) 0)
		    (cffi:mem-aref sprite-indices '%gl:uint 1) (+ (* i 4) 1)
		    (cffi:mem-aref sprite-indices '%gl:uint 2) (+ (* i 4) 2)
		    (cffi:mem-aref sprite-indices '%gl:uint 3) (+ (* i 4) 3))
	      
	      ;; Top Left
	      (set-slots vertex-data (cffi:mem-aref sprite-indices '%gl:uint 0) 'lvertexdata2D 'position 'vertexpos2d 'x
			 (float (/ (- (rect-w (aref *clips* i))) 2.0)))
	      
	      (set-slots vertex-data (cffi:mem-aref sprite-indices '%gl:uint 0) 'lvertexdata2D 'position 'vertexpos2d 'y
			 (float (/ (- (rect-h (aref *clips* i))) 2.0)))
	      
	      (set-slots vertex-data (cffi:mem-aref sprite-indices '%gl:uint 0) 'LVertexData2D 'texCoord 'LTexCoord 's
			 (float (/ (rect-x (aref *clips* i)) *texture-width*)))
	      
	      (set-slots vertex-data (cffi:mem-aref sprite-indices '%gl:uint 0) 'LVertexData2D 'texCoord 'LTexCoord 't
			 (float (/ (rect-y (aref *clips* i)) *texture-height*)))

	      ;; Top Right
	      (set-slots vertex-data (cffi:mem-aref sprite-indices '%gl:uint 1) 'lvertexdata2D 'position 'vertexpos2d 'x
			 (float (/ (rect-w (aref *clips* i)) 2.0)))
	      
	      (set-slots vertex-data (cffi:mem-aref sprite-indices '%gl:uint 1) 'lvertexdata2D 'position 'vertexpos2d 'y
			 (- (float (/ (rect-h (aref *clips* i)) 2.0))))
	      
	      (set-slots vertex-data (cffi:mem-aref sprite-indices '%gl:uint 1) 'LVertexData2D 'texCoord 'LTexCoord 's
			 (float (/ (+ (rect-x (aref *clips* i)) (rect-w (aref *clips* i))) *texture-width*)))
	      
	      (set-slots vertex-data (cffi:mem-aref sprite-indices '%gl:uint 1) 'LVertexData2D 'texCoord 'LTexCoord 't
			 (float (/ (rect-y (aref *clips* i)) *texture-height*)))


	      ;; Bottom Right

	      (set-slots vertex-data (cffi:mem-aref sprite-indices '%gl:uint 2) 'lvertexdata2D 'position 'vertexpos2d 'x
			 (float (/ (rect-w (aref *clips* i)) 2.0)))
	      
	      (set-slots vertex-data (cffi:mem-aref sprite-indices '%gl:uint 2) 'lvertexdata2D 'position 'vertexpos2d 'y
			 (float (/ (rect-h (aref *clips* i)) 2.0)))
	      
	      (set-slots vertex-data (cffi:mem-aref sprite-indices '%gl:uint 2) 'LVertexData2D 'texCoord 'LTexCoord 's
			 (float (/ (+ (rect-x (aref *clips* i)) (rect-w (aref *clips* i))) *texture-width*)))
	      
	      (set-slots vertex-data (cffi:mem-aref sprite-indices '%gl:uint 2) 'LVertexData2D 'texCoord 'LTexCoord 't
			 (float (/ (+ (rect-y (aref *clips* i)) (rect-h (aref *clips* i))) *texture-height*)))


	      ;; Bottom Left
	      
	      (set-slots vertex-data (cffi:mem-aref sprite-indices '%gl:uint 3) 'lvertexdata2D 'position 'vertexpos2d 'x
			 (- (float (/ (rect-w (aref *clips* i)) 2.0))))
	      
	      (set-slots vertex-data (cffi:mem-aref sprite-indices '%gl:uint 3) 'lvertexdata2D 'position 'vertexpos2d 'y
			 (float (/ (rect-h (aref *clips* i)) 2.0)))
	      
	      (set-slots vertex-data (cffi:mem-aref sprite-indices '%gl:uint 3) 'LVertexData2D 'texCoord 'LTexCoord 's
			 (float (/ (rect-x (aref *clips* i)) *texture-width*)))
	      
	      (set-slots vertex-data (cffi:mem-aref sprite-indices '%gl:uint 3) 'LVertexData2D 'texCoord 'LTexCoord 't
			 (float (/ (+ (rect-y (aref *clips* i)) (rect-h (aref *clips* i))) *texture-height*)))

	      
	      (gl:bind-buffer :element-array-buffer (elt *index-buffers* i))
	      (%gl:buffer-data :element-array-buffer (* 4 (cffi:foreign-type-size '%gl:uint)) sprite-indices :static-draw)
	      
	    :finally
	    (cffi:foreign-free sprite-indices))
      (gl:bind-buffer :array-buffer *vertex-data-buffer*)
      (%gl:buffer-data :array-buffer
		       (* 4 total-sprites (cffi:foreign-type-size '(:struct LVertexData2D)))
		       vertex-data :static-draw)
      (cffi:foreign-free vertex-data))
    (progn
      (when (= *texture-id* 0)
	(format t "No Texture to render with!~%"))
      (when (= (length *clips*) 0)
	(format t "No clips to generate vertexc data from!~%")))))


(defun free-sheet ()
  (when *vertex-data-buffer*
    (gl:delete-buffers '(*vertex-data-buffer*))
    (setf *vertex-data-buffer* nil))

  (when *index-buffers*
    (gl:delete-buffers '(*index-buffers*))
    (setf *index-buffers* nil))

  (setf *clips* (make-array 0 :adjustable t :fill-pointer 0)))

(defun sprite-render (index)
  (when *vertex-data-buffer*
    (gl:bind-texture :texture-2d *texture-id*)

    (gl:enable-client-state :vertex-array)
    (gl:enable-client-state :texture-coord-array)

    (gl:bind-buffer :array-buffer *vertex-data-buffer*)
    (%gl:tex-coord-pointer 2 :float (cffi:foreign-type-size '(:struct lvertexdata2D)) (cffi:foreign-slot-offset '(:struct lvertexdata2D) 'texcoord))

    (%gl:vertex-pointer 2 :float (cffi:foreign-type-size '(:struct lvertexdata2D)) (cffi:foreign-slot-offset '(:struct lvertexdata2D) 'position))

    (gl:bind-buffer :element-array-buffer (elt *index-buffers* index))
    (%gl:draw-elements :quads 4 :unsigned-int (cffi:null-pointer))

    (gl:disable-client-state :texture-coord-array)
    (gl:disable-client-state :vertex-array)))

(defun free-texture ()
  (free-sheet)
  
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
  (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
  (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)

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

(defun update ())

(defun render ( )
  (gl:clear :color-buffer-bit)

  (gl:load-identity)
  (gl:translate 64.0 64.0 0.)
  (sprite-render 0)

  (gl:load-identity)
  (gl:translate (- *width* 64.0) 64.0 0.)
  (sprite-render 1)
  
  (gl:load-identity)
  (gl:translate 64.0 (- *height* 64.0) 0.)
  (sprite-render 2)

  (gl:load-identity)
  (gl:translate (- *width* 64.0) (- *height* 64.0) 0.)
  (sprite-render 3)
  
  (sdl:update-display))

(defun handle-keys (key &optional x y))

(defun load-media (&optional path)
  (load-texture-from-file path)

  (let ((w 128)
	(h 128))
    (add-clip-sprite (make-rect :x 0   :y   0 :w w :h h))
    (add-clip-sprite (make-rect :x 128 :y   0 :w w :h h))
    (add-clip-sprite (make-rect :x 0   :y 128 :w w :h h))
    (add-clip-sprite (make-rect :x 128 :y 128 :w w :h h)))

  (generate-data-buffer))

(defun main (&aux (title "Tutorial 19: Sprite Sheets")
	       (path (concatenate 'string (namestring (asdf:system-relative-pathname :opengl-tutorials "tutorial-19/assets/")) "arrows.png")))
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


