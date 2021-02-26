;;;; Tutorial 18: Textured Vertex Buffers

(defpackage #:tutorial-18
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-18)

(defparameter *fps* 60)
(defparameter *Width* 640)
(defparameter *height* 480)

(defparameter *texture-id* 0)
(defparameter *texture-width* 0)
(defparameter *texture-height* 0)

(defparameter *VBOID* 0)
(defparameter *IBOID* 0)


(cffi:defcstruct VertexPos2D
  (x :float)
  (y :float))

(cffi:defcstruct LTexCoord
  (s :float)
  (t :float))

(cffi:defcstruct LVertexData2D
  (position  (:struct VertexPos2D))
  (texCoord (:struct LTexCoord)))


(defun set-slots (array-pointer index main-struct main-struct-slot data-struct data-struct-slot value)
  (setf (cffi:foreign-slot-value
	 (cffi:foreign-slot-pointer
	  (cffi:mem-aptr array-pointer `(:struct ,main-struct) index)
	  `(:struct ,main-struct)  main-struct-slot)
	 `(:struct ,data-struct) data-struct-slot)
	value))


(defun init-VBO ()
  (when (and (/= *texture-id* 0) (= *VBOID* 0))
    (let ((vdata (cffi:foreign-alloc '(:pointer (:STRUCT LVertexData2D)) :count 4))
	  (iData (cffi:foreign-alloc '%gl:uint :initial-contents '(0 1 2 3))))

      
      (setf *VBOID* (gl:gen-buffer)
	    *IBOID* (gl:gen-buffer))

      (gl:bind-buffer :array-buffer *VBOID*)
      (%gl:buffer-data :array-buffer (* 4 (cffi:foreign-type-size '(:STRUCT LVertexData2D)))  vData :dynamic-draw)

      (gl:bind-buffer :element-array-buffer *IBOID*)
      (%gl:buffer-data :element-array-buffer (* 4 (cffi:foreign-type-size '%gl:uint)) idata :dynamic-draw)

      (gl:bind-buffer :array-buffer 0)
      (gl:bind-buffer :element-array-buffer 0)
      
      (cffi:foreign-free vdata)
      (cffi:foreign-free idata))))

(defun free-VBO ()
  (when (/= *VBOID* 0)
    (gl:delete-buffers '(*iboid* *vboid*))
    (setf *iboid* 0
	  *vboid* 0)))

(defun free-texture ()
  (unless (= *texture-id* 0)
    (gl:delete-texture *texture-id*)
    (setf *texture-id* 0
	  *texture-width* 0
	  *texture-height* 0))

  ;; Not using a class structure, and we're only freeing textures at end
  (free-VBO))

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

  (gl:bind-texture :texture-2d 0)

  (init-VBO))

(defun texture-render (x y) 
  (unless (= *texture-id* 0)
    
    (let ((tex-top 0.0)
	  (tex-bottom 1.0)
	  (tex-left 0.0)
	  (tex-right 1.0)

	  (quad-width (float *texture-width*))
	  (quad-height (float *texture-height*)))
      

      (gl:translate  x  y 0)
      
      (let ((vData (cffi:foreign-alloc '(:STRUCT LVertexData2D) :count 4)))

	;; Texture Cordinates
	(set-slots vData 0 'LVertexData2D 'texCoord 'LTexCoord 's tex-left)
	(set-slots vData 0 'lvertexdata2d 'texcoord 'ltexcoord 't tex-top)
	
	(set-slots vData 1 'lvertexdata2d 'texcoord 'ltexcoord 's tex-right)
	(set-slots vData 1 'lvertexdata2d 'texcoord 'ltexcoord 't tex-top)
	
	(set-slots vData 2 'lvertexdata2d 'texcoord 'ltexcoord 's tex-right)
	(set-slots vData 2 'lvertexdata2d 'texcoord 'ltexcoord 't tex-bottom)
	
	(set-slots vData 3 'lvertexdata2d 'texcoord 'ltexcoord 's tex-left)
	(set-slots vData 3 'lvertexdata2d 'texcoord 'ltexcoord 't tex-bottom)


	;; Vertex Positions
	(set-slots vData 0 'lvertexdata2d 'position 'vertexpos2d 'x 0.0)
	(set-slots vData 0 'lvertexdata2d 'position 'vertexpos2d 'y 0.0)
	
	(set-slots vData 1 'lvertexdata2d 'position 'vertexpos2d 'x quad-width)
	(set-slots vData 1 'lvertexdata2d 'position 'vertexpos2d 'y 0.0)
	
	(set-slots vData 2 'lvertexdata2d 'position 'vertexpos2d 'x quad-width)
	(set-slots vData 2 'lvertexdata2d 'position 'vertexpos2d 'y quad-height)
	
	(set-slots vData 3 'lvertexdata2d 'position 'vertexpos2d 'x 0.0)
	(set-slots vData 3 'lvertexdata2d 'position 'vertexpos2d 'y quad-height)

	
	(gl:bind-texture :texture-2d *texture-id*)
	(gl:enable-client-state :vertex-array)
	(gl:enable-client-state :texture-coord-array)


	(gl:bind-buffer :array-buffer *VBOID*)
	(%gl:buffer-sub-data :array-buffer 0 (* 4 (cffi:foreign-type-size '(:struct lvertexdata2d))) vData)
	
	(%gl:tex-coord-pointer 2 :float (cffi:foreign-type-size '(:struct LVertexData2D)) (cffi:foreign-slot-offset '(:struct LVertexData2D) 'texcoord))
	
	(%gl:vertex-pointer 2 :float (cffi:foreign-type-size '(:struct lVertexData2D)) (cffi:foreign-slot-offset '(:struct lVertexData2D) 'position))
	
	(gl:bind-buffer :element-array-buffer *iboid*)
	(%gl:draw-elements :quads 4 :unsigned-int (cffi:null-pointer))

	(gl:disable-client-state :texture-coord-array)
	(gl:disable-client-state :vertex-array)

	(cffi:foreign-free vdata)
	)
)))

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
  (texture-render (round (- *width* *texture-width*) 2) (round (- *height* *Texture-height*) 2))
  
  (sdl:update-display))

(defun handle-keys (key &optional x y))

(defun load-media (&optional path)
  (load-texture-from-file path))

(defun main (&aux (title "Tutorial 18: Textured Vertex Buffers")
	       (path (concatenate 'string (namestring (asdf:system-relative-pathname :opengl-tutorials "tutorial-18/assets/")) "opengl.png")))
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


