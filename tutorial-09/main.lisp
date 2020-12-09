;;;; Tutorial 09: Updating Textures

(defpackage #:tutorial-09
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-09)

(defparameter *fps* 60)
(defparameter *Width* 640)
(defparameter *height* 480)

(defparameter *pixels* nil)
(defparameter *texture-id* 0)
(defparameter *texture-width* 0)
(defparameter *texture-height* 0)


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

  (gl:bind-texture :texture-2d 0))

;; Using pngload instead of devil for image.
(defun load-texture-from-file (path)
  (let ((image (pngload:load-file path :flatten t))) ;; Flatten because CL-Opengl expect a flat list of colors\pixels.
    (load-texture-from-pixels (pngload:data image) (pngload:width image) (pngload:height image)
			      (if (string-equal (pngload:color-type image) :truecolour-alpha) :rgba :rgb))))

(defun texture-render (x y &optional rect) 
  (unless (= *texture-id* 0)
    (gl:load-identity)
    
    (let ((tex-top 0.)
	  (tex-bottom 1.)
	  (tex-left 0.)
	  (tex-right 1.)

	  (quad-width *texture-width*)
	  (quad-height *texture-height*))
      
      (when rect
	(setf tex-left (/ (rect-x rect) *texture-width*)
	      tex-right (/ (+ (rect-x rect) (rect-w rect)) *texture-width*)
	      tex-top (/ (rect-y rect) *texture-height*)
	      tex-bottom (/ (+ (rect-y rect) (rect-h rect)) *texture-height*)
	      quad-width  (rect-w rect)
	      quad-height (rect-h rect)))

      (gl:translate x y 0)
      (gl:rotate 0 0. 0. 1.)

      (gl:bind-texture :texture-2d *texture-id*)

      (gl:with-primitive :quads
	(gl:tex-coord tex-left tex-top) (gl:vertex 0 0)
	(gl:tex-coord tex-right tex-top) (gl:vertex quad-width 0)
	(gl:tex-coord tex-right tex-bottom) (gl:vertex quad-width quad-height)
	(gl:tex-coord tex-left tex-bottom) (gl:vertex 0 quad-height)))))

(defun init-gl ()
  (gl:viewport 0. 0. *width* *height*)

  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 *width* *height* 0 1 -1)

  
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (gl:clear-color 0 0 0 1)
  
  (gl:enable :texture-2d))

(defun update ())

(defun render ( )
  (gl:clear :color-buffer-bit)

  (let ((x (round (- *width* *texture-width*) 2))
	(y (round (- *height* *texture-height*) 2)))
    (texture-render x y))
 
  (sdl:update-display))

(defun set-rect (rect x y w h)
  (setf (rect-x rect) x
	(rect-y rect) y
	(rect-w rect) w
	(rect-h rect) h))

(defun handle-keys (key &optional x y))

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



(defun load-media (path)
  (load-texture-from-file path)
  (lock)

  ;; Black out cyan
  (loop :for x :below *texture-width* :do
    (loop :for y :below *texture-height* :do
      (let ((pixel (get-pixel x y)))
	(when (and (= (elt pixel 0) #x0)
		   (= (elt pixel 1) #xFF)
		   (= (elt pixel 2) #xFF)
		   (= (elt pixel 3) #xFF))
	  (set-pixel x y #(#x0 #x0 #x0 #x0))))))


  ;;Diagonal pattern
  (loop :for y :below *texture-height* :do
    (loop :for x :below *texture-width* :do
      (when (/= (mod y 10) (mod x 10))
	(set-pixel x y #(#x00 #x00 #x00 #x00)))))

  
  (unlock)
  )

(defun main (&aux (title "Tutorial 09: Updating Textures")
	       (path (concatenate 'string (namestring (asdf:system-relative-pathname :opengl-tutorials "tutorial-09/assets/")) "circle.png")))
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



