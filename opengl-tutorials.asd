;;;; Lazy-foo.asd

(asdf:defsystem #:opengl-tutorials
  :description "CL-Opengl adaption of Lazy Foo's Open GL tutorials"
  :author "Lalzy"
  :serial t
  :depends-on (#:lispbuilder-sdl #:bt-semaphore #:cl-opengl
				 #:iterate #:cl-fond #:pngload)
  :components ((:file "tutorial-01/main") ; Hello OpenGL
	       (:file "tutorial-02/main") ; Matrices and Coloring Polygons
	       (:file "tutorial-03/main") ; The Viewport
	       (:file "tutorial-04/main") ; Scrolling and the Matrix Stack
	       (:file "tutorial-05/main") ; Texture Mapping and Pixel Manipulation
	       (:file "tutorial-06/main") ; Loading a Texture
	       (:file "tutorial-07/main") ; Clipping Textures
	       ;; skipping 8 as it's not relevant when using pngload
	       (:file "tutorial-09/main") ; Updating Textures
	       ))
