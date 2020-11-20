;;;; Lazy-foo.asd

(asdf:defsystem #:opengl-tutorials
  :description "CL-Opengl adaption of Lazy Foo's Open GL tutorials"
  :author "Lalzy"
  :serial t
  :depends-on (#:lispbuilder-sdl #:lispbuilder-sdl-image  #:bt-semaphore #:cl-opengl
				 #:iterate #:cl-fond #:pngload)
  :components ((:file "tutorial-01/main")
	       (:file "tutorial-02/main")
	       (:file "tutorial-03/main")
	       (:file "tutorial-04/main")
	       (:file "tutorial-05/main")
	       (:file "tutorial-06/main")
	       (:file "tutorial-07/main")
	       ;; skipping 8 as it's not relevant when using pngload
	       (:file "tutorial-09/main")))
