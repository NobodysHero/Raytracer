;;;; raytracer.asd

(asdf:defsystem #:raytracer-gpu
  :description "Describe raytracer here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:cepl.sdl2 :macro-utilities :rtg-math :rtg-math.vari :livesupport)
  :components ((:file "package")
               (:file "raytracer-gpu")
               ;(:file "test")
               ))

