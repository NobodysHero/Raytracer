# Simple raytracer

A very simple implementation of a raytracer in Common Lisp (using SCBL as compiler). Basically I tried using Bagger's CEPL library to write a little shader - which went pretty well considering I knew next to nothing about OpenGL or shaders in the beginning.

# Installation

 - Git clone the repository someplace know to quicklisp
 - Load the project via (ql:quickload :raytracer-gpu)
 - Change to the package with (in-package :raytracer-gpu)
 - initialize the variables with the demo via (init) and start with (start)
