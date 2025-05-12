;; -*- mode: eask; lexical-binding: t -*-

(package "opencl-c-mode"
         "1.0"
         "Syntax coloring for opencl kernels")

(website-url "https://github.com/salmanebah/opencl-mode")
(keywords "c" "opencl")

(package-file "opencl-c-mode.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)

(depends-on "emacs" "26.1")

(add-hook 'eask-before-compile-hook
           (lambda ()
             (setq byte-compile-error-on-warn t)))
