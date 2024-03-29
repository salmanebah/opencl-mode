;;; opencl-c-mode.el --- Syntax coloring for opencl kernels -*- lexical-binding: t -*-

;; Copyright (c) 2014- Salmane Bah <salmane.bah ~at~ u-bordeaux.fr>
;; Copyright (c) 2024- Gustaf Waldemarson <gustaf.waldemarson ~at~ gmail.com>
;;
;; Authors: Salmane Bah <salmane.bah ~at~ u-bordeaux.fr>
;;          Gustaf Waldemarson <gustaf.waldemarson ~at~ gmail.com>
;; Keywords: c, opencl
;; URL: https://github.com/salmanebah/opencl-mode
;; Version: 1.0

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Installation:

;; Major mode for editing OpenCL-C files.

;;; Code:

(require 'cc-mode)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(defvar opencl-c-extension-color "#A82848"
  "OpenCL shader extension specification color.")

(defvar opencl-c-extension-face 'opencl-c-extension-face)
(defface opencl-c-extension-face
  `((t (:foreground ,opencl-c-extension-color :weight bold)))
  "Custom face for OpenCL shader extensions."
  :group 'opencl-c-faces)


(defvar opencl-c-builtins
  '("get_work_dim"
    "get_global_size"
    "get_local_size"
    "get_global_id"
    "get_local_id"
    "get_num_groups"
    "get_group_id"
    "get_global_offset")
  "List of builtin OpenCL functions.")

(defvar opencl-c-constants
  '("MAXFLOAT"
    "HUGE_VALF"
    "INFINITY"
    "NAN"
    "HUGE_VAL")
  "List of OpenCL constant.")

(defvar opencl-c-primitive-types
  '("bool"
    "char"
    "uchar"
    "short"
    "ushort"
    "int"
    "uint"
    "long"
    "ulong"
    "half"
    "float"
    "double"
    "size_t"
    "ptrdiff_t"
    "intptr_t"
    "uintptr_t"
    "void")
  "List of primitive types in OpenCL C.")

(defvar opencl-c-vector-types
  (mapcan (lambda (type)
            (mapcar (lambda (n) (concat type (number-to-string n)))
                    '(2 3 4 8 16)))
          '("char" "uchar" "short" "ushort" "int" "uint"
            "long" "ulong" "float" "double"))
  "List of all vector types in OpenCL C.")

(defvar opencl-c-descriptor-types
  '("image2d_t"
    "image3d_t"
    "image2d_array_t"
    "image1d_t"
    "image1d_array_t"
    "image1d_buffer_t"
    "image2d_depth_t"
    "image2d_array_depth_t"
    "sampler_t"
    "queue_t"
    "ndrange_t"
    "clk_event_t"
    "reserve_id_t"
    "event_t"
    "cl_mem_fence_flags")
  "List of all other OpenCL types.")


(defvar opencl-c-builtins-rx (regexp-opt opencl-c-builtins 'symbols))

(defvar opencl-c-extensions-rx
  (rx (group-n 1 "#pragma")
      (+ (in space))
      (seq "OPENCL" (+ (in space)) "EXTENSION" (+ (in space)))
      (group-n 2 (seq "cl_khr_" (in "a-zA-Z") (+ (in "a-zA-Z_0-9"))))
      (* (in space)) ":" (* (in space))
      (or (group-n 3 (or "require" "enable"))
          (group-n 4 (or "warn" "disable"))))
  "Regex for OpenCL extensions.")


(defvar opencl-c-font-lock-keywords
  `((,opencl-c-builtins-rx . font-lock-builtin-face)
    (,opencl-c-extensions-rx (2 'opencl-c-extension-face nil lax)
                     (3 '(face font-lock-keyword-face) nil lax)
                     (4 '(face font-lock-warning-face) nil lax)))
  "Additional font-locking rules for OpenCL C.")

(defvar opencl-c-mode-syntax-table
  (let ((tbl (make-syntax-table c-mode-syntax-table)))
    tbl)
  "Syntax table for `opencl-c-mode'.")

(defvar opencl-c-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ! d") 'opencl-c-lookup)
    map)
  "Keymap for `opencl-c-mode'.")

(defvar opencl-c-mode-hook nil "Mode hook for `opencl-c-mode'.")


(eval-and-compile (c-add-language 'opencl-c-mode 'c-mode))

(c-lang-defconst c-primitive-type-kwds
  "Primitive type keywords.  As opposed to the other keyword lists, the
keywords listed here are fontified with the type face instead of the
keyword face.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled.

Do not try to modify this list for end user customizations; the
`*-font-lock-extra-types' variable, where `*' is the mode prefix, is
the appropriate place for that."
  glsl
  (append
   opencl-c-primitive-types
   opencl-c-vector-types
   opencl-c-descriptor-types
   ;; Use append to not be destructive on the return value below.
   (append
    (c-lang-const c-primitive-type-kwds)
    nil)))

(c-lang-defconst c-modifier-kwds
  opencl-c
  (append (c-lang-const c-modifier-kwds)
          '("__kernel" "__global" "__local" "__constant" "__private" "__generic"
            "__read_only" "__write_only" "__read_write"
            "kernel" "global" "local" "constant" "private" "generic"
            "read_only" "write_only" "read_write"
            "uniform" "pipe")))

(defconst opencl-c-font-lock-keywords-1 (c-lang-const c-matchers-1 opencl-c))
(defconst opencl-c-font-lock-keywords-2 (c-lang-const c-matchers-2 opencl-c))
(defconst opencl-c-font-lock-keywords-3 (c-lang-const c-matchers-3 opencl-c))
(defvar opencl-c-font-lock-keywords (c-lang-const c-matchers-3 opencl-c))
(defun opencl-c-font-lock-keywords ()
  "Compose a list of font-locking keywords for `opencl-c-mode'."
  (c-compose-keywords-list opencl-c-font-lock-keywords))


;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . opencl-c-mode))
  (add-to-list 'auto-mode-alist '("\\.clc\\'" . opencl-c-mode))
  (add-to-list 'auto-mode-alist '("\\.opencl\\'" . opencl-c-mode)))

;;;###autoload
(define-derived-mode opencl-c-mode prog-mode "OpenCL[C]"
  "Major mode for editing OpenCL C kernels.

\\{opencl-c-mode-map}"
  :syntax-table opencl-c-mode-syntax-table
  :after-hook (progn (c-make-noise-macro-regexps)
		     (c-make-macro-with-semi-re)
		     (c-update-modeline))
  (c-initialize-cc-mode t)
  (c-init-language-vars opencl-c-mode)
  (c-common-init 'opencl-c-mode)
  (cc-imenu-init cc-imenu-c++-generic-expression)

  (c-run-mode-hooks 'c-mode-common-hook)
  (run-mode-hooks 'opencl-c-mode-hook)

  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-padding "")

  (font-lock-add-keywords nil opencl-c-font-lock-keywords))

(defun opencl-c-lookup ()
  "Get OpenCL documentation for string in region or point."
  (interactive)
  (let* ((api-function (if (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end))
                         (thing-at-point 'symbol)))
         (doc-url (concat
                   "http://www.khronos.org/registry/cl/sdk/2.1/docs/man/xhtml/"
                   api-function ".html")))
    (browse-url doc-url)))

(provide 'opencl-c-mode)
;;; opencl-c-mode.el ends here
