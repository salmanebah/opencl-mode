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

;; for c-font-lock-keywords
(require 'cc-fonts)

(defvar opencl-c-extension-color "#A82848"
  "OpenCL shader extension specification color.")

(defface font-lock-opencl-c-face
  `((t (:foreground ,opencl-c-extension-color :weight bold)))
  "Custom face for OpenCL shader extensions."
  :group 'opencl-c-faces)

(defvar opencl-c-keywords-regexp
  (concat "\\(__\\)?"
          (regexp-opt '("kernel" "global" "local" "constant" "private" "read_only" "write_only" "read_write" "enable" "disable") t)
          "[[:blank:]\n]+")
  "Regexp for OpenCL keywords.")

(defvar opencl-c-functions-regexp
  (regexp-opt '("get_work_dim" "get_global_size"
                "get_local_size" "get_global_id"
                "get_local_id" "get_num_groups"
                "get_group_id" "get_global_offset") 'words)
  "Regexp for builtin OpenCL functions.")

(defvar opencl-c-constant-regexp
  (regexp-opt '("MAXFLOAT" "HUGE_VALF"
                "INFINITY" "NAN" "HUGE_VAL") 'words)
  "Regexp for OpenCL constant.")

(defvar opencl-c-types-regexp
  (concat (regexp-opt '("char" "short" "half" "int" "double" "float" "long" "uchar" "ushort" "uint" "ulong") t)
          "[[:digit:]]\\{0,2\\}[[:blank:]\n]+")
  "Regexp for OpenCL primitive types.")

(defvar opencl-c-scalar-types-regexp
  (regexp-opt '("bool" "size_t" "ptrdiff_t" "intptr_t" "uintptr_t")
              'words)
  "Regexp for OpenCL scalar types.")

(defvar opencl-c-image-type-regexp
  (regexp-opt '("image2d_t" "image3d_t"
                "image2d_array_t" "image3d_array_t"
                "image1d_array_t" "image1d_t"
                "image1d_buffer_t" "sampler_t"
                "event_t") 'words)
  "Regexp for OpenCL image types.")

(defvar opencl-c-extension-regexp "cl_khr_[a-zA-Z][a-zA-Z_0-9]+"
  "Regex for OpenCL extensions.")

(defvar opencl-c-font-lock-keywords
  `((,opencl-c-functions-regexp . font-lock-builtin-face)
    (,opencl-c-types-regexp . font-lock-type-face)
    (,opencl-c-scalar-types-regexp . font-lock-type-face)
    (,opencl-c-constant-regexp . font-lock-constant-face)
    (,opencl-c-keywords-regexp . font-lock-keyword-face)
    (,opencl-c-image-type-regexp . font-lock-type-face)
    (,opencl-c-extension-regexp . 'font-lock-opencl-c-face))
  "Font-lock for OpenCL keywords.")

;;;###autoload
(define-derived-mode opencl-c-mode c-mode "Opencl"
  "Major mode for editing OpenCL C kernels."
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

(define-key opencl-c-mode-map (kbd "C-c ! d") 'opencl-c-lookup)

(provide 'opencl-c-mode)
;;; opencl-c-mode.el ends here
