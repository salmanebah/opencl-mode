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


(defvar opencl-c-vload-vstore-builtins
  (append
   (mapcan
    (lambda (fn)
      (mapcar
       (lambda (n) (concat fn n))
       '("2" "3" "4" "8" "16")))
    '("vstore" "vload"))
   (mapcan
    (lambda (fn)
      (mapcan
       (lambda (n)
         (mapcar
          (lambda (rm) (concat fn rm n))
          '("" "_rte" "_rtz" "_rtp" "_rtn")))
       '("" "2" "3" "4" "8" "16")))
    '("vstore_half" "vstorea_half" "vload_half" "vloada_half")))
  "List of vector load-store built-ins in OpenCL C.")

(defvar opencl-c-builtins
  `("get_work_dim"
    "get_global_size"
    "get_global_id"
    "get_local_size"
    "get_enqueued_local_size"
    "get_local_id"
    "get_num_groups"
    "get_group_id"
    "get_global_offset"
    "get_global_linear_id"
    "get_local_linear_id"

    "get_sub_group_size"
    "get_max_sub_group_size"
    "get_num_sub_groups"
    "get_enqueued_num_sub_groups"
    "get_sub_group_id"
    "get_sub_group_local_id"

    "acos"
    "acosh"
    "acospi"
    "asin"
    "asinh"
    "asinpi"
    "atan"
    "atan2"
    "atanh"
    "atanpi"
    "atan2pi"
    "cbrt"
    "ceil"
    "copysign"
    "cos"
    "cosh"
    "cospi"
    "erfc"
    "erf"
    "exp"
    "exp2"
    "exp10"
    "expm1"
    "fabs"
    "fdim"
    "floor"
    "fma"
    "fmax"
    "fmin"
    "fmod"
    "fract"
    "frexp"
    "hypot"
    "ilogb"
    "lgamma"
    "lgamma_r"
    "log"
    "log2"
    "log10"
    "log1p"
    "logb"
    "mad"
    "maxmag"
    "minmag"
    "modf"
    "nan"
    "nextafter"
    "pow"
    "pown"
    "powr"
    "remainder"
    "remquo"
    "rint"
    "rootn"
    "round"
    "rsqrt"
    "sin"
    "sincos"
    "sinh"
    "sinpi"
    "sqrt"
    "tan"
    "tanh"
    "tanpi"
    "tgamma"
    "trunc"

    "half_cos"
    "half_divide"
    "half_exp"
    "half_exp2"
    "half_exp10"
    "half_log"
    "half_log2"
    "half_log10"
    "half_powr"
    "half_recip"
    "half_rsqrt"
    "half_sin"
    "half_sqrt"
    "half_tan"
    "native_cos"
    "native_divide"
    "native_exp"
    "native_exp2"
    "native_exp10"
    "native_log"
    "native_log2"
    "native_log10"
    "native_powr"
    "native_recip"
    "native_rsqrt"
    "native_sin"
    "native_sqrt"
    "native_tan"

    "abs"
    "abs_diff"
    "add_sat"
    "hadd"
    "rhadd"
    "clamp"
    "clz"
    "ctz"
    "dot"
    "dot_acc_sat"
    "dot_4x8packed_uu_uint"
    "dot_acc_sat_4x8packed_uu_uint"
    "mad_hi"
    "mad_sat"
    "max"
    "min"
    "mul_hi"
    "rotate"
    "sub_sat"
    "upsample"
    "popcount"

    "mad24"
    "mul24"

    "bitfield_insert"
    "bitfield_extract_signed"
    "bitfield_extract_unsigned"
    "bit_reverse"

    "cross"
    "dot"
    "distance"
    "length"
    "normalize"
    "fast_distance"
    "fast_length"
    "fast_normalize"
    "half_rsqrt"

    "isequal"
    "isnotequal"
    "isgreater"
    "isgreaterequal"
    "isless"
    "islessequal"
    "islessgreater"
    "isfinite"
    "isinf"
    "isnan"
    "isnormal"
    "isordered"
    "isunordered"
    "signbit"
    "any"
    "all"
    "bitselect"
    "select"

    "clamp"
    "degrees"
    "max"
    "min"
    "mix"
    "radians"
    "step"
    "smoothstep"
    "sign"

    "barrier"
    "work_group_barrier"
    "work_group_barrier"
    "sub_group_barrier"
    "sub_group_barrier"

    "mem_fence"
    "read_mem_fence"
    "write_mem_fence"

    "to_global"
    "to_local"
    "to_private"
    "get_fence"

    ,@opencl-c-vload-vstore-builtins

    "async_work_group_copy"
    "async_work_group_strided_copy"
    "async_work_group_copy_fence"
    "wait_group_events"
    "prefetch"

    "async_work_group_copy_2D2D"
    "async_work_group_copy_3D3D"

    "vec_step"
    "shuffle"
    "shuffle2"

    "printf"

    "read_imagef"
    "read_imageh"
    "read_imagei"
    "read_imageui"

    "write_imagef"
    "write_imageh"
    "write_imagei"
    "write_imageui"

    "get_image_width"
    "get_image_height"
    "get_image_depth"
    "get_image_channel_data_type"
    "get_image_channel_order"
    "get_image_dim"
    "get_image_dim"
    "get_image_array_size"
    "get_image_num_samples"
    "get_image_num_mip_levels"

    "ATOMIC_VAR_INIT"
    "atomic_init"

    "atomic_work_item_fence"
    "mem_fence"
    "read_mem_fence"
    "write_mem_fence"

    "atomic_store"
    "atomic_store_explicit"
    "atomic_load"
    "atomic_load_explicit"

    "atomic_exchange"
    "atomic_exchange_explicit"
    "atomic_compare_exchange_strong"
    "atomic_compare_exchange_strong_explicit"
    "atomic_compare_exchange_weak"
    "atomic_compare_exchange_weak_explicit"
    "atomic_fetch_key"
    "atomic_fetch_key_explicit"
    "atomic_flag_test_and_set"
    "atomic_flag_test_and_set_explicit"
    "atomic_flag_clear"
    "atomic_flag_clear_explicit"

    "atomic_add"
    "atom_add"
    "atomic_sub"
    "atom_sub"
    "atomic_xchg"
    "atom_xchg"
    "atomic_inc"
    "atom_inc"
    "atomic_dec"
    "atom_dec"
    "atomic_cmpxchg"
    "atom_cmpxchg"
    "atomic_min"
    "atom_min"
    "atomic_max"
    "atom_max"
    "atomic_and"
    "atom_and"
    "atomic_or"
    "atom_or"
    "atomic_xor"
    "atom_xor"

    "get_kernel_sub_group_count_for_ndrange"
    "get_kernel_max_sub_group_size_for_ndrange"

    "sub_group_non_uniform_broadcast"
    "sub_group_broadcast_first"
    "sub_group_ballot"
    "sub_group_inverse_ballot"
    "sub_group_ballot_bit_extract"
    "sub_group_ballot_bit_count"
    "sub_group_ballot_inclusive_scan"
    "sub_group_ballot_exclusive_scan"
    "sub_group_ballot_find_lsb"
    "sub_group_ballot_find_msb"
    "get_sub_group_eq_mask"
    "get_sub_group_ge_mask"
    "get_sub_group_gt_mask"
    "get_sub_group_le_mask"
    "get_sub_group_lt_mask"

    "sub_group_clustered_reduce_add"
    "sub_group_clustered_reduce_mul"
    "sub_group_clustered_reduce_min"
    "sub_group_clustered_reduce_max"

    "sub_group_clustered_reduce_and"
    "sub_group_clustered_reduce_or"
    "sub_group_clustered_reduce_xor"

    "sub_group_clustered_reduce_logical_and"
    "sub_group_clustered_reduce_logical_or"
    "sub_group_clustered_reduce_logical_xor"

    "sub_group_non_uniform_reduce_add"
    "sub_group_non_uniform_reduce_min"
    "sub_group_non_uniform_reduce_max"
    "sub_group_non_uniform_reduce_mul"
    "sub_group_non_uniform_scan_inclusive_add"
    "sub_group_non_uniform_scan_inclusive_min"
    "sub_group_non_uniform_scan_inclusive_max"
    "sub_group_non_uniform_scan_inclusive_mul"
    "sub_group_non_uniform_scan_exclusive_add"
    "sub_group_non_uniform_scan_exclusive_min"
    "sub_group_non_uniform_scan_exclusive_max"
    "sub_group_non_uniform_scan_exclusive_mul"
    "sub_group_non_uniform_reduce_and"
    "sub_group_non_uniform_reduce_or"
    "sub_group_non_uniform_reduce_xor"
    "sub_group_non_uniform_scan_inclusive_and"
    "sub_group_non_uniform_scan_inclusive_or"
    "sub_group_non_uniform_scan_inclusive_xor"
    "sub_group_non_uniform_scan_exclusive_and"
    "sub_group_non_uniform_scan_exclusive_or"
    "sub_group_non_uniform_scan_exclusive_xor"

    "sub_group_non_uniform_reduce_logical_and"
    "sub_group_non_uniform_reduce_logical_or"
    "sub_group_non_uniform_reduce_logical_xor"
    "sub_group_non_uniform_scan_inclusive_logical_and"
    "sub_group_non_uniform_scan_inclusive_logical_or"
    "sub_group_non_uniform_scan_inclusive_logical_xor"
    "sub_group_non_uniform_scan_exclusive_logical_and"
    "sub_group_non_uniform_scan_exclusive_logical_or"
    "sub_group_non_uniform_scan_exclusive_logical_xor"

    "sub_group_elect"
    "sub_group_non_uniform_all"
    "sub_group_non_uniform_any"
    "sub_group_non_uniform_all_equal"

    "sub_group_rotate"
    "sub_group_clustered_rotate"

    "sub_group_shuffle"
    "sub_group_shuffle_xor"

    "sub_group_shuffle_up"
    "sub_group_shuffle_down"

    "work_group_all"
    "work_group_any"
    "work_group_broadcast"

    "work_group_reduce_add"
    "work_group_scan_exclusive_add"
    "work_group_scan_inclusive_add"
    "work_group_reduce_min"
    "work_group_scan_exclusive_min"
    "work_group_scan_inclusive_min"
    "work_group_reduce_max"
    "work_group_scan_exclusive_max"
    "work_group_scan_inclusive_max"

    "work_group_reduce_logical_and"
    "work_group_reduce_logical_or"
    "work_group_reduce_logical_xor"
    "work_group_scan_inclusive_logical_and"
    "work_group_scan_inclusive_logical_or"
    "work_group_scan_inclusive_logical_xor"
    "work_group_scan_exclusive_logical_and"
    "work_group_scan_exclusive_logical_or"
    "work_group_scan_exclusive_logical_xor"

    "work_group_reduce_and"
    "work_group_reduce_or"
    "work_group_reduce_xor"
    "work_group_scan_inclusive_and"
    "work_group_scan_inclusive_or"
    "work_group_scan_inclusive_xor"

    "work_group_reduce_mul"
    "work_group_scan_inclusive_mul"
    "work_group_scan_exclusive_mul"

    "read_pipe"
    "write_pipe"
    "reserve_read_pipe"
    "commit_read_pipe"
    "commit_write_pipe"
    "is_valid_reserve_id"

    "work_group_reserve_read_pipe"
    "work_group_reserve_write_pipe"
    "work_group_commit_read_pipe"
    "work_group_commit_write_pipe"

    "get_pipe_num_packets"
    "get_pipe_max_packets"

    "enqueue_kernel"
    "get_kernel_work_group_size"
    "get_kernel_preferred_work_group_size_multiple"
    "get_kernel_preferred_work_group_size_multiple"
    "enqueue_marker"

    "retain_event"
    "release_event"
    "create_user_event"
    "is_valid_event"
    "set_user_event_status"
    "capture_event_profiling_info"

    "get_default_queue"
    "ndrange_1D"
    "ndrange_2D"
    "ndrange_3D"

    "sub_group_all"
    "sub_group_any"
    "sub_group_broadcast"
    "sub_group_reduce_add"
    "sub_group_scan_exclusive_add"
    "sub_group_scan_inclusive_add"
    "sub_group_reduce_min"
    "sub_group_scan_exclusive_min"
    "sub_group_scan_inclusive_min"
    "sub_group_reduce_max"
    "sub_group_scan_exclusive_max"
    "sub_group_scan_inclusive_max"

    "sub_group_reserve_read_pipe"
    "sub_group_reserve_write_pipe"
    "sub_group_commit_read_pipe"
    "sub_group_commit_write_pipe")
  "List of builtin OpenCL functions.")

(defvar opencl-c-floating-point-constants
  (mapcan
   (lambda (type)
     (mapcar (lambda (prefix) (concat prefix type)) '("HALF_" "FLT_" "DBL_")))
   '("DIG"
     "MANT_DIG"
     "MAX_10_EXP"
     "MAX_EXP"
     "MIN_10_EXP"
     "MIN_EXP"
     "MAX"
     "MIN"
     "EPSILON"))
  "List of all floating point constants in OpenCL C.")

(defvar opencl-c-math-constants
  (mapcan
   (lambda (constant)
     (mapcar (lambda (suffix) (concat constant suffix)) '("_H" "_F" "")))
   '("M_E"
     "M_LOG2E"
     "M_LOG10E"
     "M_LN2"
     "M_LN10"
     "M_PI"
     "M_PI_2"
     "M_PI_4"
     "M_1_PI"
     "M_2_PI"
     "M_2_SQRTPI"
     "M_SQRT2"
     "M_SQRT1_2"))
  "List of all mathematical constants in OpenCL C.")

(defconst opencl-c-constants
  `("MAXFLOAT"
    "HUGE_VALF"
    "INFINITY"
    "NAN"
    "HUGE_VAL"

    "FP_ILOGB0"
    "FP_ILOGBNAN"

    "CHAR_BIT"
    "CHAR_MAX"
    "CHAR_MIN"
    "INT_MAX"
    "INT_MIN"
    "LONG_MAX"
    "LONG_MIN"
    "SCHAR_MAX"
    "SCHAR_MIN"
    "SHRT_MAX"
    "SHRT_MIN"
    "UCHAR_MAX"
    "USHRT_MAX"
    "UINT_MAX"
    "ULONG_MAX"

    ,@opencl-c-math-constants
    ,@opencl-c-floating-point-constants

    "ATOMIC_FLAG_INIT"

    "memory_order_relaxed"
    "memory_order_acquire"
    "memory_order_release"
    "memory_order_acq_rel"
    "memory_order_seq_cst"

    "memory_scope_work_item"
    "memory_scope_sub_group"
    "memory_scope_work_group"
    "memory_scope_device"
    "memory_scope_all_svm_devices"
    "memory_scope_all_devices"

    "CLK_GLOBAL_MEM_FENCE"
    "CLK_LOCAL_MEM_FENCE"
    "CLK_IMAGE_MEM_FENCE"

    "CLK_NORMALIZED_COORDS_TRUE"
    "CLK_NORMALIZED_COORDS_FALSE"
    "CLK_ADDRESS_MIRRORED_REPEAT"
    "CLK_ADDRESS_REPEAT"
    "CLK_ADDRESS_CLAMP_TO_EDGE"
    "CLK_ADDRESS_CLAMP"
    "CLK_ADDRESS_NONE"

    "CLK_FILTER_NEAREST"
    "CLK_FILTER_LINEAR")
  "List of OpenCL constant.")

(eval-and-compile
  (defconst opencl-c-primitive-types
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

  (defconst opencl-c-atomic-types
    '("atomic_int"
      "atomic_uint"
      "atomic_long"
      "atomic_ulong"
      "atomic_float"
      "atomic_double"
      "atomic_intptr_t"
      "atomic_uintptr_t"
      "atomic_size_t"
      "atomic_ptrdiff_t"
      "atomic_flag")
    "List of all atomic types in OpenCL C.")

  (defconst opencl-c-vector-types
    (mapcan (lambda (type)
              (mapcar (lambda (n) (concat type (number-to-string n)))
                      '(2 3 4 8 16)))
            '("char" "uchar" "short" "ushort" "int" "uint"
              "long" "ulong" "float" "double"))
    "List of all vector types in OpenCL C.")

  (defconst opencl-c-descriptor-types
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
    "List of all other OpenCL types."))

(defvar opencl-c-constants-rx (regexp-opt opencl-c-constants 'symbols))
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

(defvar opencl-c-fp-contract-rx
  (rx (group-n 1 "#pragma")
      (+ (in space))
      (seq "OPENCL" (+ (in space)) "FP_CONTRACT" (+ (in space)))
      (or (group-n 2 (or "ON" "DEFAULT"))
          (group-n 3 "OFF")))
  "Regex for OpenCL floating point contract switch.")


(defvar opencl-c-font-lock-keywords
  `((,opencl-c-builtins-rx  . font-lock-builtin-face)
    (,opencl-c-constants-rx . font-lock-constant-face)
    (,opencl-c-fp-contract-rx (2 '(face font-lock-keyword-face) nil lax)
                      (3 '(face font-lock-warning-face) nil lax))
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
  opencl-c
  (append
   opencl-c-primitive-types
   opencl-c-atomic-types
   opencl-c-vector-types
   opencl-c-descriptor-types
   (c-lang-const c-primitive-type-kwds c)))

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
