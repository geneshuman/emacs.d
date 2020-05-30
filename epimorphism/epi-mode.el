;;; epi-mode.el --- major mode for EPIMORPHISM shader files
;;
;; Copyright (C) 4067 Yo Mama
;;
;; Authors: Gene Shuman
;; Keywords: languages
;; Version: 1.0
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Major mode for editing epimoprhism library files
;; Hack off of X-URL: http://artis.inrialpes.fr/~Xavier.Decoret/resources/glsl-mode/

;;; Installation:

;; This file requires Emacs-20.3 or higher and package cc-mode.

;; If epi-mode is not part of your distribution, put this file into your
;; load-path and the following into your ~/.emacs:
;; (add-to-list 'load-path "~/.emacs.d/epimorphism/")
;; (autoload 'epi-mode "epi-mode" nil t)
;; (setq auto-mode-alist (cons '("\.epi$" . epi-mode) auto-mode-alist))

;;; Code:

(provide 'epi-mode)

(eval-when-compile      ; required and optional libraries
  (require 'cc-mode)
  (require 'find-file))

(require 'align)

(defgroup epi nil
  "Epimorphism Object Major Mode"
  :group 'languages)

(defconst epi-language-version "6.0"
  "EPI language version number.")

(defconst gl-version "1.1"
  "EPI major mode version number.")

(defvar epi-type-face 'epi-type-face)
(defface epi-type-face
  '((t (:inherit font-lock-type-face))) "epi: type face"
  :group 'epi)

(defvar epi-builtin-face 'epi-builtin-face)
(defface epi-builtin-face
  '((t (:inherit font-lock-builtin-face))) "epi: builtin face"
  :group 'epi)

(defvar epi-keyword-face 'epi-keyword-face)
(defface epi-keyword-face
  '((t (:inherit font-lock-keyword-face))) "epi: keyword face"
  :group 'epi)


(defvar epi-numeric-face 'epi-numeric-face)
(defface epi-numeric-face
  '((t (:inherit font-lock-variable-name-face :foreground "#396"))) "epi: module face"
  :group 'epi)

(defvar epi-par-name-face 'epi-par-name-face)
(defface epi-par-name-face
  '((t (:inherit font-lock-variable-name-face :foreground "#E63"))) "epi: par face"
  :group 'epi)

(defvar epi-mode-hook nil)

(defvar epi-mode-map
  (let ((epi-mode-map (make-sparse-keymap)))
    (define-key epi-mode-map [S-iso-lefttab] 'ff-find-other-file)
    epi-mode-map)
  "Keymap for EPI major mode")

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.epi\\'" . epi-mode)))

(eval-and-compile
  ;;
  ;;    These vars are useful for completion so keep them around after
  ;;    compile as well. The goal here is to have the byte compiled code
  ;;    have optimized regexps so its not done at eval time.
  ;;

  (defvar epi-type-list
    '("float" "double" "int" "void" "bool" "true" "false" "mat2" "mat3"
      "mat4" "dmat2" "dmat3" "dmat4" "mat2x2" "mat2x3" "mat2x4" "dmat2x2"
      "dmat2x3" "dmat2x4" "mat3x2" "mat3x3" "mat3x4" "dmat3x2" "dmat3x3"
      "dmat3x4" "mat4x2" "mat4x3" "mat4x4" "dmat4x2" "dmat4x3" "dmat4x4" "vec2"
      "vec3" "vec4" "ivec2" "ivec3" "ivec4" "bvec2" "bvec3" "bvec4" "dvec2"
      "dvec3" "dvec4" "uint" "uvec2" "uvec3" "uvec4" "sampler1D" "sampler2D"
      "sampler3D" "samplerCube" "sampler1DShadow" "sampler2DShadow"
      "samplerCubeShadow" "sampler1DArray" "sampler2DArray"
      "sampler1DArrayShadow" "sampler2DArrayShadow" "isampler1D" "isampler2D"
      "isampler3D" "isamplerCube" "isampler1DArray" "isampler2DArray"
      "usampler1D" "usampler2D" "usampler3D" "usamplerCube" "usampler1DArray"
      "usampler2DArray" "sampler2DRect" "sampler2DRectShadow" "isampler2DRect"
      "usampler2DRect" "samplerBuffer" "isamplerBuffer" "usamplerBuffer"
      "sampler2DMS" "isampler2DMS" "usampler2DMS" "sampler2DMSArray"
      "isampler2DMSArray" "usampler2DMSArray" "samplerCubeArray"
      "samplerCubeArrayShadow" "isamplerCubeArray" "usamplerCubeArray"
      "image1D" "iimage1D" "uimage1D" "image2D" "iimage2D" "uimage2D" "image3D"
      "iimage3D" "uimage3D" "image2DRect" "iimage2DRect" "uimage2DRect"
      "imageCube" "iimageCube" "uimageCube" "imageBuffer" "iimageBuffer"
      "uimageBuffer" "image1DArray" "iimage1DArray" "uimage1DArray"
      "image2DArray" "iimage2DArray" "uimage2DArray" "imageCubeArray"
      "iimageCubeArray" "uimageCubeArray" "image2DMS" "iimage2DMS" "uimage2DMS"
      "image2DMSArray" "iimage2DMSArray" "uimage2DMSArray" "long" "short"
      "half" "fixed" "unsigned" "hvec2" "hvec3" "hvec4" "fvec2" "fvec3" "fvec4"
      "sampler3DRect" "FLOAT" "SRGB" "UNORM" "UNORM_SRGB" "LINEAR" "NONE" "TRILINEAR"
      "LINEAR_MIP_NEAREST" "NEAREST_MIP_NEAREST" "SPC" "MIRROR" "REPEAT" "WIDTH" "HEIGHT"))


  (autoload 'w3m-browse-url "w3m" "View URL using w3m")
  ) ; eval-and-compile


(eval-when-compile
  (defun epi-ppre (re)
    (format "\\<\\(%s\\)\\>" (regexp-opt re))))

(defvar epi-font-lock-keywords-1
  (list
   (cons (eval-when-compile
           (epi-ppre epi-type-list))
         epi-type-face)
   (cons "### [A-Za-z0-9_]+ [A-Za-z0-9_]+" epi-keyword-face)
   (cons "<< [A-Za-z0-9_]+" epi-builtin-face)
   (cons "\\$[A-Za-z0-9_]+" epi-par-name-face)
   (cons "[^A-Za-z\\*~]-?[0-9.]+[^A-Za-z\\*~]" epi-numeric-face)
   )
  "Minimal highlighting expressions for EPI mode")


(defvar epi-font-lock-keywords epi-font-lock-keywords-1
  "Default highlighting expressions for EPI mode")

(defvar epi-mode-syntax-table
  (let ((epi-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" epi-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" epi-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" epi-mode-syntax-table)
    (modify-syntax-entry ?_ "w" epi-mode-syntax-table)
    epi-mode-syntax-table)
  "Syntax table for epi-mode")


(defvar epimorphism-other-file-alist
  '(("\\.epi$" (".epi")))
  "Alist of extensions to find given the current file's extension")

(defun epi-man-completion-list ()
  (append epi-builtin-list epi-deprecated-builtin-list))

;;;###autoload
(define-derived-mode epi-mode c-mode "EpiObject"
  "Major mode for editing Epimorphism Object files."
  (set (make-local-variable 'font-lock-defaults) '(epi-font-lock-keywords))
  (set (make-local-variable 'ff-other-file-alist) 'epi-other-file-alist)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-padding) "")
  (add-to-list 'align-c++-modes 'epi-mode)
  )

;;; epi-mode.el ends here
