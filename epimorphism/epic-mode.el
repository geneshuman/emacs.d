;;; epic-mode.el --- major mode for EPIMORPHISM shader files
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

;; If epic-mode is not part of your distribution, put this file into your
;; load-path and the following into your ~/.emacs:
;; (add-to-list 'load-path "~/.emacs.d/epimorphism/")
;; (autoload 'epic-mode "epic-mode" nil t)
;; (setq auto-mode-alist (cons '("\.epic$" . epic-mode) auto-mode-alist))

;;; Code:

(provide 'epic-mode)

(eval-when-compile      ; required and optional libraries
  (require 'cc-mode)
  (require 'find-file))

(require 'align)

(defgroup epic nil
  "Epimorphism Component Major Mode"
  :group 'languages)

(defconst epic-language-version "6.0"
  "EPIC language version number.")

(defconst gl-version "1.1"
  "EPIC major mode version number.")

(defvar epic-type-face 'epic-type-face)
(defface epic-type-face
  '((t (:inherit font-lock-type-face))) "epic: type face"
  :group 'epic)

(defvar epic-builtin-face 'epic-builtin-face)
(defface epic-builtin-face
  '((t (:inherit font-lock-builtin-face))) "epic: builtin face"
  :group 'epic)

(defvar epic-keyword-face 'epic-keyword-face)
(defface epic-keyword-face
  '((t (:inherit font-lock-keyword-face))) "epic: keyword face"
  :group 'epic)

(defvar epic-module-name-face 'epic-module-name-face)
(defface epic-module-name-face
  '((t (:inherit font-lock-variable-name-face :foreground "#389"))) "epim: module face"
  :group 'epim)

(defvar epic-par-name-face 'epic-par-name-face)
(defface epic-par-name-face
  '((t (:inherit font-lock-variable-name-face :foreground "#C42"))) "epim: par face"
  :group 'epim)

(defvar epic-preprocessor-face 'epic-preprocessor-face)
(defface epic-preprocessor-face
  '((t (:inherit font-lock-preprocessor-face))) "epic: preprocessor face"
  :group 'epic)

(defvar epic-mode-hook nil)

(defvar epic-mode-map
  (let ((epic-mode-map (make-sparse-keymap)))
    (define-key epic-mode-map [S-iso-lefttab] 'ff-find-other-file)
    epic-mode-map)
  "Keymap for EPIC major mode")

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.epic\\'" . epic-mode)))

(eval-and-compile
  ;;
  ;;    These vars are useful for completion so keep them around after
  ;;    compile as well. The goal here is to have the byte compiled code
  ;;    have optimized regexps so its not done at eval time.
  ;;

  (defvar epic-type-list
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
      "sampler3DRect"))

  (defvar epic-modifier-list
    '("attribute" "const" "uniform" "varying" "buffer" "shared" "coherent" "volatile" "restrict"
      "readonly" "writeonly" "atomic_uint" "layout" "centroid" "flat" "smooth"
      "noperspective" "patch" "sample" "break" "continue" "do" "for" "while"
      "switch" "case" "default" "if" "else" "subroutine" "in" "out" "inout"
      "invariant" "discard" "return" "lowp" "mediump" "highp" "precision"
      "struct" "common" "partition" "active" "asm" "class" "union" "enum"
      "typedef" "template" "this" "packed" "resource" "goto" "inline" "noinline"
      "public" "static" "extern" "external" "interface" "superp" "input" "output"
      "filter" "sizeof" "cast" "namespace" "using" "row_major"
      "early_fragment_tests"))

  (defvar epic-builtin-list
    '("abs" "acos" "acosh" "all" "any" "asin" "asinh" "atan" "atanh"
      "atomicCounter" "atomicCounterDecrement" "atomicCounterIncrement"
      "barrier" "bitCount" "bitfieldExtract" "bitfieldInsert" "bitfieldReverse"
      "ceil" "clamp" "cos" "cosh" "cross" "degrees" "determinant" "dFdx" "dFdy"
      "dFdyFine" "dFdxFine" "dFdyCoarse" "dFdxCourse"
      "fwidthFine" "fwidthCoarse"
      "distance" "dot" "EmitStreamVertex" "EmitVertex" "EndPrimitive"
      "EndStreamPrimitive" "equal" "exp" "exp2" "faceforward" "findLSB"
      "findMSB" "floatBitsToInt" "floatBitsToUint" "floor" "fma" "fract"
      "frexp" "fwidth" "greaterThan" "greaterThanEqual" "imageAtomicAdd"
      "imageAtomicAnd" "imageAtomicCompSwap" "imageAtomicExchange"
      "imageAtomicMax" "imageAtomicMin" "imageAtomicOr" "imageAtomicXor"
      "imageLoad" "imageSize" "imageStore" "imulExtended" "intBitsToFloat"
      "imageSamples"
      "interpolateAtCentroid" "interpolateAtOffset" "interpolateAtSample"
      "inverse" "inversesqrt" "isinf" "isnan" "ldexp" "length" "lessThan"
      "lessThanEqual" "log" "log2" "matrixCompMult" "max" "memoryBarrier" "min"
      "mix" "mod" "modf" "noise" "normalize" "not" "notEqual" "outerProduct"
      "packDouble2x32" "packHalf2x16" "packSnorm2x16" "packSnorm4x8"
      "packUnorm2x16" "packUnorm4x8" "pow" "radians" "reflect" "refract"
      "round" "roundEven" "sign" "sin" "sinh" "smoothstep" "sqrt" "step" "tan"
      "tanh" "texelFetch" "texelFetchOffset" "texture" "textureGather"
      "textureGatherOffset" "textureGatherOffsets" "textureGrad"
      "textureGradOffset" "textureLod" "textureLodOffset" "textureOffset"
      "textureProj" "textureProjGrad" "textureProjGradOffset" "textureProjLod"
      "textureProjLodOffset" "textureProjOffset" "textureQueryLevels" "textureQueryLod"
      "textureSize" "transpose" "trunc" "uaddCarry" "uintBitsToFloat"
      "umulExtended" "unpackDouble2x32" "unpackHalf2x16" "unpackSnorm2x16"
      "unpackSnorm4x8" "unpackUnorm2x16" "unpackUnorm4x8" "usubBorrow"
      "flags" "desc" "include"))

  (defvar epic-preprocessor-directive-list
    '("define" "undef" "if" "ifdef" "ifndef" "else" "elif" "endif"
      "error" "pragma" "extension" "version" "line"))

  (defvar epic-preprocessor-expr-list
    '("defined" "##"))

  (autoload 'w3m-browse-url "w3m" "View URL using w3m")
  ) ; eval-and-compile

(eval-when-compile
  (defun epic-ppre (re)
    (format "\\<\\(%s\\)\\>" (regexp-opt re))))

(defvar epic-font-lock-keywords-1
  (list
   (cons (eval-when-compile
           (format "^[ \t]*#[ \t]*\\<\\(%s\\)\\>"
                   (regexp-opt epic-preprocessor-directive-list)))
         epic-preprocessor-face)
   (cons (eval-when-compile
           (epic-ppre epic-type-list))
         epic-type-face)
   (cons (eval-when-compile
           (epic-ppre epic-modifier-list))
         epic-keyword-face)
   (cons (eval-when-compile
           (epic-ppre epic-builtin-list))
         epic-builtin-face)
   (cons "#[A-Z_0-9]+" epic-module-name-face)
   (cons "\\$[A-Za-z0-9_]+" epic-par-name-face)
   )
  "Minimal highlighting expressions for EPIC mode")


(defvar epic-font-lock-keywords epic-font-lock-keywords-1
  "Default highlighting expressions for EPIC mode")

(defvar epic-mode-syntax-table
  (let ((epic-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" epic-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" epic-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" epic-mode-syntax-table)
    (modify-syntax-entry ?_ "w" epic-mode-syntax-table)
    epic-mode-syntax-table)
  "Syntax table for epic-mode")


(defvar epimorphism-other-file-alist
  '(("\\.epic$" (".epic")))
  "Alist of extensions to find given the current file's extension")

(defun epic-man-completion-list ()
  (append epic-builtin-list epic-deprecated-builtin-list))

;;;###autoload
(define-derived-mode epic-mode c-mode "EpiComponent"
  "Major mode for editing Epimorphism Component files."
  (set (make-local-variable 'font-lock-defaults) '(epic-font-lock-keywords))
  (set (make-local-variable 'ff-other-file-alist) 'epic-other-file-alist)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-padding) "")
  (add-to-list 'align-c++-modes 'epic-mode)
  )

;;; epic-mode.el ends here
