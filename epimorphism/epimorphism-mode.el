;;; epimorphism-mode.el --- major mode for Open EPIMORPHISM shader files
;;
;; Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
;; Copyright (C) 2011, 2014 Jim Hourihan
;;
;; Authors: Xavier.Decoret@imag.fr,
;;          Jim Hourihan <jimhourihan ~at~ gmail.com> (updated for 4.5, etc)
;;          Gene Shuman
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

;; If epimorphism-mode is not part of your distribution, put this file into your
;; load-path and the following into your ~/.emacs:
;;   (autoload 'epimorphism-mode "epimorphism-mode" nil t)
;;   (add-to-list 'auto-mode-alist '("\\.lib\\'" . epimorphism-mode))
;;   (add-to-list 'auto-mode-alist '("\\.slib\\'" . epimorphism-mode))

;;; Code:

(provide 'epimorphism-mode)

(eval-when-compile      ; required and optional libraries
  (require 'cc-mode)
  (require 'find-file))

(require 'align)

(defgroup epimorphism nil
  "Epimorphism Shading Language Major Mode"
  :group 'languages)

(defconst epimorphism-language-version "1.0"
  "EPIMORPHISM language version number.")

(defconst gl-version "1.0"
  "EPIMORPHISM major mode version number.")

(defvar epimorphism-type-face 'epimorphism-type-face)
(defface epimorphism-type-face
  '((t (:inherit font-lock-type-face))) "epimorphism: type face"
  :group 'epimorphism)

(defvar epimorphism-builtin-face 'epimorphism-builtin-face)
(defface epimorphism-builtin-face
  '((t (:inherit font-lock-builtin-face))) "epimorphism: builtin face"
  :group 'epimorphism)

(defvar epimorphism-deprecated-builtin-face 'epimorphism-deprecated-builtin-face)
(defface epimorphism-deprecated-builtin-face
  '((t (:inherit epimorphism-builtin-face))) "epimorphism: deprecated builtin face"
  :group 'epimorphism)

(defvar epimorphism-keyword-face 'epimorphism-keyword-face)
(defface epimorphism-keyword-face
  '((t (:inherit font-lock-keyword-face))) "epimorphism: keyword face"
  :group 'epimorphism)

(defvar epimorphism-deprecated-keyword-face 'epimorphism-deprecated-keyword-face)
(defface epimorphism-deprecated-keyword-face
  '((t (:inherit epimorphism-keyword-face))) "epimorphism: deprecated keyword face"
  :group 'epimorphism)

(defvar epimorphism-variable-name-face 'epimorphism-variable-name-face)
(defface epimorphism-variable-name-face
  '((t (:inherit font-lock-variable-name-face))) "epimorphism: variable face"
  :group 'epimorphism)

(defvar epimorphism-deprecated-variable-name-face 'epimorphism-deprecated-variable-name-face)
(defface epimorphism-deprecated-variable-name-face
  '((t (:inherit epimorphism-variable-name-face))) "epimorphism: deprecated variable face"
  :group 'epimorphism)

(defvar epimorphism-preprocessor-face 'epimorphism-preprocessor-face)
(defface epimorphism-preprocessor-face
  '((t (:inherit font-lock-preprocessor-face))) "epimorphism: preprocessor face"
  :group 'epimorphism)

(defvar epimorphism-mode-hook nil)

(defvar epimorphism-mode-map
  (let ((epimorphism-mode-map (make-sparse-keymap)))
    (define-key epimorphism-mode-map [S-iso-lefttab] 'ff-find-other-file)
    epimorphism-mode-map)
  "Keymap for EPIMORPHISM major mode")

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.lib\\'" . epimorphism-mode))
  (add-to-list 'auto-mode-alist '("\\.slib\\'" . epimorphism-mode)))

(eval-and-compile
  ;;
  ;;    These vars are useful for completion so keep them around after
  ;;    compile as well. The goal here is to have the byte compiled code
  ;;    have optimized regexps so its not done at eval time.
  ;;

  (defvar epimorphism-type-list
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

  (defvar epimorphism-modifier-list
    '("attribute" "const" "uniform" "varying" "buffer" "shared" "coherent" "volatile" "restrict"
      "readonly" "writeonly" "atomic_uint" "layout" "centroid" "flat" "smooth"
      "noperspective" "patch" "sample" "break" "continue" "do" "for" "while"
      "switch" "case" "default" "if" "else" "subroutine" "in" "out" "inout"
      "invariant" "discard" "return" "lowp" "mediump" "highp" "precision"
      "struct" "common" "partition" "active" "asm" "class" "union" "enum"
      "typedef" "template" "this" "packed" "resource" "goto" "inline" "noinline"
      "public" "static" "extern" "external" "interface" "superp" "input" "output"
      "filter" "sizeof" "cast" "namespace" "using" "row_major"
      "early_fragment_tests" "Module" "Component" "code" ))

  (defvar epimorphism-deprecated-modifier-list
    '("varying" "attribute")) ; centroid is deprecated when used with varying

  (defvar epimorphism-builtin-list
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
      "id" "family" "includes" "default_mod" "children" "component" "flags" "par"
      "modules" "sub" "images" "scripts" "zn"))

  (defvar epimorphism-deprecated-builtin-list
    '("texture1D" "texture1DProj" "texture1DLod" "texture1DProjLod"
      "texture2D" "texture2DProj" "texture2DLod" "texture2DProjLod"
      "texture2DRect" "texture2DRectProj"
      "texture3D" "texture3DProj" "texture3DLod" "texture3DProjLod"
      "shadow1D" "shadow1DProj" "shadow1DLod" "shadow1DProjLod"
      "shadow2D" "shadow2DProj" "shadow2DLod" "shadow2DProjLod"
      "textureCube" "textureCubeLod"))

  (defvar epimorphism-deprecated-variables-list
    '("gl_FragColor" "gl_FragData" "gl_MaxVarying" "gl_MaxVaryingFloats"
      "gl_MaxVaryingComponents"))

  (defvar epimorphism-preprocessor-directive-list
    '("define" "undef" "if" "ifdef" "ifndef" "else" "elif" "endif"
      "error" "pragma" "extension" "version" "line"))

  (defvar epimorphism-preprocessor-expr-list
    '("defined" "##"))

  (defvar epimorphism-preprocessor-builtin-list
    '("__LINE__" "__FILE__" "__VERSION__"))

  (autoload 'w3m-browse-url "w3m" "View URL using w3m")
  ) ; eval-and-compile

(eval-when-compile
  (defun epimorphism-ppre (re)
    (format "\\<\\(%s\\)\\>" (regexp-opt re))))

(defvar epimorphism-font-lock-keywords-1
  (list
   (cons (eval-when-compile
           (format "^[ \t]*#[ \t]*\\<\\(%s\\)\\>"
                   (regexp-opt epimorphism-preprocessor-directive-list)))
         epimorphism-preprocessor-face)
   (cons (eval-when-compile
           (epimorphism-ppre epimorphism-type-list))
         epimorphism-type-face)
   (cons (eval-when-compile
           (epimorphism-ppre epimorphism-deprecated-modifier-list))
         epimorphism-deprecated-keyword-face)
   (cons (eval-when-compile
           (epimorphism-ppre epimorphism-modifier-list))
         epimorphism-keyword-face)
   (cons (eval-when-compile
           (epimorphism-ppre epimorphism-preprocessor-builtin-list))
         epimorphism-keyword-face)
   (cons (eval-when-compile
           (epimorphism-ppre epimorphism-deprecated-builtin-list))
         epimorphism-deprecated-builtin-face)
   (cons (eval-when-compile
           (epimorphism-ppre epimorphism-builtin-list))
         epimorphism-builtin-face)
   (cons (eval-when-compile
           (epimorphism-ppre epimorphism-deprecated-variables-list))
         epimorphism-deprecated-variable-name-face)
   (cons "gl_[A-Z][A-Za-z_]+" epimorphism-variable-name-face)
   )
  "Minimal highlighting expressions for EPIMORPHISM mode")


(defvar epimorphism-font-lock-keywords epimorphism-font-lock-keywords-1
  "Default highlighting expressions for EPIMORPHISM mode")

(defvar epimorphism-mode-syntax-table
  (let ((epimorphism-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" epimorphism-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" epimorphism-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" epimorphism-mode-syntax-table)
    (modify-syntax-entry ?_ "w" epimorphism-mode-syntax-table)
    epimorphism-mode-syntax-table)
  "Syntax table for epimorphism-mode")

(defvar epimorphism-other-file-alist
  '(("\\.lib$" (".lib"))
    ("\\.slib$" (".slib"))
    )
  "Alist of extensions to find given the current file's extension")


(defun epimorphism-man-completion-list ()
  (append epimorphism-builtin-list epimorphism-deprecated-builtin-list))

;;;###autoload
(define-derived-mode epimorphism-mode c-mode "EPIMORPHISM"
  "Major mode for editing OpenEPIMORPHISM shader files."
  (set (make-local-variable 'font-lock-defaults) '(epimorphism-font-lock-keywords))
  (set (make-local-variable 'ff-other-file-alist) 'epimorphism-other-file-alist)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-padding) "")
  (add-to-list 'align-c++-modes 'epimorphism-mode)
  )

;;; epimorphism-mode.el ends here
