;;; epim-mode.el --- major mode for EPIMORPHISM shader files
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

;; If epim-mode is not part of your distribution, put this file into your
;; load-path and the following into your ~/.emacs:
;; (add-to-list 'load-path "~/.emacs.d/epimorphism/")
;; (autoload 'epim-mode "epim-mode" nil t)
;; (setq auto-mode-alist (cons '("\.epim$" . epim-mode) auto-mode-alist))

;;; Code:
(provide 'epim-mode)

(eval-when-compile      ; required and optional libraries
  (require 'cc-mode)
  (require 'find-file))

(require 'align)

(defgroup epim nil
  "Epimorphism Module Major Mode"
  :group 'languages)

(defconst epim-language-version "6.0"
  "EPIM language version number.")

(defconst gl-version "1.1"
  "EPIM major mode version number.")

(defvar epim-type-face 'epim-type-face)
(defface epim-type-face
  '((t (:inherit font-lock-type-face))) "epim: type face"
  :group 'epim)

(defvar epim-builtin-face 'epim-builtin-face)
(defface epim-builtin-face
  '((t (:inherit font-lock-builtin-face))) "epim: builtin face"
  :group 'epim)

(defvar epim-keyword-face 'epim-keyword-face)
(defface epim-keyword-face
  '((t (:inherit font-lock-keyword-face))) "epim: keyword face"
  :group 'epim)

(defvar epim-module-name-face 'epim-module-name-face)
(defface epim-module-name-face
  '((t (:inherit font-lock-variable-name-face :foreground "#389"))) "epim: module face"
  :group 'epim)

(defvar epim-par-name-face 'epim-par-name-face)
(defface epim-par-name-face
  '((t (:inherit font-lock-variable-name-face :foreground "#C42"))) "epim: par face"
  :group 'epim)

(defvar epim-preprocessor-face 'epim-preprocessor-face)
(defface epim-preprocessor-face
  '((t (:inherit font-lock-preprocessor-face))) "epim: preprocessor face"
  :group 'epim)

(defvar epim-mode-hook nil)

(defvar epim-mode-map
  (let ((epim-mode-map (make-sparse-keymap)))
    (define-key epim-mode-map [S-iso-lefttab] 'ff-find-other-file)
    epim-mode-map)
  "Keymap for EPIM major mode")

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.epim\\'" . epim-mode)))

;;(eval-and-compile
  ;;
  ;;    These vars are useful for completion so keep them around after
  ;;    compile as well. The goal here is to have the byte compiled code
  ;;    have optimized regexps so its not done at eval time.
  ;;

 ;; (autoload 'w3m-browse-url "w3m" "View URL using w3m")
 ;; ) ; eval-and-compile


(defvar epim-font-lock-keywords-1
  (list
   (cons "### [A-Za-z0-9_]+ [A-Za-z0-9_]+" epim-keyword-face)
   (cons "#[A-Z_0-9]+" epim-module-name-face)
   (cons "\\$[A-Za-z0-9_]+" epim-par-name-face)
   (cons "{[A-Za-z0-9_, ]+}" epim-builtin-face)
   )
  "Minimal highlighting expressions for EPIM mode")


(defvar epim-font-lock-keywords epim-font-lock-keywords-1
  "Default highlighting expressions for EPIM mode")

(defvar epim-mode-syntax-table
  (let ((epim-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" epim-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" epim-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" epim-mode-syntax-table)
    (modify-syntax-entry ?_ "w" epim-mode-syntax-table)
    epim-mode-syntax-table)
  "Syntax table for epim-mode")


(defvar epimorphism-other-file-alist
  '(("\\.epim$" (".epim")))
  "Alist of extensions to find given the current file's extension")

(defun epim-man-completion-list ()
  (append epim-builtin-list epim-deprecated-builtin-list))


(defun get-paren-diff ()
  (interactive)
  (let (open_cnt close_cnt p1 p2)
    (save-excursion
      (setq p1 (line-beginning-position))
      (setq p2 (line-end-position))
      (setq open_cnt 0)
      (setq close_cnt 0)
      (goto-char p1)
      (while (and (< (point) p2) (re-search-forward "(" p2 t))
        (setq open_cnt (1+ open_cnt)))
      (goto-char p1)
      (while (and (< (point) p2) (re-search-forward ")" p2 t))
        (setq close_cnt (1+ close_cnt)))
      (- open_cnt close_cnt))))

(defun epim-indent-line ()
  "Indent current line as Epimorphism Module code."
  (interactive)
  (let (cur)
    (setq cur (point))
    (beginning-of-line)
    (if (looking-at "^###")
        (indent-line-to 0)
      (let (to-indent prev-diff cur-diff)
        (setq cur-diff (get-paren-diff))
        (save-excursion
          (forward-line -1)
          (setq prev-diff (get-paren-diff))
          (setq to-indent (current-indentation)))
        (if (< 0 prev-diff)
            (setq to-indent (+ to-indent (* 2 prev-diff)))
          ())
        (if (> 0 cur-diff)
            (setq to-indent (+ to-indent (* 2 cur-diff)))
          ())
        (indent-line-to to-indent)))
    (goto-char cur)
  ))

(defun epim-indent-region (st end &optional col)
  "Indent region as Epimorphism Module code."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (goto-char st)
    (while (and (< (point) end)
                (progn
                  (epim-indent-line)
                  (/= (forward-line 1) 1))))))


;;;###autoload
(define-derived-mode epim-mode c-mode "EpiModule"
  "Major mode for editing Epimorphism Module files."
  (set (make-local-variable 'font-lock-defaults) '(epim-font-lock-keywords))
  (set (make-local-variable 'ff-other-file-alist) 'epim-other-file-alist)
  (set (make-local-variable 'indent-line-function) 'epim-indent-line)
  (set (make-local-variable 'indent-region-function) 'epim-indent-region)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-padding) "")
  (local-set-key (kbd "TAB") 'indent-for-tab-command)
  (add-to-list 'align-c++-modes 'epim-mode)
  )

;;; epim-mode.el ends here
