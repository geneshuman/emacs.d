(ensure-package-installed 'auto-package-update 'ivy 'avy  'flx 'ivy-rich 'ivy-hydra 'ivy-posframe 'ivy-xref 'counsel 'counsel-projectile 'company 'company-c-headers)

(require 'flx)

;; ivy/counsel/swiper
(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t) ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 15) ;; number of result lines to display
  (setq ivy-count-format "") ;; does not count candidates
  (setq ivy-initial-inputs-alist nil)   ;; no regexp by default
  (setq ivy-use-selectable-prompt t) ;; selectable prompt
  (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (counsel-grep-or-swiper . ivy--regex-plus)
          (counsel-git-grep . ivy--regex-plus)
          (lsp-ivy-workspace-symbol . ivy--regex-plus)
          (t      . ivy--regex-fuzzy))))

(require 'ivy-rich)
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

(defun counsel-helpful-keymap-describe ()
  "select keymap with ivy, display help with helpful"
  (interactive)
  (ivy-read "describe keymap: " (let (cands)
                  (mapatoms
                   (lambda (x)
                     (and (boundp x) (keymapp (symbol-value x))
                      (push (symbol-name x) cands))))
                  cands)
        :require-match t
        :history 'counsel-describe-keymap-history
        :sort t
        :preselect (ivy-thing-at-point)
        :keymap counsel-describe-map
        :caller 'counsel-helpful-keymap-describe
        :action (lambda (map-name)
                  (helpful-variable (intern map-name))) ))

(setq counsel-find-file-ignore-regexp "\\.png\\'")

(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)

(global-set-key (kbd "C-h M") #'counsel-helpful-keymap-describe)

(setq ivy-rich-display-transformers-list (plist-put
                      ivy-rich-display-transformers-list 'counsel-helpful-keymap-describe
                      '(:columns ((counsel-describe-variable-transformer (:width 40))
                              (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))))
(ivy-rich-set-display-transformer nil)

(defun posframe-poshandler-bottom-center (info)
  "Posframe's position handler.

Get a position which let posframe stay onto its
parent-frame's center.  The structure of INFO can
be found in docstring of `posframe-show'."
  (cons (/ (- (plist-get info :parent-frame-width)
              (plist-get info :posframe-width))
           2)
        (* (/ (- (plist-get info :parent-frame-height)
              (plist-get info :posframe-height))
           12) 11)))

(defun ivy-posframe-display-at-bottom-center (str)
  (ivy-posframe--display str #'posframe-poshandler-bottom-center))


(require 'ivy-posframe)
;; display at `ivy-posframe-style'
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-bottom-center)))
(ivy-posframe-mode 1)


(setq ivy-posframe-parameters
      '((left-fringe . 3)
        (right-fringe . 3)
        (alpha . (95 . 95))))

(defun cleanup-posframe (str)
  "Cleanup posframe if forcus changed"
  (interactive)
  (let ((cur (current-buffer)))
    (unless (string-match-p (regexp-quote "*Minibuf") cur)
      (message "hi %s" cur) ())))

(setq window-selection-change-functions '(cleanup-posframe))


(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-c b") 'counsel-bookmark)

(global-set-key (kbd "\C-s") 'swiper)
(global-set-key (kbd "\C-r") 'swiper)
(global-set-key (kbd "C-S-s") 'isearch-forward)
(global-set-key (kbd "C-S-r") 'isearch-backward)

(use-package ivy-xref
  :ensure t
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; avy
(global-set-key (kbd "C-M-s") 'avy-goto-char-timer)
(global-set-key (kbd "C-M-r") 'avy-goto-char-timer)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(setq avy-timeout-seconds 0.3)

;; counsel
(counsel-projectile-mode)
(define-key ctl-x-map (kbd "C-f") 'counsel-projectile-find-file)
(define-key ctl-x-map (kbd "C-S-f") 'counsel-find-file)

(define-key projectile-mode-map [remap projectile-grep] nil)
(global-set-key (kbd "C-c p s G") 'counsel-projectile-grep)

;; company
(use-package company
  :init
  (setq company-backends '((company-files company-keywords company-capf company-dabbrev-code company-etags company-dabbrev)))
  :custom
  (company-tooltip-align-annotations 't)
  :config
  (global-company-mode 1))

;; (use-package company-box
;;   :after company
;;   :diminish
;;   :hook (company-mode . company-box-mode))

(require 'company-tng)
(company-tng-configure-default)
(add-to-list 'company-frontends 'company-tng-frontend)
(add-to-list 'company-backends 'company-c-headers)

(setq company-minimum-prefix-length 2
      company-idle-delay 0.0)

(define-key company-active-map (kbd "C-n") nil)
(define-key company-active-map (kbd "C-p") nil)
(define-key company-active-map (kbd "M-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "M-p") 'company-select-previous-or-abort)
