;; remap mac modifier keys
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)

;; PACKAGE CONFIGURATION
(require 'package)
(setq package-user-dir "~/.emacs.d/packages/")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("emacs-pe" . "http://emacs-pe.github.io/packages/"))
(package-initialize)

(defun ensure-package-installed (&rest packages)
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
           (package-install package)
         package)) ;;)
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;;(setq lsp-use-plists t)

(ensure-package-installed 'exec-path-from-shell 'flycheck 'coffee-mode 'expand-region 'haskell-mode 'projectile 'async 'magit 'powerline 'intero 'rvm 'psc-ide 'use-package 'spaceline 'purescript-mode 'glsl-mode 'auto-package-update 'ivy 'counsel 'counsel-projectile 'flx 'ivy-rich 'whole-line-or-region 'undo-tree 'avy 'dired-filetype-face 'diredfl 'ivy-hydra 'pdf-tools 'lsp-mode 'lsp-ui 'lsp-ivy 'ivy-xref 'company 'company-c-headers 'dap-mode 'modern-cpp-font-lock 'which-key 'treemacs 'lsp-treemacs 'company-box 'cmake-mode 'ccls 'ivy-posframe 'helpful 'rainbow-delimiters 'git-auto-commit-mode 'vterm 'noccur)

(auto-package-update-maybe)

;; requires
(require 'expand-region)
(require 'uniquify)
(require 'powerline)
(require 'sql)
(require 'psc-ide)
(require 'flx)
(require 'whole-line-or-region)
(require 'dired-x)

;; misc
(setq warning-minimum-level :error)

(setq default-directory "~" )
(global-subword-mode 1) ;; split by camel case
;;(define-key key-translation-map [(control ?\;)]  [127]) ;; what is this?
(global-key-binding (kbd "C-[") nil)
(put 'upcase-region 'disabled nil)
(setq auto-window-vscroll nil) ;; speed up next-line

(setq gc-cons-threshold 100000000) ;; less frequent gc
(setq read-process-output-max (* 1024 1024))

;; key bindings
(global-set-key (kbd "M-+") (lambda () (interactive) (load "~/.emacs.d/init.el")))
(global-set-key (kbd "C-M-+") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-M-S-w") 'kill-ring-save)
(global-set-key (kbd "C-x 1") 'nil)
(global-set-key (kbd "M-?") 'nil) ;; was crashing ido?
(global-set-key (kbd "M-.") 'nil)
(global-set-key (kbd "C-M-f") 'flycheck-next-error)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "C-=") 'er/expand-region )
(global-set-key (kbd "C-M-<backspace>") 'kill-sexp)
(global-set-key (kbd "C-x C-b") nil)
(global-set-key (kbd "C-x B") 'list-buffers)
(global-set-key (kbd "C-z") 'nil)
(global-set-key (kbd "C-M-z") 'suspend-frame)

;; gene mode
(define-prefix-command 'gene-mode-map)
(global-set-key (kbd "C-M-g") 'gene-mode-map)

;; get rid of C-[ == ESC
(define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
(global-set-key (kbd "<C-[>") nil)

;; disable ESC-ESC-ESC fucking with window layout
(require 'cl-lib)
(defun my-keyboard-escape-quit (fun &rest args)
  (cl-letf (((symbol-function 'one-window-p) (lambda (&rest _) t)))
    (apply fun args)))
(advice-add 'keyboard-escape-quit :around #'my-keyboard-escape-quit)

;; fundamental mode for scratch buffer
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

;; automatically sync with disk changes
(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )

(global-set-key (kbd "M-R") 'revert-all-buffers)
(global-auto-revert-mode t)

;; automatically clean up bad whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;; if indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks
         (lambda () (and (untabify (point-min) (point-max)) nil)))

;;(add-hook 'write-file-hooks
;;         (lambda () (untabify (point-min) (point-max))))

;; remote sudo function
(defun sudo ()
  "Use TRAMP to `sudo' the current buffer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

;; Temporary files
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; add to path
(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;; add osx path & sync with rvm
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(rvm-activate-corresponding-ruby)

;; shell
(defvar shell-window)
(makunbound 'shell-window)
(defun shelly-times ()
  "Sets the ignore-other-window property or switches to window"
  (interactive)
  (if (boundp 'shell-window)
      (select-window shell-window)
    (progn
      ;;(purpose-toggle-window-buffer-dedicated)
      (if (equal major-mode 'vterm-mode)
          ()
        (vterm))
      (setq shell-window (selected-window))
      (set-window-parameter (selected-window) 'no-other-window 't)
      (message "shelly times"))))

(global-set-key (kbd "C-M-=") 'shelly-times)

(define-key comint-mode-map (kbd "C-M-l") nil)


(require 'vterm)
(setq vterm-max-scrollback 50000)

(define-key vterm-mode-map (kbd "C-M-g") 'gene-mode-map)
(define-key vterm-mode-map (kbd "C-M-j") 'windmove-left)
(define-key vterm-mode-map (kbd "C-M-l") 'windmove-right)
(define-key vterm-mode-map (kbd "C-M-i") 'windmove-up)
(define-key vterm-mode-map (kbd "C-M-k") 'windmove-down)

;;(require 'xterm-color)

(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(add-hook 'shell-mode-hook
          (lambda ()
            ;; Disable font-locking in this buffer to improve performance
            (font-lock-mode -1)
            ;; Prevent font-locking from being re-enabled in this bufferk
            (make-local-variable 'font-lock-function)
            (setq font-lock-function (lambda (_) nil))
            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))


(require 'eshell) ; or use with-eval-after-load

(add-hook 'eshell-before-prompt-hook
          (lambda ()
            (setq xterm-color-preserve-properties t)))

(setenv "TERM" "ansi")

;; tab width bs
(setq-default c-basic-offset 2
              tab-width 2
              indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2)

;; specific package configuration

;; rainbow
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; purpose mode
;; (purpose-mode)
;; (add-to-list 'purpose-user-mode-purposes '(shell-mode . terminal))
;; (purpose-compile-user-configuration)

;; (require 'window-purpose-x)
;; (purpose-x-magit-single-on)

;; magit
(require 'magit)
(global-set-key (kbd "M-B") 'magit-blame)

(defun magit-stage-all-and-commit(message)
  "Commits everything & prompts for a message."
  (interactive (list
                (read-string (format "Commit Message: ")
                             nil nil "")))

  (magit-stage-modified)
  (magit-commit-create (list "-m" message)))

(define-key gene-mode-map (kbd "c") 'magit-stage-all-and-commit)

;; zap up to char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.
   \(fn arg char)"
  'interactive)
(global-set-key [remap zap-to-char] 'zap-up-to-char)

;; helpful
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

(add-to-list 'display-buffer-alist
             '("*Help*" display-buffer-same-window))

(add-to-list 'display-buffer-alist
             '("Magit" display-buffer-same-window))

(add-to-list 'display-buffer-alist
             '("vterm" display-buffer-same-window))

;; dired
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-dwim-target t)
(require 'dired-filetype-face)
(setq dired-listing-switches "-alh")
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "C-<up>")
    (lambda () (interactive) (find-alternate-file "..")))
  ; was dired-up-directory
 ))

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


;; projectile setup
(projectile-global-mode) ;; to enable in all buffers
(counsel-projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;(setq projectile-enable-caching t)
(setq projectile-use-git-grep 1)
(setq projectile-enable-caching 1)

(define-key ctl-x-map (kbd "C-f") 'counsel-projectile-find-file)
(define-key ctl-x-map (kbd "C-S-f") 'counsel-find-file)

(define-key projectile-mode-map [remap projectile-grep] nil)
(global-set-key (kbd "C-c p s G") 'counsel-projectile-grep)
(setq projectile-file-exists-remote-cache-expire (* 10 60))

;; org mode
;; The following lines are always needed.  Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
(setq org-startup-indented t)
(setq org-log-done t)

;; undo tree
(global-undo-tree-mode)
(setq undo-tree-visualizer-diff t   )
(global-set-key (kbd "C-M-/") 'undo-tree-redo)

;; Prevent undo tree files from polluting your git repo
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; Language modes

;; Epimorphism
(add-to-list 'load-path "~/.emacs.d/epimorphism/")
(autoload 'epic-mode "epic-mode" nil t)
(setq auto-mode-alist (cons '("\.epic$" . epic-mode) auto-mode-alist))
(autoload 'epim-mode "epim-mode" nil t)
(setq auto-mode-alist (cons '("\.epim$" . epim-mode) auto-mode-alist))
(autoload 'epi-mode "epi-mode" nil t)
(setq auto-mode-alist (cons '("\.epi$" . epi-mode) auto-mode-alist))

;; flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)

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

;;(global-set-key (kbd "M-/") 'company-select-next) ;; defined in company-tng

;; haskell
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'intero-mode)

;; glsl
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;;(setq psc-ide-use-purs nil)
(add-hook 'purescript-mode-hook
  (lambda ()
    (psc-ide-mode)
    (company-mode)
    (flycheck-mode)
    (turn-on-purescript-indentation)))

;; octave
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; python
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent 4)))

;; c & c++
(setq c-default-style "linux")
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++17")))
;;(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'c-mode-hook
      '(lambda()
        (setq c-basic-offset 2)
        (setq indent-tabs-mode t)))

(setq lsp-keymap-prefix "C-'")


(use-package lsp-mode :commands lsp :ensure t)
(use-package lsp-mode
    :hook (((c-mode c++-mode objc-mode cuda-mode) . lsp)
           (lsp-mode . lsp-enable-which-key-integration))
    :custom
    ;;(lsp-auto-guess-root t)
    (lsp-prefer-capf t)
    :commands lsp)

(setq lsp-log-io nil)

(setq lsp-enable-on-type-formatting nil)
(setq lsp-enable-indentation nil)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-before-save-edits nil)

(setq lsp-idle-delay 0.1)

(add-hook 'lsp-ui-mode-hook
          (lambda()
            (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
            (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
            (lsp-register-client
             (make-lsp-client :new-connection (lsp-tramp-connection "ccls")
                              :major-modes '(c++-mode)
                              :remote? t
                              :server-id 'ccls-remote))
            ))

(with-eval-after-load 'lsp-mode
  ;; :project/:workspace/:file
  (setq lsp-diagnostics-modeline-scope :project)
  (add-hook 'lsp-managed-mode-hook 'lsp-diagnostics-modeline-mode))

;;(lsp-treemacs-sync-mode 1)

(use-package lsp-ui :commands lsp-ui-mode :ensure t)
(setq lsp-ui-sideline-delay 0.0)
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-ui-doc-position 'bottom)
(setq lsp-ui-doc-alignment 'window)
;;(setq lsp-ui-doc-use-webkit t)
(setq lsp-ui-doc-enable nil)

(global-set-key (kbd "C-M-d") 'lsp-ui-doc-glance)


(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (setq ccls-initialization-options '(:compilationDatabaseDirectory "build")))

  ;;(setq ccls-initialization-options '(:cache (:directory ".ccls-cache2"))))
  ;;(setq ccls-initialization-options '(:index (:initialBlacklist ["extern"]))))

(require 'lsp-mode)
(require 'lsp-ui)
(require 'lsp-ui-doc)
(add-hook 'c++-mode-hook 'lsp)

;;(dap-auto-configure-mode 1)
;;(require 'dap-lldb)

(use-package which-key
  :config
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  ;;(setq which-key-idle-delay 10000)
  ;;(setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-?") 'xref-find-references)

(define-key lsp-mode-map [remap xref-find-apropos] #'lsp-ivy-workspace-symbol)

(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

;; css
(add-hook 'css-mode-hook
      (lambda ()
        (setq css-indent-offset 2)
        (setq indent-tabs-mode nil)))

(add-hook 'scss-mode-hook
      (lambda ()
        (setq css-indent-offset 2)
        (setq indent-tabs-mode nil)))

;; coffescript
(add-hook 'coffee-mode-hook 'coffee-custom)
(setq auto-mode-alist (cons '("\\.coffee.erb$" . coffee-mode) auto-mode-alist))
(defun coffee-custom ()
  "coffee-mode-hook"
  (make-local-variable 'tab-width)
  (set 'tab-width 2)
  )

;; sql mode
(autoload 'sql-mode "sql-mode" "SQL Editing Mode" t)
      (setq auto-mode-alist
         (append '(("\\.sql$" . sql-mode)
                   ("\\.q$" . sql-mode))
                 auto-mode-alist))

;; ruby - no coding: utf-8 lines
(setq ruby-insert-encoding-magic-comment nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-type 'stack-ghci)
 '(package-selected-packages
   '(noccur git-auto-commit-mode vterm window-purpose rainbow-delimiters xterm-color helpful ivy-posframe counsel-projectile counsel modern-c++-font-lock dap-lldb company-c-headers company-mode company-capf modern-cpp-font-lock lsp-ivy which-key lsp-company lsp-ui ivy-xref lsp-mode diredfl dired-filetype-face avy ivy-hydra whole-line-or-region ivy-rich pdf-tools undo-tree auto-package-update cmake-mode projectile psc-ide spaceline use-package intero intero-mode powerlinem rvm exec-path-from-shell yaml-mode rubocop purescript-mode powerline markdown-mode magit helm-projectile grizzl glsl-mode flx-ido expand-region coffee-mode))
 '(safe-local-variable-values
   '((gac-automatically-push-p quote t)
     (gac-automatically-add-new-files-p quote t)))
 '(undo-tree-auto-save-history nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-selection ((t (:background "dark turquoise" :foreground "black"))))
 '(lsp-headerline-breadcrumb-path-face ((t (:inherit font-lock-string-face :foreground "dark blue")))))

;; windmove setup
(windmove-default-keybindings)
(define-key c-mode-base-map "\C-\M-j" nil)
;;(global-set-key (kbd "C-M-o") 'windmove-left)
(global-set-key (kbd "C-M-<left>") 'windmove-left)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-<down>") 'windmove-down)
(global-set-key (kbd "C-M-j") 'windmove-left)
(global-set-key (kbd "C-M-l") 'windmove-right)
(global-set-key (kbd "C-M-i") 'windmove-up)
(global-set-key (kbd "C-M-k") 'windmove-down)

;; UI CONFIGURATION
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'prassee t)
;;(set-frame-parameter (selected-frame) 'alpha '(87 . 50))
(add-to-list 'default-frame-alist '(alpha . (95 . 80)))
(add-to-list 'default-frame-alist '(width . 180))
(add-to-list 'default-frame-alist '(height . 60))

(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

(set-background-color "Black")
(set-foreground-color "White")

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;;(set-default-font "8x13")
(set-cursor-color "Grey")
(setq inhibit-startup-message t)
(column-number-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq next-line-add-newlines nil)
(setq scroll-step 10)
(setq frame-title-format "emacs - %b")
;;(load-theme 'afternoon t)

(fringe-mode 6)

(powerline-default-theme)
(use-package spaceline
:init
(progn
    (require 'spaceline-config)
    (setq powerline-default-separator 'slant)
    (setq spaceline-workspace-numbers-unicode t)
    (setq spaceline-separator-dir-left '(left . left))
    (setq spaceline-separator-dir-right '(right . right))
    (setq powerline-height 17)

    (spaceline-toggle-window-number-off)
    ;;(spaceline-toggle-projectile-root-off)
    (spaceline-toggle-battery-off)
    (spaceline-toggle-buffer-encoding-abbrev-off)
    (spaceline-toggle-buffer-size-off)
    (spaceline-toggle-minor-modes-off)

    (spaceline-toggle-buffer-modified-on)
    (spaceline-toggle-buffer-id-on)
    (spaceline-toggle-major-mode-on)
    (spaceline-toggle-hud-on) ;; ?
    (spaceline-emacs-theme)))


;; misc uitls

(global-set-key (kbd "C-M-T") 'open-linaro-todo)

(defun open-linaro-todo ()
  "open TODO on linaro"
  (interactive)
  (find-file "/ssh:linaro:/home/linaro/Programming/epimorphism6/TODO.txt")
  )




;; misc function
(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
        (message "No non-ascii characters."))))

;; search c++ docs
(defun cplusplus-search (term)
  "Search cplusplus for given TERM."
  (interactive (list
                (read-string (format "Search Term (%s): " (thing-at-point 'symbol))
                             nil nil (thing-at-point 'symbol))))
  (eww (concat "www.cplusplus.com " term)))

(global-set-key (kbd "C-M-S-d") 'cplusplus-search)

;; do shit to other buffers
(defvar other-prefix-ret)

(defun other-pre-hook ()
  "Hook to move to other window before executing command."
  ;;(message "OTHER PRE")
  (setq other-prefix-ret (selected-window))
  (other-window 1)
  (remove-hook 'pre-command-hook 'other-pre-hook))

(defun other-pre-hook-w-buffer ()
  "Hook to move to other window before executing command."
  ;;(message "OTHER PRE w BUFFER")
  (setq other-prefix-ret (selected-window))
  (let ((cur (current-buffer)))
    (other-window 1)
    (set-window-buffer (selected-window) cur)
    (other-window 0)
    (remove-hook 'pre-command-hook 'other-pre-hook-w-buffer)))

(defun other-post-hook ()
  "Hook to move to other window after executing command."
  ;;(message "OTHER POST")
  (unless (minibufferp (current-buffer))
    (if (and (boundp 'other-prefix-ret) other-prefix-ret)
        (progn
          ;;(message "OTHER POST DO")
          (select-window other-prefix-ret)
          (setq other-prefix-ret nil)
          (remove-hook 'post-command-hook 'other-post-hook)) () )))

(defun do-in-other-window ()
  (interactive)
  "Executes next command in other window."
  (setq other-prefix-ret nil)
  (add-hook 'pre-command-hook 'other-pre-hook)
  (add-hook 'post-command-hook 'other-post-hook))

(defun do-to-this-and-stay-in-other-window ()
  (interactive)
  "Functions as a prefix to execute next command in other window."
  (setq other-prefix-ret nil)
  (add-hook 'pre-command-hook 'other-pre-hook-w-buffer))

(defun do-to-this-in-other-window ()
  (interactive)
  "Functions as a prefix to execute next command in other window."
  (setq other-prefix-ret nil)
  (add-hook 'pre-command-hook 'other-pre-hook-w-buffer)
  (add-hook 'post-command-hook 'other-post-hook))

(global-set-key (kbd "C-;") 'do-in-other-window)
(global-set-key (kbd "C-:") 'do-to-this-and-stay-in-other-window)
(global-set-key (kbd "C-M-:") 'do-to-this-in-other-window)

(defun transpose-other-buffer ()
  "Transpose the buffers shown in two windows."
  (interactive)
  (let* ((other-win (or (windmove-find-other-window 'right)
                        (windmove-find-other-window 'left)))
         (this-win (selected-window))
         (this-buf (window-buffer))
         (next-buf (window-buffer other-win)))
    (set-window-buffer other-win this-buf)
    (set-window-buffer this-win next-buf)
    (select-window this-win)
    (recenter)))

(global-set-key (kbd "C-M-b") 'transpose-other-buffer)

(defun epi-exit ()
  "Switch to shell & C-c"
  (interactive)
  (let ((epi-exec-ret (selected-window)))
    (shelly-times)
    (vterm-send-C-c)
    (select-window epi-exec-ret)))

(defvar epi-cmd)
(defvar epi-args)

(defun epi-build-and-run-inner (cmd)
  "Build epimorphism & run it."

  (let ((epi-exec-ret (selected-window)))
    (setq epi-cmd cmd)
    (shelly-times)
    (epi-exit)
    (if (equal major-mode 'vterm-mode)
        (progn
          (vterm-send-string cmd)
          (vterm-send-return))
      (progn
        (goto-char (point-max))
        (insert cmd)
        (comint-send-input))
      )
    (select-window epi-exec-ret)
    ))

(defun epi-build-and-run-osx (args)
  "Build epimorphism & run it."
  (interactive (list
                (read-string (format "Args: (%s): " (if (boundp 'epi-args) epi-args "mac"))
                             nil nil (if (boundp 'epi-args) epi-args "mac"))))
  (let ((cmd (concat " cd /Users/gene/Programming/epimorphism6 && make -j8 -C build && sudo nice -n -10 ./epimorphism " args)))
  (epi-build-and-run-inner cmd)))

(defun epi-build-and-run-fb (args)
  "Build epimorphism & run it."
  (interactive (list
                (read-string (format "Args: (%s): " (if (boundp 'epi-args) epi-args "fb"))
                             nil nil (if (boundp 'epi-args) epi-args "fb"))))
  (let ((cmd (concat " cd /home/linaro/Programming/epimorphism6 && make -j2 -C build && sudo nice -n -10 ./epimorphism " args)))
  (epi-build-and-run-inner cmd)))

(defun epi-build-and-run-linunx (args)
  "Build epimorphism & run it."
  (interactive (list
                (read-string (format "Args: (%s): " (if (boundp 'epi-args) epi-args "linux"))
                             nil nil (if (boundp 'epi-args) epi-args "linux"))))
  (let ((cmd (concat " cd /Users/gene/Programming/epimorphism6 && make -j16 -C build && sudo nice -n -10 ./epimorphism " args)))
  (epi-build-and-run-inner cmd)))

(defun epi-build-and-run-cross (args)
  "Build epimorphism & run it."
  (interactive (list
                (read-string (format "Args: (%s): " (if (boundp 'epi-args) epi-args "fb"))
                             nil nil (if (boundp 'epi-args) epi-args "fb"))))
  (let ((cmd (concat " ssh -t gene@192.168.0.12 'cd /home/gene/Programming/epimorphism6 && make -j12 -C build && cp -r lib/epi /home/linaro/root/home/linaro/Programming/epimorphism6/lib' && cd /home/linaro/Programming/epimorphism6 && nice -n -10 ./epimorphism " args)))
  ;;(let ((cmd (concat "cd /home/gene/Programming/epimorphism6 && make -j12 -C build && cp -r lib/epi /home/linaro/root/home/linaro/Programming/epimorphism6/lib && ssh -t linaro 'cd /home/linaro/Programming/epimorphism6 && sudo nice -n -10 ./epimorphism " args "'")))
  (epi-build-and-run-inner cmd)))

(defun epi-build-and-run-cross-deploy (args)
  "Build epimorphism & run it."
  (interactive (list
                (read-string (format "Args: (%s): " (if (boundp 'epi-args) epi-args "fb"))
                             nil nil (if (boundp 'epi-args) epi-args "fb"))))
  (let ((cmd (concat " ssh -t gene@192.168.0.15 'cd /home/gene/Programming/epimorphism6 && make -j12 -C build && cp -r lib/epi /home/entropyandsons_remote/root/home/entropyandsons/epimorphism/lib' && cd /home/entropyandsons/epimorphism && nice -n -10 ./epimorphism " args)))
  ;;(let ((cmd (concat "cd /home/gene/Programming/epimorphism6 && make -j12 -C build && cp -r lib/epi /home/linaro/root/home/linaro/Programming/epimorphism6/lib && ssh -t linaro 'cd /home/linaro/Programming/epimorphism6 && sudo nice -n -10 ./epimorphism " args "'")))
  (epi-build-and-run-inner cmd)))

(defun epi-build-and-run-no-prompt ()
  "Build epimorphism & run it no prompt."
  (interactive)
  (epi-build-and-run-inner epi-cmd))

(defun epi-prev-cmd ()
  "Build epimorphism & run it no prompt."
  (interactive)
  (let ((epi-exec-ret (selected-window)))
    (shelly-times)
    (vterm-send-C-c)
    (vterm-send-C-p)
    (vterm-send-return)
    (select-window epi-exec-ret)))

;;(define-key gene-mode-map (kbd "g") 'epi-build-and-run-no-prompt)
(define-key gene-mode-map (kbd "C-M-g") 'epi-build-and-run-no-prompt)
(define-key gene-mode-map (kbd "g") 'epi-build-and-run-linux)
(define-key gene-mode-map (kbd "f") 'epi-build-and-run-fb)
(define-key gene-mode-map (kbd "h") 'epi-build-and-run-osx)
(define-key gene-mode-map (kbd "c") 'epi-build-and-run-cross)
(define-key gene-mode-map (kbd "d") 'epi-build-and-run-cross-deploy)
(define-key gene-mode-map (kbd "p") 'epi-prev-cmd)

(define-key gene-mode-map (kbd "x") 'epi-exit)

(put 'downcase-region 'disabled nil)

;;(set-face-attribute 'default t :font "Inconsolata" :height 130)
