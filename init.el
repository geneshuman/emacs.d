;; remap mac modifier keys
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)

;; PACKAGE CONFIGURATION
(require 'package)
(setq package-user-dir "~/.emacs.d/packages/")
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
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

(ensure-package-installed 'exec-path-from-shell 'flycheck 'coffee-mode 'expand-region 'haskell-mode 'projectile 'async 'magit 'powerline 'intero 'rvm 'psc-ide 'use-package 'spaceline 'purescript-mode 'glsl-mode 'auto-package-update 'ivy 'counsel 'counsel-projectile 'flx 'ivy-rich 'whole-line-or-region 'undo-tree 'avy 'dired-filetype-face 'diredfl 'ivy-hydra 'pdf-tools 'lsp-mode 'lsp-ui  'ivy-xref 'lsp-ivy 'company 'company-c-headers 'dap-mode 'modern-cpp-font-lock 'which-key 'treemacs 'lsp-treemacs 'company-box 'cmake-mode 'ccls)

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

(whole-line-or-region--turn-on)

;; (require 'magit)

;; misc
(setq default-directory "~" )
(global-subword-mode 1) ;; split by camel case
(define-key key-translation-map [(control ?\;)]  [127]) ;; what is this?
(put 'upcase-region 'disabled nil)
(setq auto-window-vscroll nil) ;; speed up next-line
(setq gc-cons-threshold 100000000) ;; less frequent gc
(setq read-process-output-max (* 1024 1024))

;; key bindings
(global-set-key (kbd "M-+") (lambda () (interactive) (load "~/.emacs.d/init.el")))
(global-set-key (kbd "C-M-S-w") 'kill-ring-save)
(global-set-key (kbd "C-x 1") 'nil)
;;(global-set-key (kbd "C-x C-b") 'nil)
(global-set-key (kbd "M-?") 'nil)
(global-set-key (kbd "M-.") 'nil)
(global-set-key (kbd "M-B") 'magit-blame)
(global-set-key (kbd "M-L") 'flycheck-next-error)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "C-=") 'er/expand-reg )
(global-set-key (kbd "C-M-<backspace>") 'kill-sexp)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.
   \(fn arg char)"
  'interactive)
(global-set-key [remap zap-to-char] 'zap-up-to-char)

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

;; automatically clean up bad whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
(setq-default indent-tabs-mode nil)

;; if indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks
         (lambda () (if (not indent-tabs-mode)
                        (untabify (point-min) (point-max)))
           nil ))

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

;; tab width bs
(setq-default c-basic-offset 2
              tab-width 2
              indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2)

;; specific package configuration

;; projectile setup
(projectile-global-mode) ;; to enable in all buffers
(counsel-projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;(setq projectile-enable-caching t)
(setq projectile-use-git-grep 1)


;; dired
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-dwim-target t)
(require 'dired-filetype-face)
(setq dired-listing-switches "-alh")
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

;; ivy/counsel/swiper
(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  ;;:bin
  ;;(:map ivy-mode-map
  ;;      ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t) ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 10) ;; number of result lines to display
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

(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-c b") 'counsel-bookmark)

(define-key projectile-mode-map [remap projectile-grep] nil)
(global-set-key (kbd "C-c p s G") 'counsel-projectile-grep)

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
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++14")))
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

(setq lsp-enable-on-type-formatting nil)
(setq lsp-enable-indentation nil)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-before-save-edits nil)

(add-hook 'lsp-ui-mode-hook
          (lambda()
            (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
            (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

(with-eval-after-load 'lsp-mode
  ;; :project/:workspace/:file
  (setq lsp-diagnostics-modeline-scope :project)
  (add-hook 'lsp-managed-mode-hook 'lsp-diagnostics-modeline-mode))


(lsp-treemacs-sync-mode 1)

(use-package lsp-ui :commands lsp-ui-mode :ensure t)
(setq lsp-ui-sideline-delay 0.0)
(setq lsp-ui-sideline-show-code-actions nil)
;;(setq lsp-ui-doc-position 'bottom)
;;(setq lsp-ui-doc-alignment 'window)
;;(setq lsp-doc-use-we-webkit t)
(setq lsp-ui-doc-enable nil)




(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (setq ccls-initialization-options '(:compilationDatabaseDirectory "build")))

  ;;(setq ccls-initialization-options '(:cache (:directory ".ccls-cache2"))))
  ;;(setq ccls-initialization-options '(:index (:initialBlacklist ["extern"]))))


(require 'lsp-mode)
(add-hook 'c++-mode-hook 'lsp)


(dap-auto-configure-mode 1)
(require 'dap-lldb)

(use-package which-key
  :config
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
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

;; what the hells is this stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-type 'stack-ghci)
 '(lsp-prefer-capf t)
 '(package-selected-packages
   '(counsel-projectile counsel modern-c++-font-lock dap-lldb company-c-headers company-mode company-capf modern-cpp-font-lock lsp-ivy which-key lsp-company lsp-ui ivy-xref lsp-mode diredfl dired-filetype-face avy ivy-hydra whole-line-or-region ivy-rich pdf-tools undo-tree auto-package-update cmake-mode projectile psc-ide spaceline use-package intero intero-mode powerlinem rvm exec-path-from-shell yaml-mode rubocop purescript-mode powerline markdown-mode magit helm-projectile grizzl glsl-mode flx-ido expand-region coffee-mode)))




;; windmove setup
(windmove-default-keybindings)
(global-set-key (kbd "C-M-j") 'windmove-left)
(global-set-key (kbd "C-M-o") 'windmove-left)
(global-set-key (kbd "C-M-<left>") 'windmove-left)
(global-set-key (kbd "C-M-l") 'windmove-right)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "C-M-i") 'windmove-up)
(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-k") 'windmove-down)
(global-set-key (kbd "C-M-<down>") 'windmove-down)

;; UI CONFIGURATION
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'prassee t)
 (set-frame-parameter (selected-frame) 'alpha '(87 . 50))
 (add-to-list 'default-frame-alist '(alpha . (87 . 50)))
;;(set-frame-parameter (selected-frame) 'alpha '(92 . 75))
;;(add-to-list 'default-frame-alist '(alpha . (92 . 75)))

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

(powerline-default-theme)

(tool-bar-mode -1)
(scroll-bar-mode -1)

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
    (spaceline-toggle-projectile-root-off)
    (spaceline-toggle-battery-off)
    (spaceline-toggle-buffer-encoding-abbrev-off)
    (spaceline-toggle-buffer-size-off)
    (spaceline-toggle-minor-modes-off)

    (spaceline-toggle-buffer-modified-on)
    (spaceline-toggle-buffer-id-on)
    (spaceline-toggle-major-mode-on)
    (spaceline-toggle-hud-on) ;; ?
    (spaceline-emacs-theme)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-selection ((t (:background "dark turquoise" :foreground "black")))))
