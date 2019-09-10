;; remap mac modifier keys
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)

;; GLOBAL CONFIGURATION

(require 'package)
(setq package-user-dir "~/.emacs.d/packages/")
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("emacs-pe" . "http://emacs-pe.github.io/packages/"))

(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       ;;(if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)) ;;)
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'exec-path-from-shell 'flycheck 'coffee-mode 'expand-region 'flx-ido 'flx 'grizzl 'haskell-mode 'helm-projectile 'projectile 'helm 'async 'magit 'powerline 'intero 'rvm 'psc-ide 'use-package 'spaceline 'purescript-mode 'glsl-mode)


;; fundamental mode for scratch buffer
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

(setq default-directory "~" )

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
;; if indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks
         (lambda () (if (not indent-tabs-mode)
                        (untabify (point-min) (point-max)))
                     nil ))

(global-subword-mode 1)

;; Temporary files
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;; add osx path & sync with rvm
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(rvm-activate-corresponding-ruby)


;; INDIVIDUAL PACKAGE & MODE CONFIGURATION

(add-to-list 'load-path "~/.emacs.d/epimorphism/")
(autoload 'epimorphism-mode "epimorphism-mode" nil t)
(setq auto-mode-alist (cons '("\.epi$" . epimorphism-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.lib$" . epimorphism-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.slib$" . epimorphism-mode) auto-mode-alist))

(add-hook 'after-init-hook 'global-flycheck-mode)

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'intero-mode)

(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

(require 'psc-ide)

;;(setq psc-ide-use-purs nil)
(add-hook 'purescript-mode-hook
  (lambda ()
    (psc-ide-mode)
    (company-mode)
    (flycheck-mode)
    (turn-on-purescript-indentation)))

;;(setq psc-ide-use-npm-bin t)

(global-set-key (kbd "C-M-/") 'company-complete)

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; tab width bs
(setq-default c-basic-offset 2
                  tab-width 2
                  indent-tabs-mode nil)

(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent 4)))

(setq c-default-style "linux")
;;(define-key c-mode-base-map "\t" 'self-insert-command)

(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++17")))

(add-hook 'c-mode-hook
      '(lambda()
        (setq c-basic-offset 2)
        (setq indent-tabs-mode t)))

(add-hook 'css-mode-hook
      (lambda ()
        (setq css-indent-offset 2)
        (setq indent-tabs-mode nil)))

(add-hook 'scss-mode-hook
      (lambda ()
        (setq css-indent-offset 2)
        (setq indent-tabs-mode nil)))

(add-hook 'coffee-mode-hook 'coffee-custom)
(setq auto-mode-alist (cons '("\\.coffee.erb$" . coffee-mode) auto-mode-alist))
(defun coffee-custom ()
  "coffee-mode-hook"
  (make-local-variable 'tab-width)
  (set 'tab-width 2)
)

(setq tab-width 2)
(setq js-indent-level 2)

;; no coding: utf-8 lines
(setq ruby-insert-encoding-magic-comment nil)

;; projectile setup
(projectile-global-mode) ;; to enable in all buffers
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;(setq projectile-enable-caching t)
(helm-mode 1)
(setq projectile-use-git-grep 1)

(require 'grizzl)
(setq projectile-completion-system 'grizzl)

;; ido mode
(require 'ido)
(require 'flx-ido)
(ido-mode 1)
;;(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; org mode
;; The following lines are always needed.  Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
(setq org-startup-indented t)
(setq org-log-done t)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; windmove setup
(windmove-default-keybindings)
(global-set-key (kbd "C-M-j") 'windmove-left)
(global-set-key (kbd "C-M-<left>") 'windmove-left)
(global-set-key (kbd "C-M-l") 'windmove-right)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "C-M-i") 'windmove-up)
(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-k") 'windmove-down)
(global-set-key (kbd "C-M-<down>") 'windmove-down)

(require 'sql)
(autoload 'sql-mode "sql-mode" "SQL Editing Mode" t)
      (setq auto-mode-alist
         (append '(("\\.sql$" . sql-mode)
                   ("\\.q$" . sql-mode))
                 auto-mode-alist))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-type (quote stack-ghci))
 '(package-selected-packages
   (quote
    (cmake-mode projectile psc-ide spaceline use-package intero intero-mode powerlinem rvm exec-path-from-shell yaml-mode rubocop purescript-mode powerline markdown-mode magit helm-projectile grizzl glsl-mode flx-ido expand-region coffee-mode))))





(global-set-key (kbd "C-M-w") 'kill-ring-save)
(global-set-key (kbd "C-x 1") 'nil)
(global-set-key (kbd "C-x C-b") 'nil)

(global-set-key (kbd "M-B") 'magit-blame)
(global-set-key (kbd "M-L") 'flycheck-next-error)



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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)


;; UI CONFIGURATION

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'prassee t)
(set-frame-parameter (selected-frame) 'alpha '(92 . 75))
(add-to-list 'default-frame-alist '(alpha . (92 . 75)))

(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

;; remap mac modifier keys
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)

(set-background-color "Black")
(set-foreground-color "White")

(require 'powerline)
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

(require 'uniquify)


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
