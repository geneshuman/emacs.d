;; remap mac modifier keys
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)

;; PACKAGE CONFIGURATION
(require 'package)
(setq package-user-dir "~/.emacs.d/packages/")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("emacs-pe" . "http://emacs-pe.github.io/packages/"))
(package-initialize)

;;(setq lsp-use-plists t)
;; intero

(load-file "~/.emacs.d/init_core.el")
(load-file "~/.emacs.d/init_complete.el")
(load-file "~/.emacs.d/init_lsp.el")

;; (auto-package-update-maybe)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-type 'stack-ghci)
 '(package-selected-packages
   '(layout-restore yasnippet-snippets popper lsp-clangd noccur
                    git-auto-commit-mode vterm window-purpose
                    rainbow-delimiters xterm-color helpful
                    ivy-posframe counsel-projectile counsel
                    modern-c++-font-lock company-c-headers
                    company-mode company-capf modern-cpp-font-lock
                    lsp-ivy which-key lsp-company lsp-ui ivy-xref
                    lsp-mode diredfl dired-filetype-face avy ivy-hydra
                    whole-line-or-region ivy-rich pdf-tools undo-tree
                    auto-package-update cmake-mode projectile psc-ide
                    spaceline use-package intero intero-mode
                    powerlinem rvm exec-path-from-shell yaml-mode
                    rubocop purescript-mode powerline markdown-mode
                    magit helm-projectile grizzl glsl-mode flx-ido
                    expand-region coffee-mode))
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
