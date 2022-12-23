(ensure-package-installed 'lsp-mode 'dap-mode)

;; c & c++
(setq c-default-style "linux")
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++17")))
;;(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'c-mode-hook
      '(lambda()
        (setq c-basic-offset 2)
        (setq indent-tabs-mode t)))


(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c z")
  :config
  (define-key lsp-mode-map (kbd "C-c z") lsp-command-map)
  (require 'lsp-clients)
  :hook (lsp-mode . lsp-enable-which-key-integration))


(setq lsp-keymap-prefix "C-'")

;;(use-package lsp-mode
;;  :init
;;  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;  (setq lsp-keymap-prefix "C-'")
;;  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;         (c++-mode . lsp)
;;         )
;;  :commands lsp)


(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools))
  ;;(yas-global-mode))

(setq lsp-idle-delay 0.1)  ;; clangd is fast

(setq lsp-disabled-clients "ccls")
