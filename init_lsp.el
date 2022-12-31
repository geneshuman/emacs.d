(ensure-package-installed 'lsp-mode 'lsp-ui 'lsp-ivy 'lsp-treemacs 'ccls  'modern-cpp-font-lock 'cmake-mode)

;; c & c++
(setq c-default-style "linux")
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++17")))
;;(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'c-mode-hook
      '(lambda()
        (setq c-basic-offset 2)
        (setq indent-tabs-mode t)))

(use-package lsp-mode
    :init
    (setq lsp-keymap-prefix "C-'")
    :custom
    ;;(lsp-auto-guess-root t)
    (lsp-prefer-capf t)
    :commands lsp
    :ensure t
    :config
    (define-key lsp-mode-map (kbd "C-'") lsp-command-map)
    :hook (((c-mode c++-mode objc-mode cuda-mode) . lsp)
           (lsp-mode . lsp-enable-which-key-integration))
    )

(setq lsp-disabled-clients '(ccls))

;;(use-package lsp-clangd)

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
;;            (lsp-register-client
;;             (make-lsp-client :new-connection (lsp-tramp-connection "ccls")
;;                              :major-modes '(c++-mode)
;;                              :remote? t
;;                              :server-id 'ccls-remote))
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

;;(use-package ccls
;;  :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;         (lambda () (require 'ccls) (lsp)))
;;  :config
;;  (setq ccls-initialization-options '(:compilationDatabaseDirectory "build")))
;;
;;  ;;(setq ccls-initialization-options '(:cache (:directory ".ccls-cache2"))))
;;  ;;(setq ccls-initialization-options '(:index (:initialBlacklist ["extern"]))))

(require 'lsp-mode)
(require 'lsp-ui)
(require 'lsp-ui-doc)
(add-hook 'c++-mode-hook 'lsp)


(use-package dap-mode
  :defer
  :custom
  (dap-auto-configure-mode t "Automatically configure dap.")
  ;;(dap-auto-configure-features locals breakpoints expressions tooltip)  "Remove the button panel in the top.")
  :config
  ;;; dap for c++
  (require 'dap-codelldb))

(dap-mode 1)
(dap-ui-mode 1)
(dap-tooltip-mode 1)
(dap-ui-expressions)
(dap-ui-locals)


(global-set-key (kbd "C-M-d") 'dap-hydra)


(define-key lsp-mode-map [remap xref-find-apropos] #'lsp-ivy-workspace-symbol)

(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-find-references)

(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
