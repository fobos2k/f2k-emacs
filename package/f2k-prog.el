;;; f2k-prog.el --- AppearanceProg-mode config for f2k configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(f2k-require :require 'yasnippet
             :then (lambda () (yas-global-mode 1)))

(f2k-require :require 'powershell)

(f2k-require :require 'corfu)
(f2k-require :require 'lsp-mode)


(defun f2k--corfu-setup()
  (message "[f2k] CORFU setup...")
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 2)
  (setq corfu-quit-no-match 'separator)
  (corfu-mode))

(defun f2k--common-langs-setup()
  "Common program languages hook"
  (message "[f2k] Common programming languages setup...")
  (setq display-fill-column-indicator-column 120)
  (display-fill-column-indicator-mode)
  (f2k--corfu-setup)
  (yas-minor-mode)
  (setq lsp-completion-provider :none)
  (setq lsp-auto-guess-root t))

(add-hook 'prog-mode-hook 'f2k--common-langs-setup)

;; (add-hook 'emacs-lisp-mode-hook #'lsp-deferred)

(add-hook 'lsp-mode-hook
          (lambda()
            (setq lsp-headerline-breadcrumb-enable t)))

(add-hook 'sh-mode-hook
          (lambda ()
            (lsp-deferred)
            (setq-local electric-spacing-mode -1)))


(f2k-require :require 'f2k-c-cpp)

(provide 'f2k-prog)
;;; f2k-prog.el ends here
