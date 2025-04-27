;;; f2k-common-langs.el --- Common language sets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'corfu)
(require 'lsp-mode)
(require 'project)
(require 'yasnippet)

(require 'f2k-c-cpp)

(defun f2k-corfu-setup()
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 2)
  (setq corfu-quit-no-match 'separator)
  (corfu-mode))

(defun f2k-common-langs-setup()
  "Common program languages hook"
  (display-line-numbers-mode)
  (setq display-fill-column-indicator-column 120)
  (display-fill-column-indicator-mode)
  (f2k-corfu-setup)
  (yas-minor-mode)
  (setq lsp-completion-provider :none)
  (setq lsp-auto-guess-root t))

(add-hook 'prog-mode-hook 'f2k-common-langs-setup)

(add-hook 'lsp-mode-hook
          (lambda()
            (setq lsp-headerline-breadcrumb-enable t)))

(add-hook 'shell-script-mode-hook
          (lambda ()
            (setq-local electric-spacing-mode -1)))

(provide 'f2k-common-langs)
;;; f2k-common-langs.el ends here
