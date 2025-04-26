;;; f2k-c-cpp.el --- C/C++ language settings -*- lexical-binding: t -*-

;;; Commentary:
;; Setup for C, C++ and CMake development:
;; - Formatting via clang-format
;; - Proper indentation settings
;; - Electric spacing
;; - LSP integration (clangd)

;;; Code:

(require 'cmake-mode)
(require 'clang-format)
(require 'electric-spacing)

;; Optional LSP components
(require 'lsp-mode)
(require 'lsp-lens)
(require 'lsp-modeline)
(require 'lsp-headerline)
(require 'corfu)
(require 'cape)
(require 'orderless)

(defun f2k-clang-format-buffer-safe ()
  "Format current buffer with clang-format if appropriate."
  (interactive)
  (when (and (derived-mode-p 'c-mode 'c++-mode)
             (> (buffer-size) 0))
    (message "🔥 formatting via clang-format-buffer")
    (clang-format-buffer)))

(defun f2k-cmake-setup ()
  "Common setup for CMake development: formatting, indent, electric spacing."
  (setq-local cmake-tab-width 4)
  (lsp-deferred))

(defun f2k-setup-c-cpp-style ()
  "Common setup for C/C++ development: formatting, indent, electric spacing."
  (interactive)
  ;; Indentation settings
  (setq-local c-basic-offset 4)
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)

  ;; Use .clang-format from project
  (setq-local clang-format-style "file")

  ;; Auto-format buffer before save
  (add-hook 'before-save-hook #'f2k-clang-format-buffer-safe nil t)

  ;; Enable electric-spacing if available
  (when (featurep 'electric-spacing-mode)
    (electric-spacing-mode 1))

  ;; Enable LSP if available
  (when (featurep 'lsp-mode)
    (require 'lsp-completion)
    (lsp-deferred)
    (setq-local lsp-completion-provider :capf)

    ;; Additional LSP UI features
    (when (featurep 'lsp-lens)
      (lsp-lens-mode 1))
    (when (featurep 'lsp-headerline)
      (setq lsp-headerline-breadcrumb-enable t))
    (when (featurep 'lsp-modeline)
      (setq lsp-modeline-code-actions-enable t)))

  ;; Setup CAPE completions
  (when (featurep 'cape)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev))

  ;; Enable corfu
  (when (featurep 'corfu)
    (corfu-mode 1)
    (setq corfu-auto t
          corfu-auto-delay 0.2
          corfu-auto-prefix 1
          corfu-min-width 40
          corfu-max-width 80
          corfu-preview-current nil))
  (setq tab-always-indent 'complete)


  (message "🔥 f2k-setup-c-cpp-style: C/C++ style applied"))

;; Hooks
(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)

(add-hook 'c-mode-hook #'f2k-setup-c-cpp-style)
(add-hook 'c++-mode-hook #'f2k-setup-c-cpp-style)

(add-hook 'cmake-mode-hook #'f2k-cmake-setup)

;; Additional Completion settings if available
(when (and (featurep 'corfu) (featurep 'orderless))
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(provide 'f2k-c-cpp)

;;; f2k-c-cpp.el ends here
