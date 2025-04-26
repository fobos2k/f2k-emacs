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
(require 'lsp-mode)

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
  (when (fboundp 'electric-spacing-mode)
    (electric-spacing-mode 1))

  (message "🔥 f2k-setup-c-cpp-style: C/C++ style applied"))

;; Hooks
(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)

(add-hook 'c-mode-hook #'f2k-setup-c-cpp-style)
(add-hook 'c++-mode-hook #'f2k-setup-c-cpp-style)

(add-hook 'cmake-mode-hook #'f2k-cmake-setup)

(provide 'f2k-c-cpp)

;;; f2k-c-cpp.el ends here
