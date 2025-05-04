;;; f2k-c-cpp.el --- C/C++ language support -*- lexical-binding: t -*-

;;; Commentary:
;; Setup for C/C++ development with:
;; - LSP (clangd)
;; - clang-format on save
;; - indentation
;; - optional electric spacing
;; - corfu + cape completion
;; - auto hook installation

;;; Code:

(f2k-require :require 'cmake-mode)
(f2k-require :require 'clang-format)
(f2k-require :require 'electric-spacing)

(f2k-require :require 'lsp-mode)
(f2k-require :require 'lsp-ui)
(f2k-require :require 'lsp-headerline)

(f2k-require :require 'corfu
             :then (lambda () (corfu-mode 1)))

(f2k-require :require 'cape)

(defun f2k/setup-c-cpp-style ()
  "Configure common settings for C/C++ development."
  (interactive)

  ;; Indentation
  (setq-local c-basic-offset 4)
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)

  ;; clang-format before save
  (setq-local clang-format-style "file")
  (add-hook 'before-save-hook #'f2k/clang-format-buffer-safe nil t)

  ;; Optional electric spacing
  (when (fboundp 'electric-spacing-mode)
    (electric-spacing-mode 1))

  ;; Setup LSP
  (when (fboundp 'lsp)
    (lsp))
  (when (fboundp 'lsp-headerline-breadcrumb-mode)
    (lsp-headerline-breadcrumb-mode 1))

  ;; Setup CAPE for LSP + other backends
  (when (and (fboundp 'cape-capf-super)
             (boundp 'completion-at-point-functions))
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'lsp-completion-at-point
                       #'cape-dabbrev
                       #'cape-file)))))

(defun f2k/clang-format-buffer-safe ()
  "Format buffer with clang-format if available, suppressing errors."
  (when (fboundp 'clang-format-buffer)
    (condition-case err
        (clang-format-buffer)
      (error (message "[f2k] clang-format failed: %s" (error-message-string err))))))

;; Automatically activate in C-like modes
(dolist (hook '(c-mode-hook c++-mode-hook objc-mode-hook))
  (add-hook hook #'f2k/setup-c-cpp-style))

(provide 'f2k-c-cpp)
;;; f2k-c-cpp.el ends here
