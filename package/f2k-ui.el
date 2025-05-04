;;; f2k-ui.el --- Appearance config for f2k configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Indications
(global-display-line-numbers-mode t)
(column-number-mode t)
(line-number-mode t)
(ido-mode t)

(setq display-time-24hr-format t)
(display-time-mode t)



;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; UTF-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Font settings
(when (display-graphic-p)
  (let ((preferred-fonts '("FiraCode Nerd Font Mono"
                           "Fira Code"
                           "Consolas")))
    (catch 'found
      (dolist (font preferred-fonts)
        (when (member font (font-family-list))
          (set-face-attribute 'default nil :family font :height 110)
          (throw 'found font))))))

(f2k-require :require 'darkokai-theme
               :then (lambda () (load-theme 'darkokai t)))

;; Smooth scrolling
(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;; Misc
(when (and (fboundp 'display-battery-p)
           (display-battery-p))
  (display-battery-mode t))

(when (fboundp 'yes-or-no-p)
  (fset 'yes-or-no-p 'y-or-n-p))

(provide 'f2k-ui)
;;; f2k-ui.el ends here
