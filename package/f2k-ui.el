;;; f2k-ui.el --- Appearance config for f2k configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Indications
(global-display-line-numbers-mode t)
(column-number-mode t)
(line-number-mode t)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; UTF-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Font settings
(when (display-graphic-p)
  (let ((preferred-fonts '("Fira Code" "Consolas")))
    (catch 'found
      (dolist (font preferred-fonts)
        (when (member font (font-family-list))
          (set-face-attribute 'default nil :family font :height 110)
          (throw 'found font))))))

;; Theme setup (offline-safe
(let ((theme-dir (expand-file-name "elpa/" user-emacs-directory)))
  (when (file-directory-p theme-dir)
    (dolist (entry (directory-files theme-dir t "-theme-"))
      (when (and (file-directory-p entry)
                 (string-match-p "-theme-" (file-name-nondirectory entry)))
        (message "Found theme: `%s'..." entry)
        (add-to-list 'custom-theme-load-path entry)))))

(when (member 'darkokai (custom-available-themes))
  (load-theme 'darkokai t))

;; Smooth scrolling
(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;; Shorter prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Misc
(setq display-time-24hr-format t)
(display-time-mode t)
(ido-mode t)

(provide 'f2k-ui)
;;; f2k-ui.el ends here
