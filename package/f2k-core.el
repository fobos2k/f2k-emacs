;;; f2k-core.el --- Core config for f2k configuration -*- lexical-binding: t; -*-

;; Indications
(global-display-line-numbers-mode t)
(column-number-mode t)
(line-number-mode t)

;; Disable backup and autosave files
(setq make-backup-files nil)
(setq auto-save-default nil)

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
          (set-face-attribute 'default nil :family font :height 120)
          (throw 'found font))))))

;; Theme setup (offline-safe)
(let ((theme-dir (expand-file-name "elpa/" user-emacs-directory)))
  (when (file-directory-p theme-dir)
    (dolist (entry (directory-files theme-dir t "\\w+"))
      (let ((theme-subdir (expand-file-name entry)))
        (when (file-directory-p theme-subdir)
          (add-to-list 'custom-theme-load-path theme-subdir))))))

(when (member 'darkokai (custom-available-themes))
  (load-theme 'darkokai t))

;; Smooth scrolling
(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;; Shorter prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable yasnippet if available
(when (require 'yasnippet nil 'noerror)
  (yas-global-mode 1))

;; Dired enhancements
(setq dired-dwim-target t               ; умная цель при копировании
      dired-listing-switches "-alh")    ; человекочитаемый формат

(put 'dired-find-alternate-file 'disabled nil) ; reuse buffer
(require 'dired-x) ; дополнительные функции (C-x C-j и пр.)

;; Project.el defaults
(require 'project)
(setq project-switch-commands '((project-find-file "Find file")
                                (project-find-dir "Find dir")
                                (project-dired "Dired")
                                (project-shell "Shell")))

;; Misc
(setq display-time-24hr-format t)
(display-time-mode t)
(ido-mode t)

(provide 'f2k-core)

;;; f2k-core.el ends here
