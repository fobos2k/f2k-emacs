;;; f2k-project.el --- Project settings for f2k configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'f2k-package)

;; Dired enhancements
(setq dired-dwim-target t               ; умная цель при копировании
      dired-listing-switches "-alh")    ; человекочитаемый формат

(put 'dired-find-alternate-file 'disabled nil) ; reuse buffer
(f2k-require :require 'dired-x)

;; Project.el defaults
(f2k-require :require 'project
             :then (lambda()
                     (setq project-switch-commands '((project-find-file "Find file")
                                                     (project-find-dir "Find dir")
                                                     (project-dired "Dired")
                                                     (project-shell "Shell")))))

(provide 'f2k-project)
;;; f2k-projects.el ends here
