;;; packages.el --- lang layer packages file for Spacemacs.
;;
;; Copyright (c) 2020 Tianshu Wang
;;
;; Author: Tianshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq lang-packages '(edit-indirect
                      org-edit-latex cdlatex
                      aggressive-indent
                      python
                      json-mode yaml-mode web-mode
                      auctex))

(defun lang/init-edit-indirect ()
  (use-package edit-indirect :defer t))

(defun lang/init-org-edit-latex ()
  (use-package org-edit-latex :defer t))

(defun lang/init-cdlatex ()
  (use-package cdlatex
    :defer t
    :hook ((org-mode . org-cdlatex-mode)
           ((LaTeX-mode latex-mode) . turn-on-cdlatex))))

(defun lang/pre-init-aggressive-indent ()
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(defun lang/post-init-python ()
  (setq python-shell-interpreter "/usr/local/anaconda3/bin/python"
        org-babel-python-command "/usr/local/anaconda3/bin/python"))

(defun lang/init-json-mode ()
  (use-package json-mode
    :defer t
    :config
    (add-hook 'json-mode-hook
              (lambda ()
                (make-local-variable 'js-indent-level)
                (setq js-indent-level 2)))))

(defun lang/init-yaml-mode ()
  (use-package yaml-mode :defer t))

(defun lang/init-web-mode ()
  (use-package web-mode :defer t))

(defun lang/post-init-auctex ()
  (custom-set-faces
   '(preview-reference-face ((t (:foreground "black")))))

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-save-query nil)
              (local-set-key (kbd "<H-S-mouse-1>") #'TeX-view)))
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  (setq TeX-engine 'xetex
        TeX-master t
        TeX-source-correlate-mode t
        ;; TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server t)
  (setq TeX-view-program-list
        '(("Preview.app" "open -a Preview.app %o")
          ("Skim" "open -a Skim.app %o")
          ("displayline" "displayline -b %n %o %b")
          ("open" "open %o"))
        TeX-view-program-selection
        '((output-dvi "open")
          (output-pdf "displayline")
          (output-html "open"))))
