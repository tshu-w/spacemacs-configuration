;;; config.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017-2020 Tianshu Wang
;;
;; Author: Tianshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/tshu-w/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; All
;;
(add-hook 'prog-mode-hook 'spacemacs/toggle-hungry-delete-on)
;; (if (version< "26.0" emacs-version)
;;     (setq sp-escape-quotes-after-insert nil))

;; Python
;;
(setq-default python-shell--interpreter '"python")
(setq-default python-indent-offset 4)

;; LaTeX
;;
(add-hook 'LaTeX-mode-hook
          (lambda()
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
        (output-html "open")))

;; Json
;; Make the variable buffer local so that it does not conflict with js-mode for JavaScript files.
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))
