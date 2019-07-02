;;; config.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017-2019 Tshu Wang
;;
;; Author: Tshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/Voleking/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; All
;;
(add-hook 'prog-mode-hook 'spacemacs/toggle-hungry-delete-on)
;; (if (version< "26.0" emacs-version)
;;     (setq sp-escape-quotes-after-insert nil))

;; C++
;;
;; (with-eval-after-load 'cc-vars
;;   (push '(other . "k&r") c-default-style))
;; (setq c-basic-offset 4)
;; (c-set-offset 'case-label 4)
;; (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
;; (add-hook 'c++-mode-hook (lambda ()
;;                            (set 'company-clang-arguments (list "-I/usr/include/c++/4.2.1" "-Wall" "-std=c++11"))))
;; (setq company-c-headers-path-system '("/usr/include/c++/4.2.1" "/usr/include" "/usr/local/include"))
;; (with-eval-after-load 'projectile
;;   (push '("C" "h") projectile-other-file-alist))

;; Python
;;
(setq-default python-shell--interpreter '"python")
(setq-default python-indent-offset 4)

;; LaTeX
;;
(add-hook 'LaTeX-mode-hook
          (lambda()
            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
            (setq TeX-command-default "XeLaTeX")
            (setq TeX-save-query nil)))
(add-hook 'LaTeX-mode-hook 'cdlatex-mode)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
