;;; config.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017 Voleking
;;
;; Author: Sylvain Benner <volekingsg@gmail.com>
;; URL: https://github.com/Voleking/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; All
;;
(add-hook 'prog-mode-hook 'spacemacs/toggle-hungry-delete-on)
(if (version< "26.0" emacs-version)
    (setq sp-escape-quotes-after-insert nil))

;; C++
;;
(with-eval-after-load 'cc-vars
  (push '(other . "k&r") c-default-style))
(setq c-basic-offset 4)
(c-set-offset 'case-label 4)
;; (setq-default flycheck-gcc-language-standard "c++11")
(setq-default flycheck-clang-language-standard "c++11")
;; (setq flycheck-clang-include-path (list (expand-file-name "/usr/include/c++/4.2.1")))
(setq company-c-headers-path-system '("/usr/include/c++/4.2.1" "/usr/include" "/usr/local/include"))
;; (set 'company-clang-arguments (list "-I/usr/include/c++/4.2.1"
;;                                     "-Wall"
;;                                     "-std=c++11")
;;      )
(with-eval-after-load 'projectile
  (push '("C" "h") projectile-other-file-alist))

;; Python
;;
(setq-default python-shell--interpreter '"python")
(setq-default python-indent-offset 4)
