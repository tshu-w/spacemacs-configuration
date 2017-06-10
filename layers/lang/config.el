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

;; C++
;;
(setq c-default-style "k&r")
(setq c-basic-offset 4)
(setq-default flycheck-gcc-language-standard "c++11")
(setq-default flycheck-clang-language-standard "c++11")
(setq company-c-headers-path-system '("/usr/include/c++/4.2.1" "/usr/include" "/usr/local/include"))

(require 'compile)
(add-hook 'c++-mode-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                    (concat "make -k "
                            (if buffer-file-name
                                (shell-quote-argument
                                (file-name-sans-extension buffer-file-name))))))))
(add-hook 'c-mode-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                    (concat "make -k "
                            (if buffer-file-name
                                (shell-quote-argument
                                (file-name-sans-extension buffer-file-name))))))))

;; Python
;;
(setq-default python-shell--interpreter '"python")
(setq-default python-indent-offset 4)
