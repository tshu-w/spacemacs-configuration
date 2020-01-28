;;; packages.el --- editor layer packages file for Spacemacs.
;;
;; Copyright (c) 2020 Tianshu Wang
;;
;; Author: Tianshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq editor-packages '((tramp :location built-in)
                        ivy ivy-posframe))

(defun editor/post-init-tramp ()
  ;; fix tramp file slow
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it)))

(defun editor/post-init-ivy ()
  (define-key ivy-minibuffer-map (kbd "<tab>")   'ivy-tab)
  (define-key ivy-minibuffer-map (kbd "C-h")     'ivy-c-h)
  (define-key ivy-minibuffer-map (kbd "C-c C-e") 'ivy-edit))

(defun editor/init-ivy-posframe ()
  (use-package ivy-posframe
    :defer t
    :after ivy
    :init
    (ivy-posframe-mode 1)
    :custom
    (ivy-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
    (ivy-posframe-width 120)
    (ivy-posframe-display-functions-alist '((swiper . nil)
                                            (complete-symbol . ivy-posframe-display-at-point)
                                            (t . ivy-posframe-display-at-frame-center)))))
