;;; packages.el --- misc layer packages file for Spacemacs.
;;
;; Copyright (c) 2020 Tianshu Wang
;;
;; Author: Tianshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq misc-packages '(helm-github-stars
                      (insert-translated-name
                       :location (recipe :fetcher github :repo "manateelazycat/insert-translated-name"))
                      cal-china-x
                      pdf-tools
                      pandoc-mode))

(defun misc/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t
    :custom
    (helm-github-stars-username "tshu-w")
    (helm-github-stars-refetch-time 1)))

(defun misc/init-insert-translated-name ()
  (use-package insert-translated-name
    :defer t
    :bind ("H-t" . insert-translated-name-insert)))

(defun misc/init-cal-china-x ()
  (use-package cal-china-x
    :after calendar
    :commands cal-china-x-setup
    :init (cal-china-x-setup)
    :config
    (setq calendar-mark-holidays-flag t)

    (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
    (setq cal-china-x-general-holidays
          '((holiday-lunar 1 15 "元宵节")
            (holiday-lunar 7 7 "七夕节")
            (holiday-fixed 3 8 "妇女节")
            (holiday-fixed 3 12 "植树节")
            (holiday-fixed 5 4 "青年节")
            (holiday-fixed 6 1 "儿童节")
            (holiday-fixed 9 10 "教师节")))
    (setq holiday-other-holidays
          '((holiday-fixed 2 14 "情人节")
            (holiday-fixed 4 1 "愚人节")
            (holiday-fixed 12 25 "圣诞节")
            (holiday-float 5 0 2 "母亲节")
            (holiday-float 6 0 3 "父亲节")
            (holiday-float 11 4 4 "感恩节")))
    (setq calendar-holidays
          (append cal-china-x-important-holidays
                  cal-china-x-general-holidays
                  holiday-other-holidays))))

(defun misc/post-init-pdf-tools ()
  (add-hook 'pdf-view-mode-hook (lambda () (pdf-view-midnight-minor-mode))))

(defun misc/post-init-pandoc-mode ()
  (spacemacs/set-leader-keys "a C-p" 'spacemacs/run-pandoc))
