;;; keybindings.el --- editor layer keybindings file for Spacemacs.
;;
;; Copyright (c) 2020 Tianshu Wang
;;
;; Author: Tianshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs/set-leader-keys
  "o" (lookup-key global-map (kbd "M-m a o"))
  "og" 'org-mac-grab-link)
(spacemacs/declare-prefix "of" "feeds")
(spacemacs/declare-prefix "oC" "clock")
(spacemacs/declare-prefix "oj" "org-journal")

(global-set-key (kbd "H-<backspace>")
                (lambda () (interactive) (kill-line 0) (indent-according-to-mode)))

(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "DEL") 'evil-repeat-find-char-reverse))

(with-eval-after-load 'transient
  (transient-bind-q-to-quit))

(with-eval-after-load 'magit
  (transient-append-suffix 'magit-remote "C"
    '("o" "Open remote" magit-open-repo)))
