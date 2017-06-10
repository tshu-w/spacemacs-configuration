;;; packages.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017 Voleking
;;
;; Author: Sylvain Benner <volekingsg@gmail.com>
;; URL: https://github.com/Voleking/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun notify-osx (title message)
  (call-process "terminal-notifier"
                nil 0 nil
                ;;"-sender" "org.gnu.Emacs"
                "-group" "Emacs"
                "-title" title
                "-message" message
                "-activate" "org.gnu.Emacs"))

(defun run()
  (interactive)
  (shell-command
   (concat "make -k "
           (if buffer-file-name
               (shell-quote-argument
                (file-name-sans-extension buffer-file-name)))
           " && open "
           (if buffer-file-name
               (shell-quote-argument
                (file-name-sans-extension buffer-file-name))))))
