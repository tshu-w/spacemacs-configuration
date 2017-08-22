;;; funcs.el --- Org Layer packages File for Spacemacs
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

;; Iterm2 Intergration
(defun iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"))

(defun iterm-command (cmd)
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " do shell script \"open -a iTerm\"\n"
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"%s\" \n" cmd)
    "   end tell\n"
    " end tell\n")))

(defun iterm-goto-filedir-or-home ()
  "Go to present working dir and focus iterm"
  (interactive)
  (iterm-command (concat (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                                   (shell-quote-argument (or default-directory "~")))
                         "; clear")))
