;;; funcs.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017-2019 Tshu Wang
;;
;; Author: Tshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/Voleking/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(defun notify-osx (title message)
  (call-process "terminal-notifier"
                nil 0 nil
                "-sound" "default"
                "-title" title
                "-message" message
                "-group" "Emacs"
                ;; "-sender" "com.apple.Terminal"
                "-activate" "org.gnu.Emacs"))

(defun osx-switch-back-to-previous-application ()
  (interactive)
  (do-applescript
   (mapconcat
    #'identity
    '("tell application \"System Events\""
      "  tell process \"Finder\""
      "    activate"
      "    keystroke tab using {command down}"
      "  end tell"
      "end tell")
    "\n")))

(defun set-file-executable ()
  "Add executable permissions on current file."
  (interactive)
  (when (buffer-file-name)
    (set-file-modes buffer-file-name
                    (logior (file-modes buffer-file-name) #o100))
    (message (concat "Made " buffer-file-name " executable"))))

(defun iterm-focus ()
  "open iTerm"
  (interactive)
  (do-applescript " do shell script \"open -a iTerm\"\n"))

(defun iterm-command (cmd)
  "Run shell command in iTerm"
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
  "Go to present working dir and focus iTerm"
  (interactive)
  (iterm-command (concat (replace-regexp-in-string
                          "\\\\" "\\\\\\\\"
                          (shell-quote-argument (or default-directory "~")))
                         "; clear")))
