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

;; Add executable attribute to file
(defun set-file-executable()
  "Add executable permissions on current file."
  (interactive)
  (when (buffer-file-name)
    (set-file-modes buffer-file-name
                    (logior (file-modes buffer-file-name) #o100))
    (message (concat "Made " buffer-file-name " executable"))))

;; Merge comment-line and comment-dwim
(defun comment-dwim ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line.
URL `http://ergoemacs.org/emacs/emacs_toggle_comment_by_line.html'
Version 2016-10-25"
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let (($lbp (line-beginning-position))
          ($lep (line-end-position)))
      (if (eq $lbp $lep)
          (progn
            (comment-dwim nil))
        (if (eq (point) $lep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region $lbp $lep)
            (forward-line )))))))
