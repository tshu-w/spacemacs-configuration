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

(defun run-c-cpp-file ()
  (interactive)
  (iterm-command
   (concat
    "cd " (locate-dominating-file default-directory "makefile")
    " && make -k " (file-name-sans-extension buffer-file-name)
    " && cd " (replace-regexp-in-string "\\\\" "\\\\\\\\" (shell-quote-argument (or default-directory "~")))
    " && clear"
    " && ./" (file-name-nondirectory (file-name-sans-extension buffer-file-name)))))

(defun run-current-file (arg)
  "Execute the current file.
If the file is modified or not saved, save it automatically before run.
Version 2017-06-17"
  (interactive "P")
  (let ((-suffix-map
         ;; (‹extension› . ‹shell program name›)
         `(
           ("pl" . "perl")
           ("rb" . "ruby")
           ("go" . "go run")
           ("js" . "node")
           ("sh" . "bash")
           ("php" . "php")
           ("java" . "javac")
           ))
        -fname -fsuffix -prog-name -cmd-str)
    (when (not (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))
    (message buffer-file-name)
    (setq -fname buffer-file-name)
    (setq -fsuffix (file-name-extension -fname))
    (setq -prog-name (cdr (assoc -fsuffix -suffix-map)))
    (setq -cmd-str (concat -prog-name " \""   -fname "\""))
    (cond
      ((string-equal -fsuffix "el")  (load -fname))
      ((string-equal -fsuffix "py")  (spacemacs/python-execute-file-focus arg))
      ((string-equal -fsuffix "c")   (run-c-cpp-file))
      ((string-equal -fsuffix "cpp") (run-c-cpp-file))
      ((string-equal -fsuffix "java")
       (progn
         (shell-command -cmd-str "*run-current-file output*" )
         (shell-command
          (format "java %s" (file-name-sans-extension (file-name-nondirectory -fname))))))
      (t (if -prog-name
             (progn (message "Running...")
                    (shell-command -cmd-str "*run-current-file output*"))
           (message "No recognized program file suffix for this file."))))))
