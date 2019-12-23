;;; funcs.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017-2020 Tianshu Wang
;;
;; Author: Tianshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/tshu-w/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun run-c-cpp-file ()
  (interactive)
  (shell-command
   (concat "make -k "
           (shell-quote-argument
                (file-name-sans-extension buffer-file-name))
           " && open "
               (shell-quote-argument
                (file-name-sans-extension buffer-file-name)))))

(defun run-current-file (arg)
  "Execute the current file.
If the file is modified or not saved, save it automatically before run.
ref: http://ergoemacs.org/emacs/elisp_run_current_file.html"
  (interactive "P")
  (let ((resize-mini-windows nil)
        (-suffix-map
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
    (setq -fname buffer-file-name)
    (setq -fsuffix (file-name-extension -fname))
    (setq -prog-name (cdr (assoc -fsuffix -suffix-map)))
    (setq -cmd-str (concat -prog-name " \"" -fname "\" &"))
    (cond
      ((string-equal -fsuffix "el")  (load -fname))
      ((string-equal -fsuffix "py")  (spacemacs/python-execute-file-focus arg))
      ((string-equal -fsuffix "c")   (run-c-cpp-file))
      ((string-equal -fsuffix "cpp") (run-c-cpp-file))
      ((string-equal -fsuffix "java")
       (progn
         (shell-command (format "java %s" (file-name-sans-extension (file-name-nondirectory -fname)))
                        "*run-current-file output*")))
      (t (if -prog-name
             (progn (message "Running...")
                    (shell-command -cmd-str "*run-current-file output*"))
           (error "No recognized program file suffix for this file."))))))
