;;; config.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017-2018 Tshu Wang
;;
;; Author: Tshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/Voleking/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(with-eval-after-load 'org
  (require 'org-tempo)
  (setq evil-org-key-theme '(textobjects navigation additional insert todo))
  (setq org-startup-indented t)
  (setq org-agenda-span 'day)
  (setq org-edit-src-content-indentation 0)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)

  (require 'org-projectile)
  (defun append-org-agenda-files (file)
    "append to org-agenda-files if file exists"
    (when (file-exists-p file)
      (push file org-agenda-files)))
	(mapcar 'append-org-agenda-files
			(org-projectile-todo-files))

  (setq deft-files (directory-files-recursively deft-directory ""))

  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets
        '((nil :maxlevel . 1)
          (org-agenda-files :maxlevel . 2)
          (deft-files :maxlevel . 1)))

  (defun deft-search-for (filter)
    (interactive "MFilter: ")
    (spacemacs/deft)
    (deft-filter filter t))

  (defun org-search ()
    "use org-refile to search org-mode headings"
    (interactive)
    (org-refile '(4)))
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "sf" 'org-search)

  (setq org-agenda-custom-commands
        '(("c" "Daily agenda and all TODOs"
           ((tags-todo "PRIORITY=\"A\""
                       ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo))
                        (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (agenda "" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("WAITING")))
                        (org-agenda-overriding-header "Today tasks:")))
            (todo "TODO|STARTED" ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'regexp "\\[#A\\]")
                                                                 (org-agenda-skip-subtree-if 'regexp ":LAST_REPEAT:")
                                                                 (org-agenda-skip-entry-if 'timestamp)))
                      (org-agenda-overriding-header "All TODO Items except High-priority:")))
            (todo "WAITING" )
            (todo "SOMEDAY" )
            )
           ((org-agenda-compact-blocks t)
            (org-agenda-priority-up t)
            (org-agenda-repeating-timestamp-show-all nil)))
         ))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "|" "SOMEDAY(f)" "CANCELED(c!)"))
        )

  ;; https://github.com/syl20bnr/spacemacs/issues/9763
  (setq-default org-directory '"~/Documents/Org")
  (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-directory))
  (setq org-agenda-file-gcal (expand-file-name "gcal.org" org-directory))
  (setq org-agenda-file-archive (expand-file-name "archive.org" org-directory))
  (setq org-default-notes-file (expand-file-name "gtd.org" org-directory))

  (setq org-capture-templates
       '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Tasks")
          "* TODO %?\nSCHEDULED: %t\n")
         ("i" "Inbox" entry (file+headline org-agenda-file-gtd "Inbox")
          "* %?\n  %i\n")
         ("a" "Appointment" entry (file org-agenda-file-gcal)
            "* %?\n\n  %^T\n\n")
         ("l" "Link" entry (file+headline org-agenda-file-gtd "Inbox")
            "* %:annotation\n%i\n" :immediate-finish t :kill-buffer t)
         ("s" "Archive" entry (file+headline org-agenda-file-archive "Archived")
            "* %?\n\t%U\n"
            :empty-lines 1)
         ))

  (setq org-journal-file-format "%Y-%m-%d")
  (setq org-journal-date-prefix "#+TITLE: ")
  (setq org-journal-date-format "%A, %B %d %Y")
  (setq org-journal-time-prefix "* ")
  ;; (setq org-journal-time-format "")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (C . t)
     (latex . t)
     (org . t)
     (dot . t)
     (latex . t)
     ))

  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "red" :weight bold))
          ("WAITING" . (:foreground "IndianRed1" :weight bold))
          ("STARTED" . (:foreground "plum" :weight bold))
          ("SOMEDAY" . (:foreground "thistle" :weight bold))))
  (setq org-todo-keyword-faces '(("TODO" . "red")
                                 ("DOING" . "yellow")
                                 ("DONE" . "green")))

  (add-hook 'org-mode-hook 'org-cdlatex-mode)
  (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

  (setq org-latex-create-formula-image-program 'dvisvgm)
  (setq org-image-actual-width 500)
  (setf org-html-mathjax-options
        '((path " https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.3/MathJax.js?config=TeX-AMS_HTML")
          (scale "100")
          (align "center")
          (font "TeX")
          (linebreaks "false")
          (autonumber "AMS")
          (indent "0em")
          (multlinewidth "85%")
          (tagindent ".8em")
          (tagside "right"))
        )
  )
