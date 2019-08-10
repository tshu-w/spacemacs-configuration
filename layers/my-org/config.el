;;; config.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2017-2019 Tshu Wang
;;
;; Author: Tshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/Voleking/spacemacs-configuration
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(with-eval-after-load 'org
  ;; Org basic
  (require 'org-tempo)
  (require 'ox-html)
  (require 'ox-latex)
  (setq org-edit-src-content-indentation 0)
  (setq org-image-actual-width 500)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-startup-indented t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "PROJ(p)" "WAITING(w@/!)" "|" "SOMEDAY(s)" "CANCELED(c)")))

  (setq org-todo-keyword-faces
        '(("TODO" . org-upcoming-deadline)
          ("DONE" . org-upcoming-distant-deadline)
          ("CANCELED" . org-upcoming-distant-deadline)
          ("PROJ" . (:foreground "RosyBrown4" :weight bold))
          ("WAITING" . (:foreground "light coral" :weight bold))
          ("SOMEDAY" . (:foreground "plum" :weight bold))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (C . t)
     (latex . t)
     (org . t)
     (latex . t)))

  (setq deft-files (directory-files-recursively deft-directory ""))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets
        '((nil :maxlevel . 1)
          (org-agenda-files :maxlevel . 2)
          (deft-files :maxlevel . 1)))

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
          (tagside "right")))

  ;; Org for GTD
  (require 'org-projectile)
  (add-to-list 'org-modules 'org-habit t)
  (org-clock-persistence-insinuate)
  (setq org-agenda-clockreport-parameter-plist
        '(:link t :maxlevel 3 :scope agenda-with-archives :fileskip0 t :stepskip0 t :compact t :narrow 80))
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-persistent-filter t)
  (setq org-agenda-skip-additional-timestamps-same-entry t)
  (setq org-agenda-time-grid nil)
  (setq org-global-properties
        '(("Effort_ALL" .
           "0:10 0:15 0:30 0:45 1:00 2:00 3:00 5:00 0:00")
          ("STYLE_ALL" . "habit")))
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  (setq org-clock-continuously t)
  (setq org-clock-history-length 10)
  (setq org-clock-idle-time 5)
  (setq org-clock-in-resume t)
  (setq org-clock-persist t)
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-out-when-done t)
  (setq org-columns-default-format "%50ITEM %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-habit-graph-column 70)
  (setq org-pretty-entities t)
  (setq org-stuck-projects '("/PROJ" ("TODO") nil ""))
  (setq org-tags-match-list-sublevels 'indented)
  (setq org-track-ordered-property-with-tag t)

  (defun my-org-clock-select-task ()
    (interactive)
    (org-clock-in '(4)))

  (defun append-org-agenda-files (file)
    "append to org-agenda-files if file exists"
    (when (file-exists-p file)
      (push file org-agenda-files)))
	(mapcar 'append-org-agenda-files
			(org-projectile-todo-files))

  (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-directory))
  (setq org-default-notes-file (expand-file-name "gtd.org" org-directory))

  (defun org-journal-find-location ()
    (org-journal-new-entry t)
    (goto-char (point-max)))

  (defun org-capture-goto-link ()
    (org-capture-put :target (list 'file+headline
                                   (nth 1 (org-capture-get :target))
                                   (org-capture-get :annotation)))
    (org-capture-put-target-region-and-position)
    (widen)
    (let ((hd (org-capture-get :annotation)))
      (goto-char (point-min))
      (if (re-search-forward
          (format org-complex-heading-regexp-format (regexp-quote hd)) nil t)
          (org-end-of-subtree)
        (goto-char (point-max))
        (or (bolp) (insert "\n"))
        (insert "* " (nth 2 (org-capture-get :target)) "\n"))))

  (setq org-capture-templates
        '(("j" "Journal" plain (function org-journal-find-location)
           "** %(format-time-string org-journal-time-format)%k%?" :clock-in t :clock-resume t)
          ("i" "Inbox" entry (file org-agenda-file-gtd)
           "* TODO %?\n  %i\n")
          ("l" "Link" plain (file+function org-agenda-file-gtd org-capture-goto-link)
           "%i\n" :empty-lines 1 :immediate-finish t)
          ("n" "Note" plain (function deft-new-file)
           "#+TITLE: %?")
          ("d" "Deadline")
          ("dt" "Today" entry (file org-agenda-file-gtd)
           "* TODO %?\nDEADLINE: %t\n")
          ("dT" "Tomorrow" entry (file org-agenda-file-gtd)
           "* TODO %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))")
          ("dd" "Deadline" entry (file org-agenda-file-gtd)
           "* TODO %?\nDEADLINE: %^T")
          ("s" "SCHEDULE")
          ("st" "Today" entry (file org-agenda-file-gtd)
           "* TODO %?\nSCHEDULED: %t\n")
          ("sT" "Tomorrow" entry (file org-agenda-file-gtd)
           "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))")
          ("ss" "Schedule" entry (file org-agenda-file-gtd)
           "* TODO %?\nSCHEDULED: %^T")))

  (setq org-agenda-custom-commands
        '(("c" "Daily agenda and all TODOs"
           ((agenda "" ((org-agenda-overriding-header "Today's tasks:")
                        (org-agenda-skip-scheduled-if-done t)
                        (org-agenda-skip-deadline-if-done t)
                        (org-agenda-skip-timestamp-if-done t)
                        (org-agenda-skip-scheduled-if-deadline-is-shown t)
                        (org-agenda-span 1)))
            (todo "TODO" ((org-agenda-overriding-header "All TODO items without scheduled or deadline")
                          (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'timestamp)
                                                         (org-agenda-skip-subtree-if 'regexp "habit"))))))
           ((org-agenda-compact-blocks t)
            (org-agenda-dim-blocked-tasks 'invisible)
            (org-agenda-repeating-timestamp-show-all nil)))
          ("O" . "Overview")
          ("Od" "Daily Review"
           ((agenda "" ((org-agenda-span 3))))
           ((org-agenda-compact-blocks t)
            (org-agenda-start-with-log-mode '(closed clock state))
            (org-agenda-start-with-clockreport-mode t)
            (org-agenda-archives-mode)))
          ("Ow" "Weekly Review"
           ((agenda "" ((org-agenda-span 7)
                        (org-agenda-start-on-weekday 1)))
            (todo "PROJECT")
            (todo "WAITING")
            (todo "SOMEDAY")
            (stuck ""))
           ((org-agenda-compact-blocks t)
            (org-agenda-start-with-clockreport-mode t)
            (org-agenda-archives-mode t)))
          ("d" "Upcoming deadlines" agenda ""
           ((org-agenda-entry-types '(:deadline))
            (org-agenda-span 1)
            (org-deadline-warning-days 30)))))

  ;; Tex
  (add-hook 'org-mode-hook 'org-cdlatex-mode)
  (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

  (setq org-latex-compiler "xelatex")
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-latex-packages-alist
               '(("fontset=macnew,UTF8" "ctex" t)))
  (setq org-preview-latex-process-alist
        '((dvisvgm :programs
                   ("xelatex" "dvisvgm")
                   :description "xdv > svg" :message "you need to install the programs: xelatex and dvisvgm." :use-xcolor t :image-input-type "xdv" :image-output-type "svg" :image-size-adjust
                   (1.7 . 1.5)
                   :latex-compiler
                   ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                   :image-converter
                   ("dvisvgm %f -n -b min -c %S -o %O"))
          (imagemagick :programs
                       ("xelatex" "convert")
                       :description "pdf > png" :message "you need to install the programs: xelatex and imagemagick." :use-xcolor t :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                       (1.0 . 1.0)
                       :latex-compiler
                       ("xelatex -interaction nonstopmode -output-directory %o %f")
                       :image-converter
                       ("convert -density %D -trim -antialias %f -quality 100 %O"))))

  ;; org-journal
  (defun org-journal-date-format-func (time)
    "Custom function to insert journal date header,
and some custom text on a newly created journal file."
    (when (= (buffer-size) 0)
      (insert
       (pcase org-journal-file-type
         (`daily "#+TITLE: Daily Journal\n\n")
         (`weekly "#+TITLE: Weekly Journal\n\n")
         (`monthly "#+TITLE: Monthly Journal\n\n")
         (`yearly "#+TITLE: Yearly Journal\n\n"))))
    (concat org-journal-date-prefix (format-time-string "%A, %B %d %Y" time)))

  (setq org-journal-file-type 'weekly)
  (setq org-journal-date-format 'org-journal-date-format-func)

  ;; Function
  (defun org-search ()
    "use org-refile to search org-mode headings"
    (interactive)
    (org-refile '(4)))
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "sf" 'org-search)
  )
