;;; config.el --- my-org layer config file for Spacemacs.
;;
;; Copyright (c) 2020 Tianshu Wang
;;
;; Author: Tianshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-tempo t)

  ;; Org basic
  (setq org-columns-default-format "%50ITEM %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM"
        org-image-actual-width 500
        org-global-properties '(("STYLE_ALL" . "habit"))
        org-hide-emphasis-markers t
        org-log-into-drawer t
        org-startup-indented t
        org-tags-match-list-sublevels 'intented
        org-track-ordered-property-with-tag t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "PROJ(p)" "WAITING(w@/!)" "|" "SOMEDAY(s)" "CANCELED(c)"))
        org-todo-keyword-faces
        '(("CANCELED" . org-upcoming-distant-deadline)
          ("PROJ" . (:foreground "RosyBrown4" :weight bold))
          ("WAITING" . (:foreground "light coral" :weight bold))
          ("SOMEDAY" . (:foreground "plum" :weight bold))))

  ;; Org Capture
  (setq org-gtd-file (expand-file-name "gtd.org" org-directory)
        org-default-notes-file (expand-file-name "gtd.org" org-directory))

  (setq org-capture-templates
        '(("i" "Inbox" entry (file org-gtd-file)
           "* %?\n  %i\n")
          ("j" "Journal" plain (function org-journal-find-location)
           "** %(format-time-string org-journal-time-format)%?")
          ("t" "Todo" entry (file org-gtd-file)
           "* TODO %?\n  %i\n")
          ("l" "Link" plain (file+function org-gtd-file org-capture-goto-link)
           "%i\n" :empty-lines 1 :immediate-finish t)
          ("r" "Record" entry (file org-gtd-file)
           "* %?\n  %i\n" :clock-in t :clock-keep t)
          ("R" "Review")
          ("Rd" "Daily Review" plain (function org-journal-find-location)
           "** Daily Review\n%?\n%i")
          ("Rw" "Weekly Review" plain (function org-journal-find-location)
           "* Weekly Review\n%?\n%i")))

  ;; Org Agenda
  (add-to-list 'org-modules 'org-habit t)

  (setq org-agenda-clockreport-parameter-plist
        '(:maxlevel 3 :scope agenda-with-archives :fileskip0 t :stepskip0 t
                    :emphasize t :link t :narrow 80! :tcolumns 1 :formula %)
        org-agenda-columns-add-appointments-to-effort-sum t
        org-agenda-dim-blocked-tasks t
        org-agenda-persistent-filter t
        org-agenda-skip-additional-timestamps-same-entry t
        org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-timestamp-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-span 1
        org-agenda-start-on-weekday nil
        org-agenda-todo-ignore-scheduled 'all
        org-agenda-todo-ignore-deadlines 'far
        org-agenda-time-grid nil
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies nil
        org-habit-graph-column 75
        org-stuck-projects '("/PROJ" ("TODO") nil ""))

  (setq org-agenda-custom-commands
        '(;; ("c" "Custom agenda view"
          ;;  ((agenda "" ((org-agenda-overriding-header "Today's tasks:")
          ;;               (org-agenda-span 1)))
          ;;   (todo "TODO" ((org-agenda-overriding-header "All TODO items without scheduled or deadline")
          ;;                 (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'timestamp)
          ;;                                                (org-agenda-skip-subtree-if 'regexp "habit"))))))
          ;;  ((org-agenda-compact-blocks t)
          ;;   (org-agenda-dim-blocked-tasks 'invisible)))
          ("O" . "Overview")
          ("Od" "Daily Review"
           ((agenda "" ((org-agenda-span 3))))
           ((org-agenda-compact-blocks t)
            (org-agenda-start-with-log-mode '(closed clock state))
            (org-agenda-start-with-clockreport-mode t)
            (org-agenda-archives-mode t)))
          ("Ow" "Weekly Review"
           ((agenda "" ((org-agenda-span 7)
                        (org-agenda-start-on-weekday 1)))
            (stuck "")
            (todo "PROJ")
            (todo "WAITING")
            (todo "SOMEDAY"))
           ((org-agenda-compact-blocks t)
            (org-agenda-start-with-clockreport-mode t)
            (org-agenda-archives-mode t)))
          ("d" "Upcoming deadlines" agenda ""
           ((org-agenda-entry-types '(:deadline))
            (org-agenda-span 1)
            (org-deadline-warning-days 30)))))

  ;; Org Clock
  (setq org-clock-auto-clock-resolution 'when-no-clock-is-running
        org-clock-history-length 10
        org-clock-idle-time 10
        org-clock-in-resume t
        org-clock-persist t
        org-clock-persist-query-resume nil
        org-clock-out-remove-zero-time-clocks t
        org-clock-out-when-done t
        org-clock-report-include-clocking-task t)

  ;; Org Refile
  (setq deft-files (directory-files-recursively deft-directory ""))
  (setq org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-outline-path 'file
        org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 4)
          (deft-files :maxlevel . 2)))

  ;; Org Babel
  (setq org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0
        org-src-preserve-indentation t
        org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (C . t)
     (org . t)
     (latex . t)))
  (setq org-babel-C-compiler "gcc -std=c++17"
        org-babel-C++-compiler "g++ -std=c++17")

  ;; Org Export
  (with-eval-after-load 'ox-html
    (setq org-export-with-section-numbers nil
          org-html-divs '((preamble "header" "preamble")
                          (content "main" "content")
                          (postamble "footer" "postamble"))
          org-html-doctype "html5"
          org-html-html5-fancy t
          org-html-checkbox-type 'html
          org-html-metadata-timestamp-format "%Y-%m-%d"
          org-html-htmlize-output-type 'inline-css
          org-publish-project-alist
          `(("site" :components ("posts" "static"))
            ("posts"
             :base-directory ,(expand-file-name "Site/content/posts/" org-directory)
             :base-extension "org"
             :publishing-directory ,(expand-file-name "Site/public/posts/" org-directory)
             :recursive t
             :exclude "sitemap.org"
             :publishing-function org-html-publish-to-html
             :headline-levels 3
             :html-head-include-default-style nil
             :html-head-include-scripts nil
             :html-preamble t
             :html-postamble t
             :auto-sitemap t
             :sitemap-filename "_sitemap.org"
             :sitemap-title "Sitemap"
             :sitemap-format-entry org-publish-sitemap-default-entry
             :sitemap-function org-publish-sitemap-default)
            ("pages"
             :base-directory ,(expand-file-name "Site/content/" org-directory)
             :base-extension "org"
             :publishing-directory ,(expand-file-name "Site/public/" org-directory)
             :recursive t
             :exclude ,(regexp-opt (list "posts" "static" "public"))
             :publishing-function org-html-publish-to-html
             :html-head-include-default-style nil
             :html-head-include-scripts nil
             :html-preamble t
             :html-postamble t
             :auto-sitemap nil)
            ("static"
             :base-directory ,(expand-file-name "Site/content/static/" org-directory)
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|svg\\|pdf\\|mp3\\|ogg\\|eot\\|woff\\|woff2\\|ttf"
             :publishing-directory ,(expand-file-name "site/public/static/" org-directory)
             :recursive t
             :publishing-function org-publish-attachment)
            ("rss"
             :base-directory ,(expand-file-name "Site/content/" org-directory)
             :base-extension "org"
             :publishing-directory ,(expand-file-name "Site/public/" org-directory)
             :publishing-function org-rss-publish-to-rss
             :html-link-home "http://example.com"
             :html-link-use-abs-url t
             :rss-extension "xml"
             :table-of-contents nil
             :exclude ".*"
             :include ("sitemap.org"))))
    (add-to-list 'org-export-filter-link-functions 'filter-local-links)
    )

  ;; Tex
  (with-eval-after-load 'ox-latex
    (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex"))
          org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))

    (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))

    (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

    (setq org-latex-compiler "xelatex"
          org-latex-packages-alist '(("" "mathspec" t)
                                     ("fontset=macnew,UTF8" "ctex" t))
          org-preview-latex-default-process 'dvisvgm
          org-preview-latex-process-alist
          '((dvisvgm :programs ("xelatex" "dvisvgm")
                     :description "xdv > svg" :use-xcolor t
                     :message "you need to install the programs: xelatex and dvisvgm."
                     :image-input-type "xdv" :image-output-type "svg" :image-size-adjust (1.7 . 1.5)
                     :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                     :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
            (imagemagick :programs ("xelatex" "convert")
                         :description "pdf > png" :use-xcolor t
                         :message "you need to install the programs: xelatex and imagemagick."
                         :image-input-type "pdf" :image-output-type "png" :image-size-adjust (1.0 . 1.0)
                         :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
                         :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))))
    )

  ;; Org Attach
  (setq org-attach-auto-tag "ATTACH"
        org-attach-archive-delete 'query
        org-attach-directory "attach/"
        org-attach-method 'mv)

  ;; Org Tag
  (setq org-fast-tag-selection-single-key 'expert
        org-tags-column 0
        org-tag-alist (quote ((:startgroup)
                              ("@office" . ?o)
                              ("@home" . ?h)
                              ("@computer" .?c)
                              ("@phone" . ?p)
                              (:endgroup)
                              ("PERSONAL" . ?p)
                              ("WORK" . ?w)
                              ("NOTE" . ?n)
                              ("errants" . ?e)
                              ("Action" . ?a)
                              ("Focused" . ?f)
                              ("Dessert" . ?d))))

  (advice-add #'org-agenda-quit                        :before #'org-save-all-org-buffers)
  (advice-add #'org-agenda-quit                        :after  #'org-agenda-finalize@after)
  (advice-add #'org-agenda-exit                        :after  #'org-agenda-finalize@after)
  (advice-add #'org-agenda-get-restriction-and-command :around #'org-agenda-get-restriction-and-command@around)
  (advice-add #'org-capture-finalize                   :after  #'org-capture-finalize@after)
  (advice-add #'org-capture-select-template            :around #'org-capture-select-template@around)
  (advice-add #'org-refile                             :after  #'org-save-all-org-buffers)
  (advice-add #'org-switch-to-buffer-other-window      :after  #'supress-frame-splitting)
  )
