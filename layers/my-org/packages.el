(setq my-org-packages '(zotxt org-projectile org-journal))

(defun my-org/init-zotxt ()
  (use-package zotxt
    :defer t
    :hook (org-mode . org-zotxt-mode)
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'org-mode "mz" "zotero")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "zi"    'org-zotxt-insert-reference-link
        "iz"    'org-zotxt-insert-reference-link
        "zo"    'org-zotxt-open-attachment))))

(defun my-org/post-init-org-projectile()
  (with-eval-after-load 'org
    (require 'org-projectile)
    (mapcar 'append-org-agenda-files (org-projectile-todo-files))))

(defun my-org/post-init-org-journal ()
  (setq org-journal-file-type    'weekly
        org-journal-date-format #'org-journal-date-format-func))
