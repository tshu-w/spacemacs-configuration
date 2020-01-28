;;; packages.el --- spacemacs-cfg layer packages file for Spacemacs.
;;
;; Copyright (c) 2020 Tianshu Wang
;;
;; Author: Tianshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst spacemacs-cfg-packages '(writeroom-mode
                                   spaceline doom-modeline
                                   google-translate
                                   (redo-spacemacs :location local)))

(defun spacemacs-cfg/post-init-writeroom-mode ()
  (setq writeroom-width 120
        writeroom-fringes-outside-margins t
        writeroom-bottom-divider-width 0
        writeroom-mode-line t
        writeroom-fullscreen-effect nil
        writeroom-mode-line-toggle-position 'mode-line-format
        writeroom-major-modes '(text-mode prog-mode))
  (global-writeroom-mode t))

(defun spacemacs-cfg/post-init-spaceline ()
  (with-eval-after-load 'spaceline
    (spaceline-toggle-org-clock-on)
    (spaceline-toggle-minor-modes-off)))

(defun spacemacs-cfg/post-init-doom-modeline ()
  (setq inhibit-compacting-font-caches t)
  (with-eval-after-load 'doom-modeline
    (custom-set-faces
     '(mode-line ((t (:height 0.9))))
     '(mode-line-inactive ((t (:height 0.9))))))
  (setq doom-modeline-buffer-file-name-style 'auto
        doom-modeline-buffer-encoding nil))

(defun spacemacs-cfg/post-init-google-translate ()
  ;; https://github.com/atykhonov/google-translate/issues/98#issuecomment-562870854
  (defun google-translate-json-suggestion@override (json)
    "Retrieve from JSON (which returns by the
`google-translate-request' function) suggestion. This function
does matter when translating misspelled word. So instead of
translation it is possible to get suggestion."
    (let ((info (aref json 7)))
      (if (and info (> (length info) 0))
          (aref info 1) nil)))
  (advice-add #'google-translate-json-suggestion :override #'google-translate-json-suggestion@override)

  (setq google-translate-default-source-language "en"
        google-translate-default-target-language "zh-CN"))

(defun spacemacs-cfg/init-redo-spacemacs ()
  ;; https://github.com/ekaschalk/.spacemacs.d/tree/master/layers/config
  (use-package redo-spacemacs
    :init
    (progn
      (setq redo-spacemacs-prefixes-list
            '(;; Primary prefixes
              "C"    ; capture/colors
              "K"    ; kmacros
              "N"    ; navigation
              "P"    ; pandoc
              "R"    ; rectangles
              ;; Sub prefixes
              "a s"  ; shells
              "a o"  ; org
              "b N"  ; new buffer
              "f C"  ; files/convert
              "s a"  ; ag
              "s g"  ; grep
              "s k"  ; ack
              "s r"  ; ripgrep
              "s t"  ; pt
              "w c"  ; centered
              ))
      (setq redo-spacemacs-undo-bindings-alist
            '(;; Top-level
              ("`" winum-select-window-by-number)
              ;; ;; a - applications
              ;; ;; Leaving unchanged
              ;; ;; b - buffers
              ("b." spacemacs/buffer-transient-state/body)
              ("be" spacemacs/safe-erase-buffer)
              ("bP" spacemacs/copy-clipboard-to-whole-buffer)
              ("bR" spacemacs/safe-revert-buffer)
              ("bw" read-only-mode)
              ("bW" spacemacs/goto-buffer-workspace)
              ("bY" spacemacs/copy-whole-buffer-to-clipboard)
              ;; ;; C - capture/colors
              ;; ;; Removed entire leader
              ;; ;; c - compile/comments
              ;; ;; Leaving unchanged
              ;; ;; D - ediff
              ;; ;; d - docs
              ;; ;; e - errors
              ;; ;; F - frame
              ;; ;; f - files
              ("fCu" spacemacs/dos2unix)
              ("fCd" spacemacs/unix2dos)
              ("fj" dired-jump)
              ;; ;; g - git/version-control
              ;; ;; h - help
              ;; ;; i - insertion
              ("ib" insert-buffer)
              ("ij" spacemacs/evil-insert-line-below)
              ("iJ" spacemacs/insert-line-below-no-indent)
              ("ik" spacemacs/evil-insert-line-above)
              ("iK" spacemacs/insert-line-above-no-indent)
              ;; ;; j - jump/join/split
              ("jk" spacemacs/evil-goto-next-line-and-indent)
              ("jo" open-line)
              ("jS" spacemacs/split-and-new-line)
              ;; ;; K - kmacros
              ;; ;; Removed entire leader
              ;; ;; N - navigation
              ;; ;; n - narrow/numbers
              ;; ;; P - pandoc
              ("P/" spacemacs/run-pandoc)
              ;; ;; p - projects
              ("pb" counsel-projectile-switch-to-buffer)
              ("pf" counsel-projectile-find-file)
              ("pv" projectile-vc)
              ("pF" projectile-find-file-dwim)
              ;; ;; q - quit
              ;; ;; R - rectangles
              ;; ;; r - registers/rings/resume
              ;; ;; s - search/symbol
              ("sf" spacemacs/search-auto)
              ("sF" spacemacs/search-auto-region-or-symbol)
              ("sj" spacemacs/counsel-jump-in-buffer)
              ;; ;; T - UI toggles/themes
              ("Tt" spacemacs/toggle-tool-bar)
              ("TZ" zone)
              ;; ;; t - toggles
              ;; ;; w - windows
              ("w[" spacemacs/window-transient-state/spacemacs/shrink-window-horizontally)
              ("w]" spacemacs/window-transient-state/spacemacs/enlarge-window-horizontally)
              ("w-" split-window-below)
              ("w/" split-window-right)
              ("w_" spacemacs/maximize-horizontally)
              ("w|" spacemacs/maximize-vertically)
              ("wF" make-frame)
              ("wo" other-frame)
              ("w{" spacemacs/window-transient-state/spacemacs/shrink-window)
              ("w}" spacemacs/window-transient-state/spacemacs/enlarge-window)
              ("w <down>"    evil-window-down)
              ("w <up>"      evil-window-up)
              ("w <left>"    evil-window-left)
              ("w <right>"   evil-window-right)
              ("w <S-down>"  evil-window-move-very-bottom)
              ("w <S-up>"    evil-window-move-very-top)
              ("w <S-left>"  evil-window-move-far-left)
              ("w <S-right>" evil-window-move-far-right)
              ;; ;; x - text
              ("x TAB" indent-rigidly)
              ("xu" downcase-region)
              ("xU" upcase-region)
              ;; ;; z - zoom
              ))
      )))
