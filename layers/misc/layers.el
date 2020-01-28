;;; layers.el --- misc layer layers file for Spacemacs.
;;
;; Copyright (c) 2020 Tianshu Wang
;;
;; Author: Tianshu Wang <volekingsg@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(configuration-layer/declare-layers
 '(
   osx
   (chinese :packages (not pyim chinese-conv)
            :variables chinese-enable-fcitx t)
   (pdf :variables pdf-view-use-scaling t)
   (pandoc :packages (not ox-pandoc)
           :variables pandoc-data-dir "~/.spacemacs.d/pandoc-mode")
   (dash :variables
         dash-docs-docset-newpath "/Users/wangtianshu/Library/Application Support/Dash/DocSets")
   (wakatime :variables
             wakatime-cli-path "/usr/local/bin/wakatime"
             wakatime-api-key "3fd63845-ecde-47ea-bd1a-7042221d1046")
   (geolocation :variables
                geolocation-enable-automatic-theme-changer t
                geolocation-enable-location-service nil
                calendar-location-name "Beijing"
                calendar-latitude 39.90
                calendar-longitude 116.40)
   helpful
   )
 )
