;; -*- lexical-binding: t; -*-
;;; Early Init --- Early init code
;;; Commentary:
;;; Code loaded before anything else
;;; 
;;; Code:
(setq gc-cons-threshold most-positive-fixnum   ; disable GC during init
      gc-cons-percentage 0.6)

;; Prevent regex matching on every loaded file during init
(defvar mjs/--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq package-enable-at-startup nil)

(setq default-frame-alist '((tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)))

(require 'base16-stylix-theme)
(load-theme 'base16-stylix t)
(setq base16-theme-256-color-source 'colors)

(provide 'early-init)
;;; early-init.el ends here
