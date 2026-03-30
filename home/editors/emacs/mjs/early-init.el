;; -*- lexical-binding: t; -*-
;;; Early Init --- Early init code
;;; Commentary:
;;; Code loaded before anything else
;;; 
;;; Code:
(setq package-enable-at-startup nil)

(require 'base16-stylix-theme)
(load-theme 'base16-stylix t)
(setq base16-theme-256-color-source 'colors)

(provide 'early-init)
;;; early-init.el ends here
