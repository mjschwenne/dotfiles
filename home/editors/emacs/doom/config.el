;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Matt Schwennesen"
      user-mail-address "matt@schwennesen.org")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15 :weight 'light))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/")

(map! :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right)

;; Make evil-snipe search the whole buffer by default
(customize-set-variable 'evil-snipe-scope 'buffer)
(customize-set-variable 'evil-want-C-u-scroll nil)

;; Configuration for Built-in Modes

(add-hook 'org-mode-hook 'auto-fill-mode)
;; Image previewing in org mode
(customize-set-variable 'org-startup-with-inline-images t)
;; Normally, this would be set with `customize-set-variable`, but DOOM Emacs has a function
;; which is loaded as part of their org initialization which overrides this variable.
;; While I could change that, I want to be able to pull the DOOM repo without issue so
;; I'm using advice to update that value after anytime the function in question is called.
(advice-add '+org-init-appearance-h :after (lambda () (customize-set-variable 'org-image-actual-width '(600))))
;; The default font scaling for org-tree-slide is one step too large. The defualt value is 5
;; so we should set it to 4.
(customize-set-variable '+org-present-text-scale 4)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(after! vertico
  (setq vertico-resize t)
  (vertico-reverse-mode t))

(after! org-roam
  (setq org-roam-directory (file-truename org-directory)))

(use-package! olivetti-mode
  :init (setq olivetti-body-width 100)
  :hook org-mode)

(use-package! vulpea
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-mode)))

(use-package! org
  :custom (org-entities-user
           '(("mathcalC" "\\mathcal{C}" nil "&x1D49E" "C" "C" "𝒞")
             ("mathbbE" "\\mathbb{E}" nil "&x1D53C" "E" "E" "𝔼")
             ("mathcalF" "\\mathcal{F}" nil "&x1D4D5" "F" "F" "𝓕")
             ("mathbbN" "\\mathbb{N}" nil "&x2115" "N" "N" "ℕ")
             ("mathcalO" "\\mathcal{O}" nil "&x1D4AA" "O" "O" "𝒪")
             ("mathbbP" "\\mathbb{P}" nil "&x2119" "P" "P" "ℙ")
             ("mathbbR" "\\mathbb{R}" nil "&x211D" "R" "R" "ℝ")
             ("subsetneqq" "\\subsetneqq" nil "&x2ACB" "subsetneqq" "subsetneqq" "⫋")
             ("supseteq" "\\supseteq" nil "&x2287" "supseteq" "supseteq" "⊇")
             ("bot" "\\bot" nil "&x22A5" "_|_" "_|_" "⊥")
             ("top" "\\top" nil "&x22A4" "T" "T" "⊤")
             ("lightning" "\\lightning" nil "&x21AF" "</" "</" "↯")
             ("qed" "\\qedsymbol" nil "&x25A1" "[]" "[]" "☐"))))

(use-package! org-appear
  :after org
  :custom (org-hide-emphasis-markers t)
  (org-appear-autolinks t)
  (org-appear-trigger 'manual)
  :hook (org-mode . (lambda ()
                      (org-appear-mode t)
                      (add-hook 'evil-insert-state-entry-hook
                                #'org-appear-manual-start nil t)
                      (add-hook 'evil-insert-state-exit-hook
                                #'org-appear-manual-stop nil t))))

(use-package! org-modern
  :init (global-org-modern-mode)
  :config (set-face-attribute 'org-modern-done nil
                              :background "#4c566a" :foreground "#eceff4"))

(use-package! proof-general
  :hook (coq-mode . (lambda ()
                      (set-face-background 'proof-locked-face
                                           "#3b4252"))))

(use-package! rainbow-mode
  :hook prog-mode)
