;; -*- lexical-binding: t; -*-

;;; Code:
(setq user-full-name "Matt Schwennesen"
      user-login-name "matt"
      user-real-login-name "mjs"
      user-mail-address "matt@schwennesen.org")

(require 'use-package)
(require 'base16-stylix-theme)
(setq base16-theme-256-color-source 'colors)

(setq use-package-verbose t
	  use-package-always-defer t
	  use-package-always-ensure t
	  use-package-compute-statistics t
	  use-package-enable-imenu-support t)

(setq-default cursor-in-non-selected-widows nil
              speedbar t
              load-prefer-new t
              make-backup-files nil
              compilation-always-kill t
              compilation-ask-about-save nil
              compilation-scroll-output t
              tab-width 4
              indent-tabs-mode nil
              require-final-newline t
              x-select-enable-clipboard t
              fill-column 80
              initial-scratch-message nil
              inhibit-startup-screen t
              column-number-mode t
              enable-recursive-minibuffers t
              use-dialog-box nil
              scroll-margin 8 hscroll-margin 8
              scroll-conservatively 101)

;; Setup autoloads, I'm currently targeting user facing functions not required to load the system
(add-to-list 'load-path (expand-file-name "autoloads" user-emacs-directory))
(loaddefs-generate
 (expand-file-name "autoloads" user-emacs-directory)
 (expand-file-name "autoloads/auto.el" user-emacs-directory))
(require 'auto)

(use-package diminish
  :commands diminish)

(use-package which-key
  :diminish which-key-mode
  :defer nil
  :config (which-key-mode 1))

(use-package general
  :defer nil
  :config
  (general-evil-setup)
  (general-auto-unbind-keys)
  (general-create-definer mjs-leader-def
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :states '(insert motion visual normal)
    :which-key "Leader")
  (general-create-definer mjs-local-leader-def
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m"
    :states '(insert motion visual normal)
    :which-key "Local Leader")
  (general-unbind :states '(insert motion visual) :keymaps 'global "M-SPC"))

(use-package evil
  :defer nil
  :diminish evil-mode
  :custom ((evil-want-keybinding nil)
           (evil-want-integration t)
           (evil-cross-lines t)
           (evil-echo-state nil)
           (evil-undo-system 'undo-redo)
           (evil-want-C-u-scroll t))
  :config
  ;; Make evil search similar to vim
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Rebind `universal-argument` to 'C-M-u' since 'C-u' now scrolls the buffer
  (global-set-key (kbd "C-M-u") 'universal-argument)

  ;; Use visual line movements by default
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Shifting in visual mode keeps selection
  (evil-global-set-key 'visual (kbd ">") 'mjs/evil-shift-right)
  (evil-global-set-key 'visual (kbd "<") 'mjs/evil-shift-left)

  ;; Simplify window movements
  (general-define-key :states '(normal motion) :keymaps 'override
                      "C-h" #'evil-window-left
                      "C-j" #'evil-window-down
                      "C-k" #'evil-window-up
                      "C-l" #'evil-window-right
                      "C-c" #'evil-window-delete)

  (evil-mode 1))

(use-package evil-org
  :defer nil
  :after org evil
  :diminish evil-org-mode
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-set-initial-state 'org-agenda-mode 'normal))

(use-package evil-collection
  :after evil
  :defer nil
  :diminish evil-collection-unimpaired-mode
  :custom (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

(use-package evil-args
  :after evil
  :custom (evil-args-delimiters '(" ")) ; default value is '("," ";")
                                        ; may want to investigate major-mode dependent
                                        ; values
  :general (:keymaps 'evil-inner-text-objects-map "a" 'evil-inner-arg)
  (:keymaps 'evil-outer-text-objects-map "a" 'evil-outer-arg)
  (:states 'normal
           "L" 'evil-forward-arg
           "H" 'evil-backward-arg
           "K" 'evil-jump-out-args)
  (:states 'motion
           "L" 'evil-forward-arg
           "H" 'evil-backward-arg))

                                        ; (use-package evil-easymotion
                                        ;   :after evil
                                        ;   :general (:states 'motion "SPC SPC" '(nil :which-key "Easy Motion")
                                        ;                             "SPC SPC" evilem-map))

(use-package evil-surround
  :after evil
  :hook (emacs-startup . global-evil-surround-mode))

(use-package evil-embrace
  :after evil-surround
  :config
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-escape
  :after evil
  :defer nil
  :diminish evil-escape-mode
  :custom (evil-escape-key-sequence "jk")
  :config (evil-escape-mode))

(use-package evil-exchange
  :after evil
  :config (evil-exchange-install))

(use-package evil-goggles
  :after evil
  :defer nil
  :diminish evil-goggles-mode
  :config (evil-goggles-mode 1))

(use-package evil-indent-plus
  :after evil
  :config (evil-indent-plus-default-bindings))

(use-package evil-lion
  :commands evil-lion-left evil-lion-right
  :general (:states 'normal
                    "g a " #'evil-lion-left
                    "g A " #'evil-lion-right))

(use-package evil-nerd-commenter
  :after evil
  :general (:states 'normal "g c c" '("Comment" . evilnc-comment-or-uncomment-lines))
  (:states 'visual "g c" '("Comment" . evilnc-comment-or-uncomment-lines))
  :config (evilnc-default-hotkeys nil t))

(use-package evil-numbers
  :after evil
  :commands evil-numbers/inc-at-pt evil-numbers/dec-at-pt
  :general (:states '(normal visual)
                    "g -" '("Decrement Number at Point" . evil-numbers/dec-at-pt)
                    "g =" '("Increment Number at Point" . evil-numbers/inc-at-pt)))

(use-package evil-snipe
  :after evil
  :defer nil
  :diminish evil-snipe-local-mode
  :custom ((evil-snipe-smart-case t)
           (evil-snipe-tab-increment t))
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-visualstar
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backwards)
  :custom (evil-visualstar/persistent nil)
  :config (evil-define-key* 'visual 'global
            "*" #'evil-visualstar/begin-search-forward
            "#" #'evil-visualstar/begin-search-backward))

(use-package vimish-fold
  :after evil
  :defer nil
  :diminish vimish-fold-mode)

(use-package evil-vimish-fold
  :after vimish-fold
  :hook (emacs-startup . global-evil-vimish-fold-mode)
  :diminish evil-vimish-fold-mode
  :custom (evil-vimish-fold-targets-mode '(prog-mode conf-mode text-mode)))

(use-package restart-emacs
  :commands restart-emacs)

(mjs-leader-def :keymaps 'override
  "q" '(nil :which-key "Quit")
  "q d" '("Restart Emacs Server" . mjs/restart-server)
  "q f" '("Delete Frame" . save-buffers-kill-emacs)
  "q F" '("Clear Current Frame" . mjs/kill-all-buffers)
  "q K" '("Kill Emacs (and Daemon)" . save-buffers-kill-emacs)
  ;; "q l" '("Restore Last Session" . )
  ;; "q L" '("Restore Session from File" . )
  "q q" '("Quit Emacs" . save-buffers-kill-terminal)
  "q Q" '("Quit Emacs without Saving" . evil-quit-all-with-error-code)
  ;; "q r" '("Restart & Restore Emacs" . )
  "q r" '("Restart Emacs" . mjs/restart-emacs)
  ;; "q s" '("Quick Save Current Session" . )
  ;; "q S" '("Save Session to File" . )
  )

(mjs-leader-def :keymaps 'override
  "b"   '(nil :which-key "Buffer")
  "b b" '("Switch Buffer" . mjs/switch-buffer)
  "b B" '("Switch Buffer (all)" . consult-buffer)
  "b c" '("Clone Buffer" . clone-indirect-buffer)
  "b i" '("ibuffer" . ibuffer)
  "b k" '("Kill Buffer" . kill-current-buffer)
  "b K" '("Kill All Buffers" . mjs/kill-all-buffers)
  "b n" '("Next Buffer" . next-buffer)
  "b ]" '("Next Buffer" . next-buffer)
  "b p" '("Previous Buffer" . previous-buffer)
  "b [" '("Previous Buffer" . previous-buffer)
  "b r" '("Revert Buffer" . revert-buffer)
  "b s" '("Save Buffer" . basic-save-buffer)
  "b S" '("Save All Buffers" . evil-write-all)
  "b z" '("Bury Buffer" . bury-buffer)
  "f"   '(nil :which-key "File")
  "f c" '("Copy this File" . mjs/copy-this-file)
  "f C" '("Open Config" . (lambda ()
                            (interactive)
                            (find-file (concat
                                        user-emacs-directory
                                        "init.el"))))
  "f d" '("Delete this File" . mjs/delete-this-file)
  "f D" '("Browse Directory" . dired-at-point)
  "f f" '("Find File" . find-file)
  "f l" '("Locate File" . locate)
  "f m" '("Move this File" . mjs/move-this-file)
  "f p" '("Find File in Config" . (lambda ()
                                    (interactive)
                                    (let ((default-directory
                                           user-emacs-directory))
                                      (call-interactively #'find-file))))
  "f r" '("Recent Files" . consult-recent-file)
  "f s" '("Save File" . save-buffer)
  "f S" '("Save File As" . write-file)
  "f y" '("Yank File Path" . mjs/yank-buffer-path)
  "f Y" '("Yank Relative File Path" . (lambda ()
                                        (interactive)
                                        (mjs/yank-buffer-path
                                         default-director))))

(global-auto-revert-mode +1)
(diminish 'auto-revert-mode)

(mjs-leader-def :keymaps 'override
  "i"  '(nil :which-key "Insert")
  "i u" '("Character" . insert-char)
  "i 0" '("Zero-Width Space" . (lambda () (interactive) (insert-char #x00200B)))
  "i r" '("Evil Registers" . evil-show-registers)
  "i e" '("Emoji" . emojify-insert-emoji))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(set-frame-parameter (selected-frame) 'font "JetBrainsMono Nerd Font-12")
(add-to-list 'default-frame-alist
             '(font . "JetBrainsMono Nerd Font-12"))

(use-package ligature
  :config
  ;; Enable all JetBrains Mono Nerd Font ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://"))
  ;; Enable most ligatures in text mode, but not all of them since some can
  ;; mess with formatting
  ;; in org-mode or markdown documents
  (ligature-set-ligatures
   'text-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode -1)
  :hook (prog-mode . ligature-mode)
  (text-mode . ligature-mode))

(when window-system (global-hl-line-mode 1))

(use-package rainbow-mode
  :diminish "󰙸 "
  :hook prog-mode)

(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :hook prog-mode)

(use-package hl-todo
  :custom (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
   `(
     ("DONE" . ,(plist-get base16-stylix-theme-colors :base0B))
     ("OKAY" . ,(plist-get base16-stylix-theme-colors :base0D))
     ("NEXT" . ,(plist-get base16-stylix-theme-colors :base0B))
     ("NOTE" . ,(plist-get base16-stylix-theme-colors :base0D))
     ("TODO" . ,(plist-get base16-stylix-theme-colors :base0A))
     ("WARN" . ,(plist-get base16-stylix-theme-colors :base0A))
     ("HOLD" . ,(plist-get base16-stylix-theme-colors :base0A))
     ("HACK" . ,(plist-get base16-stylix-theme-colors :base0A))
     ("XXX*" . ,(plist-get base16-stylix-theme-colors :base0A))
     ("BUG" . ,(plist-get base16-stylix-theme-colors :base08))
     ("FIXME". ,(plist-get base16-stylix-theme-colors :base08))
     ("FAIL" . ,(plist-get base16-stylix-theme-colors :base08))
     ))
  :hook (prog-mode . hl-todo-mode))

(use-package telephone-line
  :defer nil
  :custom (telephone-line-lhs
           '((evil . (telephone-line-evil-tag-segment))
             (accent . (telephone-line-process-segment
                        mjs/minor-mode-segment
                        mjs/popup-segment))
             (nil . (mjs/buffer-mod))))
  (telephone-line-rhs
   '((nil . (telephone-line-misc-info-segment
             mjs/anzu-segment
             mjs/flycheck-status
             telephone-line-atom-encoding-segment))
     (accent . (telephone-line-major-mode-segment))
     (evil . (mjs/buffer-position))))
  :config
  (telephone-line-defsegment* mjs/anzu-segment ()
    (funcall anzu-mode-line-update-function
             anzu--current-position
             anzu--total-matched))
  (telephone-line-defsegment* mjs/popup-segment ()
    (cond ((not (boundp 'popper-popup-status)) "")
          ((eq popper-popup-status 'popup) "POPUP")
          ((eq popper-popup-status 'raised) "RAISED")
          ((eq popper-popup-status 'user-popper) "U POP")))
  (telephone-line-defsegment* mjs/buffer-mod ()
    (let* ((read-only (or buffer-read-only (string-match-p "\\*.*\\*" (buffer-name))))
           (modifed (buffer-modified-p)))
      (concat
       (if read-only
           (propertize "󱙃 "
                       'face `(:inherit mode-line-emphasis
                                        :foreground ,(plist-get base16-stylix-theme-colors :base0A)))
         (if modifed
             (propertize "󰆓 "
                         'face `(:inherit mode-line-emphasis
                                          :foreground ,(plist-get base16-stylix-theme-colors :base08)))
           (propertize "󰆓 "
                       'face `(:inherit mode-line-emphasis
                                        :foreground ,(plist-get base16-stylix-theme-colors :base0C)))))
       (propertize (buffer-name) 'face 'mode-line-emphasis))))
  (telephone-line-defsegment* mjs/flycheck-status ()
    (if (bound-and-true-p flycheck-mode)
        (let* ((flycheck-status (flycheck-count-errors flycheck-current-errors))
               (status (if (eq flycheck-last-status-change 'finished)
                           "󰨮 "
                         "󰃤 "))
               (errors (number-to-string
                        (alist-get 'error flycheck-status 0)))
               (warnings (number-to-string
                          (alist-get 'warning flycheck-status 0)))
               (infos (number-to-string
                       (alist-get 'info flycheck-status 0))))
          (concat status
                  (propertize " " 'face `(:foreground ,(plist-get base16-stylix-theme-colors :base08)))
                  (propertize errors 'face `(:foreground ,(plist-get base16-stylix-theme-colors :base08)))
                  (propertize "  " 'face `(:foreground ,(plist-get base16-stylix-theme-colors :base0A)))
                  (propertize warnings 'face `(:foreground ,(plist-get base16-stylix-theme-colors :base0A)))
                  (propertize "  " 'face `(:foreground ,(plist-get base16-stylix-theme-colors :base0C)))
                  (propertize infos 'face `(:foreground ,(plist-get base16-stylix-theme-colors :base0C)))))
      ""))
  (telephone-line-defsegment* mjs/buffer-position ()
    (cond ((eq major-mode 'pdf-view-mode)
           (let* ((current (pdf-view-current-page))
                  (max (or
                        (ignore-errors
                          (pdf-cache-number-of-pages))
                        " "))
                  (percent (if (stringp max)
                               " "
                             (format "%d%%%% "
                                     (floor (* 100 (/ (float current)
                                                      (float max))))))))
             (format "%s %s/%s" percent current max)))
          ((eq major-mode 'image-mode)
           "%I")
          (t `((-3 "%p")
               ,(concat " %3l:%" (if (bound-and-true-p column-number-indicator-zero-based)
                                     "c" "C"))))))
  (telephone-line-defsegment mjs/minor-mode-segment ()
    `((:propertize ("" minor-mode-alist)
                   mouse-face mode-line-highlight
                   help-echo "Minor mode\n\
        mouse-1: Display minor mode menu\n\
        mouse-2: Show help for minor mode\n\
        mouse-3: Toggle minor modes"
                   local-map ,mode-line-minor-mode-keymap
                   face ,face)
      (:propertize (:eval (when (buffer-narrowed-p) " 󰘢"))
                   mouse-face mode-line-highlight
                   help-echo "mouse-2: Remove narrowing from buffer"
                   local-map ,(make-mode-line-mouse-map
                               'mouse-2 #'mode-line-widen)
                   face ,face)))
  (telephone-line-mode 1))

(use-package anzu
  :after evil
  :hook (emacs-startup . global-anzu-mode)
  :custom ((anzu-mode-lighter "")
           (anzu-cons-mode-line-p nil))
  :custom-face
  (anzu-mode-line ((t  :foreground ,(plist-get base16-stylix-theme-colors :base05)))))

(use-package evil-anzu
  :after evil
  :defer nil)

(setq display-line-numbers-type 'relative
      display-line-numbers-current-absolute t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

(use-package vi-tilde-fringe
  :diminish vi-tilde-fringe-mode
  :hook (emacs-startup . global-vi-tilde-fringe-mode))

(use-package nerd-icons)

(use-package dashboard
  :defer nil
  :config (defun mjs/dashboard-next-items ()
            (unless (and (org-entry-is-todo-p)
                         (not (org-entry-is-done-p))
                         (not (org-in-archived-heading-p))
                         (string= (org-get-todo-state) "NEXT"))
              (point)))
  (dashboard-setup-startup-hook)
  (mjs-leader-def :keymaps 'override
    "D" '("Open Dashboard" . dashboard-open))
  :commands (dashboard-jump-to-agenda dashboard-jump-to-recents)
  :general
  (:keymaps 'dashboard-mode-map :states 'normal
            "a" #'dashboard-jump-to-agenda
            "r" #'dashboard-jump-to-recents)
  :custom
  (dashboard-startup-banner (expand-file-name "logo.webp" user-emacs-directory))
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-navigator t)
  (dashbaord-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-filter-agenda-entry #'mjs/dashboard-next-items)
  (dashboard-items '((recents . 5)
                     (agenda . 10)))
  (dashboard-item-names '(("Recent Files:" . "Recently Opened:")
                          ("Agenda for the coming week:" . "NEXT Items:")))
  (dashboard-footer-icon (nerd-icons-sucicon "nf-custom-emacs"))
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))))

(use-package popper
  :defer nil
  :custom (popper-reference-buffers '("\\*Messages\\*"
                                      "Output\\*$"
                                      "\\*Backtrace\\*"
                                      "\\*Scratch\\*"
                                      "\\*Warnings\\*"
                                      "\\*Async Shell Command\\*"
                                      "\\*Org Agenda\\*"
                                      help-mode
                                      "\\*info\\*"
                                      helpful-mode
                                      compilation-mode
                                      "\\*Embark Actions\\*"
                                      cfw:details-mode
                                      vterm-mode
                                      ))
  (popper-display-control nil)
  (popper-mode-line "")
  (display-buffer-alist '(("\\*Org Links\\*" display-buffer-no-window)
                          ("\\*Org Agenda\\*" (display-buffer-in-direction
                                               display-buffer-pop-up-window
                                               display-buffer-full-frame)
                           (direction . left)
                           (window-min-width 80)
                           (window-width . fit-window-to-buffer-horizontally)
                           (window . root)
                           (dedicated . t))
                          ("\\*compilation\\*" display-buffer-no-window)
                          ("\\*Embark Actions\\*" (display-buffer-at-bottom)
                           (window-height . 14))
                          ((or (major-mode . Info-mode)
                               (major-mode . help-mode)
                               (major-mode . helpful-mode))
                           (display-buffer-reuse-mode-window
                            display-buffer-pop-up-window)
                           (mode . (helpful-mode help-mode Info-mode))
                           (dedicated . t))
                          ("\\*vterm\\*" display-buffer-in-direction
                           (direction . bottom)
                           (window . root)
                           (window-height . 0.3)
                           (dedicated . t)
                           (inhibit-same-window . nil))))
  :config
  (popper-mode +1)
  (require 'popper-echo)
  (popper-echo-mode +1)
  (mjs-leader-def :keymaps 'override
	"p" '(nil :which-key "Popup")
	"p p" '("Toggle Popup" . popper-toggle)
	"t"   '(nil :which-key "Toggle")
	"t p" '("Toggle Popup" . popper-toggle)
	"p c" '("Cycle Popups" . popper-cycle)
	"p m" '("Make Popup" . popper-toggle-type)
	"p k" '("Kill Popup" . popper-kill-latest-popup))
  :config (defun mjs/escape-popups ()
            (if (eq popper-popup-status 'popup)
                (popper-toggle-latest)))
  (advice-add #'evil-force-normal-state :after #'mjs/escape-popups))

(use-package helpful
  :commands (helpful-callable
             helpful-function
             helpful-macro
             helpful-command
             helpful-key
             helpful-variable
             helpful-at-point
             helpful-symbol
             helpful-kill-buffers)
  :init (mjs-leader-def :keymaps 'override
          "h" '(nil :which-key "Help")
          "h f" '("Callable" . helpful-callable)
          "h F" '("Function" . helpful-function)
          "h t" '("Text (Face)" . describe-face)
          "h m" '("Mode" . describe-mode)
          "h M" '("Macro" . helpful-macro)
          "h x" '("Command" . helpful-command)
          "h k" '("Key" . helpful-key)
          "h K" '("Keymap" . describe-keymap)
          "h v" '("Variable" . helpful-variable)
          "h p" '("Thing-at-Point" . helpful-at-point)
          "h s" '("Symbol" . helpful-symbol)
          "h q" '("Kill Help Buffers" . helpful-kill-buffers)))

(use-package vertico
  :defer nil
  :custom (vertico-resize t)
  (vertico-cycle t)
  (enable-recursive-minibuffers t)
  :config  (defun crm-indicator (args)
             (cons (format "[CRM%s] %s"
                           (replace-regexp-in-string
                            "\\`\\[.*?\\*\\|\\[.*?]\\*\\'" ""
                            crm-separator)
                           (car args))
                   (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible f face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (vertico-mode)
  (savehist-mode)
  (general-define-key :keymaps 'vertico-map :states '(insert normal visual motion)
		              "S-RET" #'vertico-exit-input
		              "C-k"   #'vertico-next
		              "C-S-k" #'vertico-next-group
		              "C-j"   #'vertico-previous
		              "C-S-j" #'vertico-previous-group
		              "S-TAB" #'minibuffer-complete
		              ;; More convenient directory navigation commands
		              "RET" #'vertico-directory-enter
		              "DEL" #'vertico-directory-delete-char
		              "S-DEL" #'vertico-directory-delete-word
		              "?"     #'minibuffer-completion-help
		              ;;
		              "ESC" #'keyboard-escape-quit)
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-directory
  :after vertico
  :defer nil
  :ensure nil)

(use-package vertico-reverse
  :after vertico
  :defer nil
  :ensure nil
  :config (vertico-reverse-mode))

(use-package marginalia
  :general (:keymaps 'minibuffer-local-map
                     "M-A" #'marginalia-cycle)
  :defer nil
  :config (marginalia-mode))

(use-package nerd-icons-completion
  :config (nerd-icons-completion-mode))

(use-package orderless
  :defer nil
  :ensure t
  :custom (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :init (recentf-mode 1)
  :general (mjs-leader-def :keymaps 'override
	         "s"     '(nil :which-key "Search")
	         "s b"   '("Buffer" . consult-buffer)
	         "s c"   '("Complex Command" . consult-complex-command)
	         "s e"   '("Compile Error" . consult-compile-error)
	         "s f"   '("Recent Files" . consult-recent-file)
	         "s g"   '(nil :which-key "External Search")
	         "s g g" '("Grep" . consult-grep)
	         "s g r" '("Ripgrep" . consult-ripgrep)
	         "s g f" '("Find" . consult-find)
	         "s g l" '("Locate" . consult-locate)
	         "s h"   '(nil :which-key "Help")
	         "s h i" '("Emacs Info" . consult-info)
	         "s h m" '("UNIX Manual" . consult-man)
	         "s m"   '(nil :which-key "Modes")
	         "s m m" '("Minor Modes" . consult-minor-mode-menu)
	         "s m c" '("Mode Commands" . consult-mode-command)
	         "s M"   '("Macro" . consult-kmacro)
	         "s n"   '(nil :which-key "Navigation")
	         "s n i" '("imenu" . consult-imenu)
	         "s n I" '("Multi-imenu" . consult-imenu-multi)
	         "s n l" '("Goto Line" . consult-goto-line)
	         "s n m" '("Goto Mark" . consult-mark)
	         "s n M" '("Goto Global Mark" . consult-global-mark)
	         "s n o" '("Outline" . consult-outline)
	         "s o"   '(nil :which-key "Org")
	         "s o a" '("Agenda" . consult-org-agenda)
	         "s o h" '("Heading" . consult-org-heading)
	         "s r"   '("Registers" . consult-register)
	         "s s"   '(nil :which-key "Search")
	         "s s l" '("Line" . consult-line)
	         "s s m" '("Multi-buffer line" . consult-line-multi)
	         "s t"   '("Themes" . consult-theme)
	         "s y"   '(nil :which-key "Yank")
	         "s y k" '("Kill Ring" . consult-yank-from-kill-ring)
	         "s y p" '("Pop" . consult-yank-pop)
	         "s y r" '("Replace" . consult-yank-replace)
             "c s" '("Search Symbols" . consult-eglot-symbols))
  :custom (register-preview-function #'consult-register-format)
  (register-preview-delay 0.5)
  :config
  (advice-add #'register-preview :override #'consult-register-window))

(use-package embark
  :commands embark-act embark-dwim embark-bindings
  :init (mjs-leader-def :keymaps 'override
          "e" '(nil :which-key "Embark")
          "e a" '("Embark Act"       . embark-act)
          "e A" '("Embark DWIM"      . embark-dwim)
          "h e" '("Emark Bindings" . embark-bindings))
  :custom (prefix-help-command #'embark-prefix-help-command)
  ;; (embark-prompter #'embark-completing-read-prompter))
  :config (setq embark-indicator 'embark-minimal-indicator))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :hook (emacs-startup . global-corfu-mode)
  :custom (corfu-cycle t)
  (completion-cycle-threshold 3)
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-preselect 'prompt)
  (cape-dict-file (expand-file-name "words.txt" user-emacs-directory))
  (ispell-alternate-dictionary (expand-file-name "words.txt" user-emacs-directory))
  :general (:keymaps 'corfu-map
                     "TAB" #'corfu-next
                     [tab] #'corfu-next
                     "S-TAB" #'corfu-previous
                     [backtab] #'corfu-previous))

(use-package cape
  :after corfu
  :defer nil
  :hook (prog-mode . (lambda ()
                       (add-to-list 'completion-at-point-functions #'cape-keyword)))
  (text-mode . (lambda ()
                 (add-to-list 'completion-at-point-functions #'cape-dict)
                 (add-to-list 'completion-at-point-functions #'cape-dabbrev)))
  (org-mode . (lambda ()
                (add-to-list 'completion-at-point-functions #'cape-elisp-block)))
  :config (add-to-list 'completion-at-point-functions #'cape-file))

(use-package cape-keyword
  :ensure nil
  :commands (cape-keyword))

(use-package company
  :diminish company-mode
  :commands company-dabbrev)

(global-prettify-symbols-mode +1)

(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "󰘧 Emacs Lisp")))
(add-hook 'emacs-lisp-mode-hook #'electric-pair-mode)

(defcustom mjs/indent-bars-inhibit-functions ()
  "A list of predicate functions.

Each function will be run to check if `indent-bars' should be enabled.
If any function returns non-nil, the mode will not be activated."
  :type 'hook
  :group 'mjs/indent-bars)

(use-package indent-bars
  :unless noninteractive
  :hook (((prog-mode conf-mode) . mjs/indent-bars-init-maybe-h)
         (enable-theme-functions . #'indent-bars-reset-styles))
  :init (defun mjs/indent-bars-init-maybe-h ()
          "Enable `indent-bars-mode' depending on `mjs/indent-bars-inhibit-functions'."
          (unless (run-hook-with-args-until-success 'mjs/indent-bars-inhibit-functions)
            (indent-bars-mode +1)))
  :custom ((indent-bars-prefer-character nil)
           (indent-bars-starting-column 0)
           (indent-bars-width-frac 0.15)
           (indent-bars-color-by-depth nil)
           (indent-bars-color '(font-lock-comment-face :face-bg nil :blend 0.425))
           (indent-bars-highlight-current-depth nil)
           (indent-bars-treesit-support nil))
  :config
  ;; Org's virtual indentation messes up indent-bars
  (add-hook 'mjs/indent-bars-inhibit-functions
            (lambda () (bound-and-true-p org-indent-mode)))
  ;; Don't display guides in childframe popups
  (add-hook 'mjs/indent-bars-inhibit-functions
            (lambda () (frame-parameter nil 'parent-frame)))
  (defun mjs/indent-bars-prevent-passing-newline-a (fn col &rest args)
    "The way `indent-bars-display-on-blank-lines' functions, it places text
properties with a display property containing a newline, which confuses
`move-to-column'. This breaks `next-line' and `evil-next-line' without this
advice."
    (if-let* ((indent-bars-mode)
              (indent-bars-display-on-blank-lines)
              (nlp (line-end-position))
              (dprop (get-text-property nlp 'display))
              ((seq-contains-p dprop ?\n))
              ((> col (- nlp (point)))))
        (goto-char nlp)
      (apply fn col args)))
  (advice-add 'move-to-column :around #'mjs/indent-bars-prevent-passing-newline-a)
  (defun mjs/indent-bars-remove-after-lsp-ui-peek-a (&rest _)
    (when (and indent-bars-mode
               (not indent-bars-prefer-character)
               (overlayp lsp-ui-peek--overlay))
      (save-excursion
        (let ((indent-bars--display-function #'ignore)
              (indent-bars--display-blank-lines-function #'ignore))
          (indent-bars--fontify (overlay-start lsp-ui-peek--overlay)
                                (1+ (overlay-end lsp-ui-peek--overlay))
                                nil)))))
  (advice-add 'lsp-ui-peek--peek-now :after #'mjs/indent-bars-remove-after-lsp-ui-peek-a)
  (defun mjs/indent-bars-restore-after-lsp-ui-peek-a (&rest _)
    (unless indent-bars-prefer-character (indent-bars-setup)))
  (advice-add 'lsp-ui-peek--peek-hide :after #'mjs/indent-bars-restore-after-lsp-ui-peek-a))

;; STYLE: Redefine fringe bitmaps to be sleeker by making them solid bars
;; without borders that only take up half the horizontal space in the fringe.
;; This approach lets us avoid robbing fringe space from other packages/modes
;; that may need it like magit, flycheck, etc.
(if (fboundp 'fringe-mode) (fringe-mode '8))
(setq-default fringes-outside-margins t)

(use-package diff-hl-flydiff
  :ensure nil
  :commands diff-hl-flydiff-mode)

(use-package diff-hl
  :defer nil
  :commands diff-hl-stage-current-hunk diff-hl-revert-hunk diff-hl-next-hunk diff-hl-previous-hunk
  :custom-face (diff-hl-insert ((t (:background "unspecified"))))
  (diff-hl-delete ((t (:background "unspecified"))))
  (diff-hl-change ((t (:background "unspecified"))))
  :custom ((diff-hl-draw-borders nil)
           (diff-hl-fringe-bmp-function #'mjs/diff-hl-type-at-pos-fn)
           (diff-hl-global-modes '(not image-mode pdf-view-mode))
           (vc-git-diff-switches '("--histogram"))
           (diff-hl-flydiff-delay 0.5)
           (diff-hl-update-async t)
           (diff-hl-show-stages-changes nil))
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :hook (vc-dir-mode . turn-on-diff-hl-mode)
  :hook (dired-mode . mjs/diff-hl-enable-maybe-h)
  :hook (diff-hl-flydiff-mode . mjs/diff-hl-init-flydiff-mode-h)
  :init
  (let* ((width 2)
         (bitmap (vector (1- (expt 2 width)))))
    (define-fringe-bitmap 'mjs/diff-hl-bitmap bitmap 1 width '(top t))
    (define-fringe-bitmap 'mjs/diff-hl-delete-bitmap
      [216 108 54 27 54 108 216] nil nil 'center))
  (defun mjs/diff-hl-type-at-pos-fn (type _pos)
    (if (eq type 'delete)
        'mjs/diff-hl-delete-bitmap
      'mjs/diff-hl-bitmap))
  (defun mjs/diff-hl-enable-maybe-h ()
    "Conditionally enable `diff-hl-dired-mode' in dired buffers.
Respects `diff-hl-disable-on-remote'."
    (unless (and (bound-and-true-p diff-hl-disable-on-remote)
                 (file-remote-p default-directory))
      (diff-hl-dired-mode +1)))
  :config
  (defun mjs/diff-hl-save-excursion-a (fn &rest args)
    "Suppresses unexpected cursor movement by `diff-hl-revert-hunk'."
    (let ((pt (point)))
      (prog1 (apply fn args)
        (goto-char pt))))
  (advice-add 'diff-hl-revert-hunk :around #'mjs/diff-hl-save-excursion-a)
  (defun mjs/diff-hl-init-flydiff-mode-h ()
    (if diff-hl-flydiff-mode
        (add-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update)
      (remove-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update)))
  (defun mjs/diff-hl-shrink-popup-a (fn &rest args)
    "The revert popup consumes 50% of the frame, regardless of the size of the
reversion. This resizes the popup to match its contents."
    (cl-letf* ((refine-mode diff-auto-refine-mode)
               (diff-auto-refine-mode t)
               (diff-refine-hunk (lambda ()
                                   (when refine-mode
                                     (funcall diff-refine-hunk))
                                   (shrink-window-if-larger-than-buffer))))
      (apply fn args)))
  (advice-add 'diff-hl-revert-hunk-1 :around #'mjs/diff-hl-shrink-popup-a)
  (defun mjs/diff-hl-kill-thread (&optional block?)
    (when-let ((th mjs/diff-hl-thread))
      (when (thread-live-p th)
        (thread-signal th 'quit nil)
        (when block?
          (conditional-case _
                            (thread-join th)
                            ((quit error) nil))))))
  (defvar-local mjs/diff-hl-thread nil)
  (defun mjs/diff-hl-debounce-threads-a (&rest _)
    (unless (or inhibit-redisplay
                non-essential
                delay-mode-hooks
                (null (buffer-file-name (buffer-base-buffer)))
                (null (get-buffer-window (current-buffer))))
      (if (and diff-hl-update-async
               (not
                (run-hook-with-args-until-success 'diff-hl-async-inhibit-functions
                                                  default-directory)))
          (progn
            (mjs/diff-hl-kill-thread)
            (setq mjs/diff-hl-thread
                  (make-thread (lambda ()
                                 (unwind-protect
                                     (diff-hl--update-safe)
                                   (setq mjs/diff-hl-thread nil)))
                               "diff-hl--update-safe")))
        (diff-hl--update))
      t))
  (advice-add 'diff-hl-update :override #'mjs/diff-hl-debounce-threads-a)
  ;; HACK: This advice won't work in *all* cases since it's a C function and any call
  ;; from C doesn't trigger advice, but the thread issues are typically elisp calls.
  (defun mjs/diff-hl-kill-diff-hl-thread-a (&optional buf)
    (when-let ((buf (ignore-errors (window-normalize-buffer))))
      (with-current-buffer buf
        (mjs/diff-hl-kill-thread t))))
  (advice-add 'kill-buffer :before #'mjs/diff-hl-kill-diff-hl-thread-a)
  (global-diff-hl-mode))

(use-package lispy
  :diminish "'󰅲 "
  :init (mjs-local-leader-def :keymaps 'emacs-lisp-mode-map
          "l" '("Lispy" . lispy-mode)))

(use-package macrostep
  :commands marcostep-expand
  :init (mjs-local-leader-def :states '(normal insert visual motion)
          :keymaps 'emacs-lisp-mode-map
          "e" '("Expand Macro" . macrostep-expand)))

(use-package elisp-def
  :commands elisp-def
  :init (mjs-local-leader-def :states '(normal insert visual motion)
          :keymaps 'emacs-lisp-mode-map
          "d" '("Find Definition" . elisp-def)))

(use-package elisp-demos
  :config (advice-add 'helpful-update
                      :after #'elisp-demos-advice-helpful-update))

(use-package highlight-quoted
  :diminish highlight-quoted-mode
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :custom (eldoc-minor-mode-string ""))

(mjs-leader-def :keymaps 'override
  "a"     '("Agenda" . org-agenda)
  "A"     '("GTD Agenda" . (lambda () (interactive) (org-agenda nil "g")))
  "c"     '(nil :which-key "Capture")
  "c c"   '("Org Capture" . org-capture)
  "c r"   '("Roam Capture" . mjs/org-roam-capture)
  "c f"   '("Finish Capture" . org-capture-finalize)
  "c k"   '("Abort Capture" . org-capture-kill)
  "c r"   '("Refile Capture" . org-capture-refile)
  "n"     '(nil :which-key "Notes")
  "n a"   '("Agenda" . org-agenda)
  "n c"   '(nil :which-key "Contexts")
  "n c c" '("Set Context" . mjs/org-auto-tags--set-by-context)
  "n c i" '("Inspect Tags" . (lambda ()
                               (interactive)
                               (message "%s"
                                        (mapconcat (lambda (tag)
                                                     (concat "#" tag))
                                                   mjs/org-auto-tags--current-list
                                                   " "))))
  "n c s" '("Change Tags" . mjs/org-auto-tags--set)
  "n C"   '("GOTO Clock" . org-clock-goto)
  "n l"   '("Store Link" . org-store-link)
  "n R"   '("Refile DWIM" . mjs/org-refile-dwim)
  "n s"   '("Search Notes" . org-search-view)
  "n t"   '("TODO List" . org-todo-list)
  "n T"   '("Tag View" . org-tags-view))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-emphasis-alist
   '(("*" bold) ("/" italic) ("_" underline) ("=" org-verbatim verbatim)
     ("~" org-code verbatim) ("+" (:strike-through t)) ("!" (:overline t))))
 '(safe-local-variable-directories '("/home/mjs/workspace/pollux/"
                                     "/home/mjs/workspace/pollux-report/"
                                     "/home/mjs/workspace/everparse/"))
 '(safe-local-variable-values
   '((mjs/org-auto-tags--current-list quote ("great_basin"))
     (mjs/org-auto-tags--current-list "great_basin")
     (mjs/org-auto-tags--current-list "kb"))))

(use-package org
  ;; For whatever reason, `org' gets upset if these aren't defined soon enough
  :init (setq org-directory "~/Documents/")
  :general (:states 'normal :keymaps 'org-mode-map "RET" #'mjs/org-dwim-at-point)
  (:states 'insert :keymaps 'org-mode-map "RET" #'mjs/org-return)
  ;; (:states 'normal :keymaps calendar-mode-map "RET" #'org-calendar-select)
  (:states '(normal insert)
           :keymaps '(org-mode-map evil-org-mode-map)
           "C-<return>"   #'mjs/org-insert-item-below
           "C-S-<return>" #'mjs/org-insert-item-above)
  (mjs-leader-def :keymaps 'org-mode-map
    "f M" '("Move File & Update Links" . mjs/move-and-update-file-links))
  (mjs-local-leader-def :states '(normal insert visual motion) :keymaps 'org-mode-map
    "a"      '("Archive" . org-archive-subtree)
    "A"      '("Attach" . org-attach)
    "b"      '(nil :which-key "Tables")
    "b a"    '("Align" . org-table-align)
    "b b"    '("Blank" . org-table-blank-field)
    "b c"    '("Convert to Table" . org-table-create-or-convert-from-region)
    "b d"    '(nil :which-key "Delete")
    "b d c"  '("Delete Column" . org-table-delete-column)
    "b d r"  '("Delere Row" . org-table-kill-row)
    "b e"    '("Edit" . org-table-edit-field)
    "b f"    '("Edit Formulas" . org-table-edit-formulas)
    "b h"    '("Help" . org-table-field-info)
    "b i"    '(nil :which-key "Insert")
    "b i c"  '("Insert Column" . org-table-insert-column)
    "b i r"  '("Insert Row" . org-table-insert-row)
    "b i h"  '("Insert Hline" . org-table-insert-hline)
    "b i H"  '("Insert Hline & Move" . org-table-hline-and-move)
    "b s"    '("Sort Rows" . org-table-sort-lines)
    "b r"    '("Recalculate Formulas" . org-table-recalculate)
    "b R"    '("Recalculate All Tables" . org-table-recalculate-buffer-tables)
    "b t"    '("Toggle Table.el" . org-table-create-with-table.el)
    "b -"    '("Insert Hline" . org-table-insert-hline)
    "B"      '(nil :which-key "Babel")
    "B t"    '("Tangle" . org-babel-tangle)
    "B e"    '("Execute Block" . org-babel-execute-src-block)
    "B E"    '("Execute Buffer" . org-babel-execute-buffer)
    "B h"    '("Hide Result" . org-babel-hide-result-toggle)
    "B H"    '("Hide All Results" . org-babel-result-hide-all)
    "B k"    '("Remove Result" . org-babel-remove-result)
    "B K"    '("Remove All Results" . mjs/org-babel-remove-result-blocks)
    "B n"    '("Next Src Block" . org-babel-next-src-block)
    "B p"    '("Pervious Src Block" . org-babel-previous-src-block)
    "c"      '(nil :which-key "Capture")
    "c c"    '("Org Capture" . org-capture)
    "c r"    '("Roam Capture" . mjs/org-roam-capture)
    "c f"    '("Finish Capture" . org-capture-finalize)
    "c k"    '("Abort Capture" . org-capture-kill)
    "c r"    '("Refile Capture" . org-capture-refile)
    "f"      '(nil :which-key "File Links")
    "f m"    '("Move File" . mjs/move-and-update-file-links)
    "f d"    '("Move Directory" . mjs/move-dir-update-link-links)
    "f r"    '("Regenerate Links" . mjs/regenerate-file-links)
    "f R"    '("Regenerate Links Globally" . mjs/regenerate-file-links-globally)
    "h"      '(nil :which-key "Heading")
    "h b"    '("Mark HOLD" . (lambda () (interactive) (org-todo "HOLD")))
    "h d"    '("Mark DONE" . (lambda () (interactive) (org-todo "DONE")))
    "h H"    '("Toggle Heading" . org-toggle-heading)
    "h h"    '("Promote Heading" . org-metaleft)
    "h k"    '("Mark KILL" . (lambda () (interactive) (org-todo "KILL")))
    "h l"    '("Demote Heading" . org-metaright)
    "h n"    '("Mark NEXT" . (lambda () (interactive) (org-todo "NEXT")))
    "h s"    '("Set Heading State" . org-todo)
    "h t"    '("Set Heading Tags" . org-set-tags-command)
    "h T"    '("Mark TODO" . (lambda () (interactive) (org-todo "TODO")))
    "i"      '(nil :which-key "ID")
    "i c"    '("Copy ID" . org-id-copy)
    "i i"    '("Create ID" . org-id-get-create)
    "i g"    '("Goto ID" . org-id-goto)
    "i u"    '("Update IDs" . org-id-update-id-locations)
    "I"      '("Create ID" . org-id-get-create)
    "l"      '(nil :which-key "Links")
    "l i"    '("Store ID Link" . org-id-store-link)
    "l l"    '("Insert Link" . org-insert-link)
    "l L"    '("Insert All Links" . org-insert-all-links)
    "l t"    '("Toggle Links" . org-toggle-link-display)
    "l s"    '("Store Link" . org-store-link)
    "l S"    '("Insert Stored Link" . org-insert-last-stored-link)
    "r"      '("Refile" . org-refile)
    "R"      '("Refile DWIM" . mjs/org-refile-dwim)
    "s"      '("Search Headings" . consult-org-heading)
    "S"      '(nil :which-key "Subtree")
    "S a"    '("Toggle Archive Tag" . org-toggle-archive-tag)
    "S A"    '("Archive" . org-archive-subtree)
    "S b"    '("Move to Buffer" . org-tree-to-indirect-buffer)
    "S c"    '("Clone" . org-clone-subtree-with-time-shift)
    "S d"    '("Delete" . org-cut-subtree)
    "S h"    '("Promote" . org-promote-subtree)
    "S j"    '("Move Down" . org-move-subtree-down)
    "S k"    '("Move Up" . org-move-subtree-up)
    "S l"    '("Demote" . org-demote-subtree)
    "S n"    '("Narrow to Subtree" . org-narrow-to-subtree)
    "S N"    '("Widen" . widen)
    "S r"    '("Refile" . org-refile)
    "S s"    '("Sparse Subtree" . org-sparse-tree)
    "S S"    '("Sort" . org-sort)
    "t"      '(nil :which-key "Time")
    "t d"    '("Deadline" . org-deadline)
    "t s"    '("Schedule" . org-schedule)
    "t t"    '("Time Stamp" . org-time-stamp)
    "t T"    '("Inactive Time Stamp" . org-time-stamp-inactive)
    "t e"    '("Set Effort" . org-set-effort)
    "t E"    '("Increase Effort" . org-inc-effort)
    "t i"    '("Clock-in" . org-clock-in)
    "t o"    '("Clock-out" . org-clock-out)
    "t g"    '("Goto Current Clock" . org-clock-goto)
    "t c"    '("Cancel Clock" . org-clock-cancel)
    "t r"    '("Report" . org-clock-report)
    "T"      '(nil :which-key "Toggle")
    "T s"    '("Toggle Sub/Superscripts" . (lambda ()
                                             (interactive)
                                             (setq org-pretty-entities-include-sub-superscripts
                                                   (not org-pretty-entities-include-sub-superscripts)))))
  :custom ((org-fontify-quote-and-verse-blocks t)
           (org-src-fontify-natively t)
           (org-pretty-entities t)
           (org-highlight-latex-and-related '(native latex))
           (org-ellipsis " ▾")
           (tab-always-indent nil)
           (org-startup-with-inline-images t)
           (org-startup-indented t)
           (org-image-actual-width 600)
           (org-startup-align-all-tables t)
           (org-startup-folded 'showall)
           (org-startup-with-latex-preview t)
           (org-preview-latex-default-process 'dvisvgm)
           (org-tag-persistent-alist
            '((:startgroup)
              ("kb")
              ("ttrpg")
              (:endgroup)
              ("merge")
              (:startgrouptag)
              ("ttrpg")
              (:grouptags)
			  ;; Specific games
              ("great_basin")
              ("etera")
              ("obscured_realms")
              ("graves")
              ("madttrpg")
			  ;; Generic ttrpg types
              ("character")
              ("event")
              ("faction")
              ("location")
              ("object")
              ("session")
              ("stat")
              (:endgrouptag)))
           (org-agenda-start-with-log-mode t)
           (org-log-done 'time)
           (org-log-into-drawer t)
           (org-treat-insert-todo-heading-as-state-change t)
           (org-agenda-hide-tags-regexp ".")
           (org-agenda-files (list (concat org-directory "agenda/")))
           (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")))
           (org-agenda-custom-commands
            '(("r" "Weekly Review"
               ((agenda "" ((org-agenda-span 7)))
                (tags "TODO=\"DONE\"&CLOSED>=\"<-1w>\""
                      ((org-agenda-overriding-header "\nCompleted This Week\n")))))
              ("g" "Get Things Done"
               ((agenda* ""
                         ((org-agenda-log-mode-items '(closed clock))
                          (org-deadline-warning-days 0)
                          (org-agenda-span 1)))
                (tags-todo "DEADLINE<=\"<+14d>\""
                           ((org-agenda-entry-types '(:deadline))
                            (org-agenda-prefix-format " %i %-12:c [%(mjs/agenda-time-format 'deadline)] ")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notregexp "\\*+ \\(NEXT\\|TODO\\)"))
                            (org-agenda-overriding-header "\nDeadlines\n")))
                (tags "SCHEDULED<=\"<today>\"-TODO=\"DONE\"-TODO=\"KILLED\"-STYLE=\"habit\""
                      ((org-agenda-overriding-header "\nScheduled\n")
                       (org-agenda-prefix-format " %i %-12:c [%(mjs/agenda-time-format 'scheduled)] ")))
                (tags "STYLE=\"habit\"" ((org-agenda-overriding-header "\nHabits\n")))
                (todo "NEXT"
                      ((org-agenda-prefix-format " %i %-12:c [%e] ")
                       (org-agenda-overriding-header "\nTasks\n")))
                (tags-todo "inbox"
                           ((org-agenda-prefix-format " %?-12t% s")
                            (org-agenda-overriding-header "\nInbox\n")))
                (tags "CLOSED>=\"<today>\""
                      ((org-agenda-overriding-header "\nCompleted today\n")))))))
           (org-refile-targets '((org-agenda-files
                                  :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))
           (org-refile-use-outline-path 'file)
           (org-outline-path-complete-in-steps nil)
           (org-entities-user
            '(("mathcalC" "\\mathcal{C}" nil "&x1D49E" "C" "C" "𝒞")
              ("mathbbE" "\\mathbb{E}" nil "&x1D53C" "E" "E" "𝔼")
              ("mathcalF" "\\mathcal{F}" nil "&x1D4D5" "F" "F" "𝓕")
              ("mathbbN" "\\mathbb{N}" nil "&x2115" "N" "N" "ℕ")
              ("mathcalO" "\\mathcal{O}" nil "&x1D4AA" "O" "O" "𝒪")
              ("mathbbP" "\\mathbb{P}" nil "&x2119" "P" "P" "ℙ")
              ("mathbbR" "\\mathbb{R}" nil "&x211D" "R" "R" "ℝ")
              ("subsetneqq" "\\subsetneqq" nil "&x2ACB" "subsetneqq" "subsetneqq" "⫋")
              ("supseteq" "\\supseteq" nil "&x2287" "supseteq" "supseteq" "⊇")
              ("precsim" "\\precsim" nil "&x227E" "<" "<" "≾")
              ("bot" "\\bot" nil "&x22A5" "_|_" "_|_" "⊥")
              ("top" "\\top" nil "&x22A4" "T" "T" "⊤")
              ("lightning" "\\lightning" nil "&x21AF" "</" "</" "↯")
              ("qed" "\\qedsymbol" nil "&x25A1" "[]" "[]" "☐")))
           (org-confirm-babel-evaluate nil))
  :diminish ((org-cdlatex-mode . " ")
             (auto-fill-function . "")
             (variable-pitch-mode . ""))
  :hook ((org-mode . turn-on-org-cdlatex)
         (org-mode . (lambda () (setq mode-name " Org")))
         (org-agenda-mode . (lambda () (setq mode-name "󰃮 Agenda")))
         (org-mode . auto-fill-mode)
         (org-after-todo-state-change . log-todo-next-creation-date)
         (org-capture-mode . mjs/org-capture-update-header)
         (org-capture-mode . (lambda () (flycheck-mode -1)))
         (org-capture-mode . (lambda () (require 'diminish)
                               (diminish 'org-capture-mode " 󰄀")))
         ;; Automatically set the scale depending on the scale of the monitor. Is is so that on `luna'
         ;; the fragments are rendered at the correct size and not huge.
         (org-mode . (lambda ()
                       (plist-put org-format-latex-options
                                  :scale (if-let ((scale (alist-get 'scale-factor
                                                                    (car (display-monitor-attributes-list)))))
                                             (/ 1 scale)
                                           1.0))))

         (text-scale-mode . (lambda ()
                              (if (and text-scale-mode
                                       (eq major-mode 'org-mode))
                                  (mjs/resize-org-latex-overlays)
                                (if (eq major-mode 'org-mode)
                                    (mjs/resize-org-latex-overlays)))))

         (org-indent-mode .  (lambda () (diminish 'org-indent-mode))))
  :config
  (setq org-capture-templates
        `(("c" "Class Lecture" plain
           (function (lambda () (mjs/class-capture)))
           ,(concat "#+filetags: :%(format mjs--capture-title):\n"
                    "#+title: %(format mjs--capture-title) (%<%d %B %Y>)\n"
                    "#+author: %(user-full-name)\n\n%?")
           :jump-to-captured t
           :immediate-finish t)
          ("C" "New Contact" entry
           (file+headline ,(concat org-directory "contacts.org") "Other")
           ,(concat
             "** %(org-contacts-template-name)\n"
             ":PROPERTIES:\n"
             ":ADDRESS: %^{Address?}\n"
             ":BIRTHDAY: %^{yyyy-mm-dd}\n"
             ":EMAIL: %(org-contacts-template-email)\n"
             ":NOTE: %?\n"
             ":END:")
           :empty-lines 1)
          ("a" "Daily Log (AM)" entry
           (file+olp+datetree ,(format-time-string "classes/log/%Y/%m-%B-log.org"))
           ,(concat "* Planning\n\n"
                    "- [ ] Record Habits\n"
                    "- [ ] %?\n")
           :empty-lines 1
           :tree-type week
           :jump-to-captured t
           :immediate-finish t)
          ("p" "Daily Log (PM)" entry
           (file+olp+datetree ,(format-time-string "classes/log/%Y/%m-%B-log.org"))
           ,(concat "* Review\n\n"
                    "- [ ] Update completed tasks\n\n%?")
           :empty-lines 1
           :tree-type week
           :jump-to-captured t
           :immediate-finish t)
          ("e" "Etera Session" entry
           (file "freetime/ttrpg/games/etera/notes.org")
           "* Session %<%Y-%m-%d>\n\n%?"
           :empty-lines 1
           :jump-to-captured t
           :immediate-finish t)
          ("g" "Graves Session" entry
           (file "freetime/ttrpg/games/graves-and-groves/sessions.org")
           "* Session %<%Y-%m-%d>\n\n%?"
           :empty-lines 1
           :prepend t
           :jump-to-captured t
           :immediate-finish t)
          ("i" "Inbox" entry
           (file "agenda/inbox.org")
           ,(concat "* TODO %?\n"
                    "/Entered on/ %U")
           :empty-lines 1
           :prepend t)
          ("m" "Meeting" entry
           (file+headline "agenda/agenda.org" "Future")
           ,(concat "* %? :meeting:\n"
                    "SCHEDULED: %^{Meeting Time}T"))
          ("n" "Meeting Notes" entry
           (file "agenda/notes.org")
           ,(concat "* Notes (%a)\n"
                    "/Entered on/ %U\n\n%?"))
          ("o" "Obscured Realms Session" entry
           (file "freetime/ttrpg/games/obscured-realms/sessions.org")
           "* Session %<%Y-%m-%d>\n\n%?"
           :empty-lines 1
           :jump-to-captured t
           :immediate-finish t)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (dot . t)
     (emacs-lisp . t)
     (latex . t)
     (mermaid . t)
     (python . t)
     (R . t)
     (java . t)
     (rust . t)))

  (add-to-list 'org-modules 'org-habit t)
  (add-to-list 'org-modules 'org-tempo t)
  (advice-add #'org-agenda-finalize :before #'mjs/org-agenda-mark-habits)

  ;; Save the buffers after refile
  (advice-add #'org-refile :after #'org-save-all-org-buffers)

  (defun mjs/org-fix-newline-and-indent (&optional indent _arg _interactive)
    "Mimic `newline-and-indent' in src blocks w/ lang-appropriate indentation."
    (when (and org-src-tab-acts-natively (org-in-src-block-p t))
      (save-window-excursion
        (org-babel-do-in-edit-buffer
         (call-interactively #'indent-for-tab-command)))))

  (advice-add #'org-return :after #'mjs/org-fix-newline-and-indent)

  (add-to-list 'org-latex-packages-alist '("" "sfmath" t))
  (add-to-list 'org-latex-packages-alist '("margin=1in" "geometry" t))
  (add-to-list 'org-latex-packages-alist '("" "parskip" t))
  (add-to-list 'org-latex-packages-alist '("" "mathpartir" t))
  (add-to-list 'org-latex-packages-alist '("" "nicematrix" t))
  (add-to-list 'org-latex-packages-alist '("" "amsthm" t))
  (add-to-list 'org-latex-packages-alist '("" "cancel" t))

  ;; consult-org is not being loaded by default
  (require 'consult-org))


(use-package org-habit
  :ensure nil
  :config
  (set-face-foreground 'org-habit-ready-face (plist-get base16-stylix-theme-colors :base01))
  (set-face-background 'org-habit-ready-face (plist-get base16-stylix-theme-colors :base0B))
  (set-face-background 'org-habit-ready-future-face (plist-get base16-stylix-theme-colors :base0B))
  (set-face-background 'org-habit-clear-face (plist-get base16-stylix-theme-colors :base0D))
  (set-face-background 'org-habit-clear-future-face (plist-get base16-stylix-theme-colors :base0D))
  (set-face-background 'org-habit-alert-face (plist-get base16-stylix-theme-colors :base0A))
  (set-face-background 'org-habit-alert-future-face (plist-get base16-stylix-theme-colors :base0A))
  (set-face-background 'org-habit-overdue-face (plist-get base16-stylix-theme-colors :base08))
  (set-face-background 'org-habit-overdue-future-face (plist-get base16-stylix-theme-colors :base08)))

(use-package ob-rust
  :after org)

(use-package ob-mermaid
  :after org)

(use-package org-tempo
  :ensure nil
  :after org
  :defer nil
  :custom (org-structure-template-alist '(("t" . "LaTeX latex")
                                          ("j" . "src java")
                                          ("R" . "src R")
                                          ("r" . "src rust")
                                          ("p" . "src python")
                                          ("sj" . "src javascript")
                                          ("sh" . "src haskell")
                                          ("st" . "src latex")
                                          ("el" . "src emacs-lisp")
                                          ("cp" . "src cpp")
                                          ("sC" . "src C")
                                          ("a" . "export ascii")
                                          ("c" . "center")
                                          ("C" . "comment")
                                          ("e" . "example")
                                          ("E" . "export")
                                          ("h" . "export html")
                                          ("l" . "export latex")
                                          ("q" . "quote")
                                          ("s" . "src")
                                          ("v" . "verse"))))

(use-package eros
  :after org
  :hook (org-mode . eros-mode))

(use-package org-contacts
  :after org
  :custom (org-contacts-files
           (list (concat org-directory "contacts.org"))))

(use-package org-fragtog
  :commands org-fragtog-mode
  :hook (org-mode . (lambda ()
                      (add-hook 'evil-insert-state-entry-hook
                                (lambda ()
                                  (when (eq major-mode 'org-mode)
                                    (org-fragtog-mode +1))))
                      (add-hook 'evil-insert-state-exit-hook
                                (lambda ()
                                  (when (eq major-mode 'org-mode)
                                    (progn
                                      (org-fragtog-mode -1)
                                      (if (org-inside-LaTeX-fragment-p)
                                          (org-latex-preview)))))))))

(use-package org-appear
  :after org
  :defer nil
  :custom (org-hide-emphasis-markers t)
  (org-appear-autolinks t)
  (org-appear-trigger 'manual)
  :hook (org-mode . (lambda ()
                      (org-appear-mode t)
                      (add-hook 'evil-insert-state-entry-hook
                                #'org-appear-manual-start nil t)
                      (add-hook 'evil-insert-state-exit-hook
                                #'org-appear-manual-stop nil t))))

(use-package olivetti
  :custom (olivetti-body-width 100)
  (olivetti-lighter " 󰘞")
  :hook (org-mode . olivetti-mode)
  :init (diminish 'visual-line-mode)
  (mjs-local-leader-def :keymaps 'org-mode-map
    "o" '("Toggle Olivetti" . olivetti-mode)))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :custom ((org-modern-star nil)
           (org-modern-hide-stars nil))
  :config (set-face-attribute 'org-modern-done nil
                              :background (plist-get base16-stylix-theme-colors :base02)
                              :foreground (plist-get base16-stylix-theme-colors :base05)))

(use-package org-modern-indent
  :ensure nil
  :after org-modern
  :hook (org-modern-mode . org-modern-indent-mode))

(use-package org-pomodoro
  :after org
  :custom ((org-pomodoro-format " %s")
           (org-pomodoro-short-break-format "󱁕 %s")
           (org-pomodoro-long-break-format "󱁕 %s")
           (org-pomodoro-start-sound-p t)
           (org-pomodoro-audio-player "play"))
  :custom-face (org-pomodoro-mode-line-break ((t (:foreground
                                                  ,(plist-get base16-stylix-theme-colors :base0B)))))
  :general (mjs-local-leader-def :keymaps 'org-mode-map
             "t p" '("Pomodoro" . org-pomodoro))
  (mjs-leader-def :keymaps 'override
    "n p" '("Pomodoro" . org-pomodoro)))

(use-package flycheck
  :hook (emacs-startup . global-flycheck-mode)
  :diminish flycheck-mode
  :defer nil
  :custom ((flycheck-global-modes t)
           (flycheck-indication-mode 'right-fringe)
           (flycheck-mode-line))
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

(use-package flycheck-ledger
  :after (flycheck ledger)
  :hook (ledger-mode . (lambda () (require 'flycheck-ledger))))

(use-package jinx
  :diminish " 󰓆"
  :hook (emacs-startup . global-jinx-mode)
  :general (:states '(normal visual) :keymaps 'jinx-mode-map
                    "z =" #'jinx-correct
                    "Z =" #'jinx-languages)
  (:keymaps 'evil-motion-state-map
            "[ s" #'jinx-previous
            "] s" #'jinx-next))

(use-package org-superstar
  :after org
  :custom ((org-superstar-leading-bullet " ")
           (org-hide-leading-stars nil)
           (org-superstar-remove-leading-stars nil))
  :hook (org-mode . org-superstar-mode)
  :config
  (defun mjs/org-indent-compute-prefixes ()
    "Compute prefix strings for regular text and headlines.

This is taken from the `org-indent' source code, but I've changed
the characters."
    (setq org-indent--heading-line-prefixes
          (make-vector org-indent--deepest-level nil))
    (setq org-indent--inlinetask-line-prefixes
          (make-vector org-indent--deepest-level nil))
    (setq org-indent--text-line-prefixes
          (make-vector org-indent--deepest-level nil))
    (when (> org-indent-indentation-per-level 0)
      (dotimes (n org-indent--deepest-level)
        (let ((indentation (if (<= n 1) 0
                             (* (1- org-indent-indentation-per-level)
                                (1- n)))))
          ;; Headlines line prefixes.
          ;; (let ((heading-prefix (make-string indentation ?·)))
          (let ((heading-prefix (make-string indentation 32)))
            (aset org-indent--heading-line-prefixes
                  n
                  (org-add-props heading-prefix nil 'face 'org-indent))
            ;; Inline tasks line prefixes
            (aset org-indent--inlinetask-line-prefixes
                  n
                  (cond ((<= n 1) "")
                        ((bound-and-true-p org-inlinetask-show-first-star)
                         (concat org-indent-inlinetask-first-star
                                 (substring heading-prefix 1)))
                        (t (org-add-props heading-prefix nil 'face 'org-indent)))))
          ;; Text line prefixes.
          (aset org-indent--text-line-prefixes
                n
                (org-add-props
                    (concat (make-string (+ n indentation) ?\s)
                            (and (> n 0)
                                 (char-to-string org-indent-boundary-char)))
                    nil 'face 'org-indent))))))
  (advice-add 'org-indent--compute-prefixes :override #'mjs/org-indent-compute-prefixes))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :diminish (pdf-view-themed-minor-mode . " ")
  :custom ((pdf-view-display-size 'fit-page)
           (pdf-view-use-scaling t)
           (pdf-view-use-imagemagick nil))
  :hook ((pdf-view-mode . (lambda () (setq buffer-read-only nil)
                            (mjs-local-leader-def :keymaps 'pdf-view-mode-map
                              "a"   '(nil :which-key "Annotations")
                              "a a" '("Add Annotation" . pdf-annot-add-markup-annotation)
                              "a d" '("Delete Annotation" . pdf-annot-delete)
                              "a h" '("Add Highlight Annotation" . pdf-annot-add-highlight-markup-annotation)
                              "a l" '("List Annotations" . pdf-annot-list-annotations)
                              "a s" '("Add Squiggly Annotation" . pdf-annot-add-squiggly-markup-annotation)
                              "a t" '("Add Text Annotation" . pdf-annot-add-text-annotation)
                              "a u" '("Add Underline Annotation" . pdf-annot-add-underline-markup-annotation)
                              "a -" '("Add Strike-through Annotation" . pdf-annot-add-strikeout-markup-annotation)
                              "d" '("Toggle Dark Mode" . pdf-view-themed-minor-mode)
                              "o" '("Outline" . pdf-outline)
                              "p" '("Goto Page" . pdf-view-goto-page))
                            (mjs-local-leader-def :keymaps 'pdf-annot-edit-contents-minor-mode-map
                              "P"   '(nil :which-key "PDF Annotations")
                              "P f" '("Finalize Annotation" . (lambda () (interactive) (pdf-annot-edit-contents-finalize t t)))
                              "P k" '("Kill Annotation" . pdf-annot-edit-contents-abort)
                              "P s" '("Save Annotation" . pdf-annot-edit-contents-commit))))
         (pdf-view-mode . (lambda ()
                            (set (make-local-variable 'evil-default-cursor) (list nil))
                            (pdf-view-themed-minor-mode)))
         (pdf-view-mode . (lambda ()
                            ;; disable triggering visual mode on selection in PDFView buffers
                            (add-hook 'evil-local-mode-hook
                                      (lambda () (remove-hook
                                                  'activate-mark-hook
                                                  'evil-visual-activate-hook
                                                  t))
                                      nil t)
                            ;; implement yank ourselves
                            (evil-define-key 'normal pdf-view-mode-map
                              "y" #'mjs/pdf-view-evil-yank-visual
                              ))))
  :config
  (add-to-list 'evil-normal-state-modes 'pdf-view-mode)
  ;; Silence large file prompts for PDFs
  (defun mjs/suppress-large-file-prompts (fn size op-type filename
                                             &optional offer-raw)
    (unless (string-match-p "\\.pdf\\'" filename)
      (funcall fn size op-type filename offer-raw)))
  (advice-add #'abort-if-file-too-large :around
              #'mjs/suppress-large-file-prompts)
  ;; Add message about needing to install the pdf server.
  (defun mjs/install-epdfinfo-message (fn &rest args)
    (if (and (require 'pdf-info nil t)
             (or (pdf-info-running-p)
                 (ignore-errors (pdf-info-check-epdfinfo) t)))
        (apply fn args)
      (fundamental-mode)
      (message "Viewing PDFs in Emacs requires epdfinfo. Use `M-x pdf-tools-install' to build it")))
  (advice-add #'pdf-view-mode :around #'mjs/install-epdfinfo-message)
  (defun mjs/pdf-annot-update-header ()
    (setq header-line-format
          (concat
           "Save annotation with "
           (propertize "SPC m P s" 'face 'help-key-binding)
           ", or abort with "
           (propertize "SPC m P k" 'face 'help-key-binding)
           " in normal mode.")))
  ;; based on 'pdf-view-kill-ring-save'
  (defun mjs/pdf-view-evil-yank-visual ()
    (interactive)
    (pdf-view-assert-active-region)
    (let ((txt (pdf-view-active-region-text))
          (reg evil-this-register))
      (pdf-view-deactivate-region)
      (evil-set-register
       (or reg ?\")
       (mapconcat #'identity txt))))
  (add-hook 'pdf-annot-edit-contents-minor-mode-hook #'mjs/pdf-annot-update-header))

(use-package saveplace-pdf-view
  :after pdf-view)

(use-package org-noter)

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    "Add a function to ensure precise note is inserted"
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))


(use-package org-roam
  :custom (org-roam-directory (file-truename org-directory))
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${all-tags:60}" 'face 'org-tag)))

  :general (mjs-leader-def :keymaps 'override
             "n r"   '(nil :which-key "Roam")
             "n r a" '("Add Alias" . org-roam-alias-add)
             "n r A" '("Remove Alias" . org-roam-alias-remove)
             "n r b" '("Toggle Roam Buffer" . org-roam-buffer-toggle)
             "n r f" '("Find Node" . (lambda () (interactive)
                                       (mjs/org-roam-find-node nil "^")))
             "n r F" '("Find Ref" . org-roam-ref-find)
             "n r g" '("Graph" . org-roam-graph)
             "n r i" '("Insert Link" . mjs/org-roam-node-insert)
             "n r c" '("Roam Capture" . mjs/org-roam-capture)
             "n r s" '("Roam Sync" . org-roam-db-sync)
             "n r S" '("Strip Roam Links" . mjs/strip-org-roam-links)
             "n r d" '("Daily" . org-roam-dailies-capture-today)
             "n r r" '("Random Node" . org-roam-node-random)
             "n r t" '("Add Tags" . org-roam-tag-add)
             "n r T" '("Remove Tags" . org-roam-tag-remove)
             "i U"   '("Update Roam IDs" . org-roam-update-org-id-locations))
  (mjs-local-leader-def :keymaps 'org-mode-map
    "m"   '(nil :which-key "Roam")
    "m a" '("Add Alias" . org-roam-alias-add)
    "m A" '("Remove Alias" . org-roam-alias-remove)
    "m b" '("Toggle Roam Buffer" . org-roam-buffer-toggle)
    "m f" '("Find Node" . (lambda () (interactive) (mjs/org-roam-find-node nil "^")))
    "m F" '("Find Ref" . org-roam-ref-find)
    "m g" '("Graph" . org-roam-graph)
    "m i" '("Insert Link" . mjs/org-roam-node-insert)
    "m s" '("Roam Sync" . org-roam-db-sync)
    "m S" '("Strip Roam Links" . mjs/strip-org-roam-links)
    "m d" '("Daily" . org-roam-dailies-capture-today)
    "m r" '("Random Node" . org-roam-node-random)
    "m t" '("Add Tags" . org-roam-tag-add)
    "m T" '("Remove Tags" . org-roam-tag-remove))
  (:states 'insert :keymaps 'org-mode-map
           "C-f" #'mjs/org-roam-node-insert
           "C-S-f" #'org-insert-link)
  :hook (org-mode . org-roam-db-autosync-mode)
  ;; Taken from https://github.com/org-roam/org-roam/pull/2219/files
  :config (defconst mjs/org-roam-bracket-completion-re
            "\\[\\(?:\\[id:\\([^z-a]*?\\)]\\)?\\[\\([^z-a]+?\\)]]"
            "Regex for completion within link brackets.

Matches both empty links (i.e. \"[[|]]\") and existing \"id:\"
links (e.x. \"[[id:01234][|]]\").")

  (setq safe-local-variable-values '((mjs/org-auto-tags--current-list . ("great_basin"))
                                     (mjs/org-auto-tags--current-list . ("kb"))))

  (cl-defmethod org-roam-node-all-tags ((node org-roam-node))
    (mapconcat (lambda (tag) (concat "#" tag))
               (delete-dups
                (flatten-list
                 (mapcar #'mjs/all-parent-tags
                         (org-roam-node-tags node))))
               " "))

  (defvar mjs/org-context-plist
    (list
     :none
     (list :name "none"
           :tags (list))
     :knowledge-base
     (list :name "knowledge-base"
           :tags '("kb"))
     :ttrpg
     (list :name "ttrpg"
           :tags '("ttrpg"))
     :great-basin
     (list :name "great-basin"
           :tags '("great_basin"))
     :madttrpg
     (list :name "madttrpg"
           :tags '("madttrpg")))
    "A list of tags groups defining common writing contexts")

  (defvar mjs/org-roam-capture-templates-plist
    (list
     :knowledge-base
     `(("k" "Knowledge Base" plain "%?"
        :target (file+head "knowledge-base/%<%Y%m%d%H%M%S>-${slug}.org"
                           ,(concat "#+filetags: ${auto-tags}\n"
                                    "#+author: %(user-full-name)\n"
                                    "#+title: ${title}\n"))
        :unnarrowed t))
     :great-basin
     `(("g" "Great Basin")
       ("gc" "Great Basin Character" plain
        ,(format "%s%%[%s%s]"
                 (concat "#+filetags: :great_basin:character:\n"
                         "#+title: ${title}\n\n")
                 org-roam-directory
                 "/ttrpg/games/great-basin/characters/template.org")
        :target (file "ttrpg/games/great-basin/%<%Y%m%d%H%M%S>-${slug}.org")
        :unnarrowed t)
       ("ge" "Great Basin Event" plain
        ,(format "%s%%[%s%s]"
                 (concat "#+filetags: :great_basin:event:\n"
                         "#+title: ${title}\n")
                 org-roam-directory
                 "/ttrpg/games/great-basin/events/template.org")
        :target (file "ttrpg/games/great-basin/%<%Y%m%d%H%M%S>-${slug}.org")
        :unnarrowed t)
       ("gl" "Great Basin Location" plain
        ,(format "%s%%[%s%s]"
                 (concat "#+filetags: :great_basin:location:\n"
                         "#+title: ${title}\n")
                 org-roam-directory
                 "/ttrpg/games/great-basin/locations/template.org")
        :target (file "ttrpg/games/great-basin/%<%Y%m%d%H%M%S>-${slug}.org")
        :unnarrowed t)
       ("go" "Great Basin Object" plain
        ,(format "%s%%[%s%s]"
                 (concat "#+filetags: :great_basin:object:\n"
                         "#+title: ${title}\n")
                 org-roam-directory
                 "/ttrpg/games/great-basin/%<%Y%m%d%H%M%S>-${slug}.org")
        :target (file "ttrpg/games/great-basin/objects/template.org")
        :unnarrowed t)
       ("gr" "Great Basin Session Record" plain
        ,(format "%%[%s%s]"
                 org-roam-directory
                 "/ttrpg/games/great-basin/sessions/template.org")
        :target (file "ttrpg/games/great-basin/sessions/great-basin-%<%Y-%m-%d>.org")
        :unnarrowed t)
       ("gR" "Great Basin Public Session Record" plain
        ,(format "%%[%s%s]"
                 org-roam-directory
                 "/ttrpg/games/great-basin/public/session-recaps/template.org")
        :target (file "ttrpg/games/great-basin/public/session-recaps/great-basin-%<%Y-%m-%d>.org")
        :unnarrowed t)
       ("gs" "Great Basin Stat Block" plain
        ,(format "%s%%[%s%s]"
                 (concat "#+filetags: :great_basin:stat:\n"
                         "#+title: ${title}\n")
                 org-roam-directory
                 "/ttrpg/games/great-basin/stat-blocks/template.org")
        :target (file "ttrpg/games/great-basin/stat-blocks/%<%Y%m%d%H%M%S>-${slug}.org")
        :unnarrowed t))
     :ttrpg
     `(("c" "5E Character Sheet" plain
        ,(format "%s%%[%s%s]"
                 (concat "#+filetags: :ttrpg:stat:\n"
                         "#+title: ${title}\n")
                 org-roam-directory
                 "/ttrpg/systems/dungeons-and-dragons-5e/character.org")
        :target (file "ttrpg/games/%<%Y%m%d%H%M%S>-${slug}.org")
        :unnarrowed t)
       ("e" "Etera Session" entry "%?"
        :target (file+datetree "ttrpg/games/etera/notes.org" 'day)
        :unnarrowed t
        :immediate-finish t
        :jump-to-captured t)
       ))
    "Templates for use with `org-roam-capture'.")

  (defvar mjs/org-auto-tags--current-list
    (list)
    "The list of tags to automatically apply to an org capture")

  (cl-defun mjs/org-context-list-completing-read (&key (context-plist mjs/org-context-plist))
    "Create a list of contexts from the CONTEXT-PLIST for completing read.

The form should be '((\"none\" 1) (\"knowledge-base\" 3) ...)."
    (-non-nil (seq-map-indexed
               (lambda (context index)
                 (when (cl-oddp index)
                   (list (plist-get context :name) index)))
               context-plist)))

  (cl-defun org-roam-node-auto-tags (node &key (tag-list mjs/org-auto-tags--current-list))
    "Inject the TAG-LIST into the {auto-tags} region of captured NODE."
    (if (and tag-list (> (length tag-list) 0))
        (concat ":" (s-join ":" tag-list) ":")
      ""))

  (cl-defun mjs/org-roam-filter-context-fn (node &key
                                                 (tag-list mjs/org-auto-tags--current-list))
    "Determine if TAG-LIST is a subset of NODE's tags."
    ;; gnus-subsetp is a more "permissive" version of subsetp. It doesn't
    ;; consider order. And looks at strings as equal if their values are the
    ;; same.
    (gnus-subsetp tag-list
                  (delete-dups
                   (flatten-list
                    (mapcar #'mjs/all-parent-tags
                            (org-roam-node-tags node))))))

  (cl-defun mjs/org-roam-templates-list
      (context &key (template-plist mjs/org-roam-capture-templates-plist))
    "List of `org-roam' capture templates based on the given CONTEXT.

Searches the TEMPLATE-PLIST for the templates.

Note, the `:all' or `:none' contexts assumes we use the whole list"
    (if (or (eq context :all) (eq context :none))
        (-non-nil
         (seq-map-indexed
          (lambda (temp index)
            (when (cl-oddp index)
              temp))
          template-plist))
      (plist-get template-plist context)))

  (cl-defun mjs/org-roam-templates-context-fn
      (&key (tag-list mjs/org-auto-tags--current-list))
    "Returns a set of templates based on TAG-LIST.

Returns templates for all contexts defined in `mjs/org-context-plist' whose tags
are a subset of TAG-LIST, with the exception of the `:none' context which is only
used if TAG-LIST is empty."
    (if (and tag-list (> (length tag-list) 0))
        (-flatten-n 1
                    (-non-nil (seq-map-indexed
                               (lambda (context index)
                                 (when (and (cl-oddp index)
                                            (not (string= (plist-get context :name) "none"))
                                            (gnus-subsetp (plist-get context :tags)
                                                          mjs/org-auto-tags--current-list))
                                   (mjs/org-roam-templates-list
                                    (intern-soft (concat ":" (plist-get context :name))))))
                               mjs/org-context-plist)))
      (mjs/org-roam-templates-list :all)))

  (advice-add #'org-roam-complete-link-at-point
              :override #'mjs/org-roam-complete-link-at-point)
  (advice-add #'org-roam-complete-everywhere
              :override #'mjs/org-roam-complete-everywhere))


;; (use-package vulpea
;;   :hook (org-roam-db-autosync-mode . vulpea-db-autosync-mode))

(use-package org-cliplink
  :commands mjs/clean-org-cliplink
  :general (mjs-local-leader-def :keymaps 'org-mode-map
             "l c" '("Paste URL" . mjs/clean-org-cliplink)
             "l C" '("Paste Raw URL" . org-cliplink)))

(use-package org-chef
  :after org
  :init (add-to-list 'org-capture-templates
                     `("r" "Recipe" entry
                       (file ,(concat org-directory "recipes.org"))
                       "%(org-chef-get-recipe-from-url)"
                       :empty-lines 1) t)
  (add-to-list 'org-capture-templates
               `("R" "Manual Recipe" entry
                 (file ,(concat org-directory "personal/recipes.org"))
                 ,(concat "* %^{Recipe title: }\n"
                          "  :PROPERTIES:\n"
                          "  :source:\n"
                          "  :export_hugo_bundle:\n"
                          "  :export_file_name: index"
                          "  :END:\n\n"
                          "** Ingredients\n\n"
                          "   %?\n\n"
                          "** Directions\n\n")) t))

(use-package ox-pandoc
  :general (mjs-local-leader-def :keymap 'org-mode-map
             "e" '(nil :which-key "Export")
             "e e" '("Export Dispatcher" . org-export-dispatch)
             "e l" '(nil :which-key "LaTeX")
             "e l b" '("Pandoc LaTeX Buffer" . org-pandoc-export-as-latex)
             "e l l" '("LaTeX File" . org-latex-export-to-pdf)
             "e l f" '("Pandoc LaTeX File" . org-pandoc-export-to-latex)
             "e l o" '("Pandoc LaTeX Open File" . org-pandoc-export-to-latex-and-open)
             "e l p" '("Pandoc LaTeX PDF" . org-pandoc-export-to-latex-pdf)
             "e l P" '("Pandoc LaTeX Open PDF" . org-pandoc-to-latex-pdf-and-open)
             "e H" '(nil :which-key "HTML")
             "e H b" '("HTML5 Buffer" . org-pandoc-export-as-html5)
             "e H B" '("HTML4 Buffer" . org-pandoc-export-as-html4)
             "e H f" '("HTML5 File" . org-pandoc-export-to-html5)
             "e H F" '("HTML4 File" . org-pandoc-export-to-html4)
             "e H o" '("Open HTML5 File" . org-pandoc-export-to-html5-and-open)
             "e H O" '("Open HTML4 File" . org-pandoc-export-to-html4-and-open)
             "e M" '(nil :which-key "Man")
             "e M b" '("Man Buffer" . org-pandoc-export-as-man)
             "e M f" '("Man File" . org-pandoc-export-to-man)
             "e M o" '("Open Man File" . org-pandoc-export-to-man-and-open)
             "e m" '(nil :which-key "Markdown")
             "e m b" '("Markdown Buffer" . org-pandoc-export-as-markdown)
             "e m f" '("Markdown File" . org-pandoc-export-to-markdown)
             "e m o" '("Open Markdown File" . org-pandoc-export-to-markdown-and-open)
             "e w" '(nil :which-key "MediaWiki")
             "e w b" '("MediaWiki Buffer" . org-pandoc-export-as-mediawiki)
             "e w f" '("MediaWiki File" . org-pandoc-export-to-mediawiki)
             "e w o" '("Open MediaWiki File" . org-pandoc-export-to-mediawiki-and-open)))

(use-package ox-hugo
  :general (mjs-local-leader-def :keymaps 'org-mode-map
             "e h" '(nil :which-key "Hugo")
             "e h d" '("DWIM" . org-hugo-export-wim-to-md)
             "e h a" '("All Subtrees" . (lambda () (org-hugo-wim-to-md t)))
             "e h o" '("File" . org-hugo-export-to-md))
  :config
  (advice-add #'org-hugo--export-subtree-to-md
              :after #'mjs/hugo-blowfish-thumbnail)
  (advice-add #'org-hugo--export-file-to-md
              :after #'mjs/hugo-blowfish-thumbnail))

(use-package org-tree-slide
  :commands org-tree-slide-mode
  :diminish " 󰐨"
  :general (mjs-local-leader-def :keymaps 'org-mode-map
             "p" '("Present" . org-tree-slide-mode))
  (:states '(normal insert motion) :keymaps 'org-tree-slide-mode-map
           "C-<right>" #'org-tree-slide-move-next-tree
           "C-<left>" #'org-tree-slide-move-previous-tree)
  :custom (org-tree-slide-activate-message " ")
  (org-tree-slide-deactivate-message " ")
  (org-tree-slide-modeline-display nil)
  (org-tree-slide-heading-emphasis t)
  :config (org-tree-slide-simple-profile)
  (add-hook 'org-tree-slide-play-hook
            (lambda ()
              (text-scale-set 4)))
  (add-hook 'org-tree-slide-stop-hook
            (lambda ()
              (text-scale-set 1)
              (text-scale-mode -1))))

(use-package org-timeblock
  :ensure nil
  :commands org-timeblock
  :general (mjs-leader-def :keymaps 'override "B" '("Time Blocks" . org-timeblock)))

(use-package oc
  :ensure nil
  :after org
  :custom ((org-cite-global-bibliography '("~/Documents/zotero.bib"))
           (org-cite-csl-styles-dir "~/Zotero/styles/")
           (org-cite-export-processors '((latex biblatex)
                                         (md . (csl "ieee.csl"))
                                         (t . (csl "ieee.csl")))))
  :general (mjs-local-leader-def :keymaps 'org-mode-map
             "C" '("Citation" . org-cite-insert))
  :custom-face (org-cite ((t (:foreground ,(plist-get base16-stylix-theme-colors :base0B)))))
  (org-cite-key ((t (:foreground ,(plist-get base16-stylix-theme-colors :base0B) :slant italic))))
  :config
  (require 'citar)
  (require 'citar-org))

(use-package citar
  :after oc
  :custom (citar-bibliography org-cite-global-bibliography)
  :init
  (put 'citar-bibliography 'safe-local-variable #'listp)
  :config
  (defvar citar-indicator-files-icon
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-file_pdf_o"
              :face 'nerd-icons-lred)
     :function #'citar-has-files
     :padding " "
     :tag "has:files"))
  (defvar citar-indicator-notes-icon
    (citar-indicator-create
     :symbol (nerd-icons-sucicon
              "nf-custom-orgmode"
              :face 'nerd-icons-lgreen)
     :function #'citar-has-notes
     :padding " "
     :tag "has:notes"))
  (defvar citar-indicator-links-icon
    (citar-indicator-create
     :symbol (nerd-icons-octicon
              "nf-oct-link"
              :face 'nerd-icons-lblue)
     :function #'citar-has-links
     :padding " "
     :tag "has:links"))
  (defvar citar-indicator-cited-icon
    (citar-indicator-create
     :symbol (nerd-icons-octicon
              "nf-oct-book"
              :face 'nerd-icons-lred)
     :function #'citar-is-cited
     :padding " "
     :tag "is:cited"))
  (setq citar-indicators
        (list citar-indicator-files-icon
              citar-indicator-notes-icon
              citar-indicator-links-icon
              citar-indicator-cited-icon)))

(use-package citar-org
  :ensure nil
  :after (oc citar)
  :custom ((org-cite-insert-processor 'citar)
           (org-cite-follow-processor 'citar)
           (org-cite-activate-processor 'citar)))

(use-package citar-embark
  :after citar
  :diminish citar-embark-mode
  :init (setq citar-at-point-function 'embark-act)
  :hook ((org-mode . citar-embark-mode)
         (LaTeX-mode . citar-embark-mode)))

(use-package citar-capf
  :after citar
  :ensure nil
  :hook ((org-mode . citar-capf-setup)
         (LaTeX-mode . citar-capf-setup)))

(use-package latex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook ((LaTeX-mode . (lambda () (setq mode-name " LaTeX")))
         (LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . TeX-fold-mode)
         (LaTeX-mode . TeX-PDF-mode)
         (LaTeX-mode . mjs/preview-scale-adjustment)
         (LaTeX-mode . auto-fill-mode)
         (LaTeX-mode . (lambda () (apheleia-mode -1)))
         (after-save . (lambda ()
		  				 (when (eq major-mode #'LaTeX-mode)
						   (TeX-command-run-all nil)))))
  :custom ((TeX-newline-function #'reindent-then-newline-and-indent)
           (TeX-command-default "LaTeX")
           (TeX-check-TeX nil)
           (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)
                                    ("Zathura" "zathura --synctex-forward :: %o")))
           (TeX-view-program-selection '((output-pdf "PDF Tools")))
           (TeX-source-correlate-start-server t)
           (TeX-save-query nil)
           (TeX-engine 'luatex)
           (TeX-parse-self t)
           (TeX-auto-save t))
  :init (require 'auctex)
  :config (general-define-key :states '(insert normal) :map 'LaTeX-mode-map
                              "C-S-e" #'mjs/latex-math-from-calc)
  (add-to-list 'TeX-command-list '("Make" "make" TeX-run-compile nil t))
  (require 'citar-latex)
  :general
  (:states 'insert :keymaps 'LaTeX-mode-map
           "C-S-c" #'citar-insert-citation)
  (mjs-local-leader-def :keymaps 'LaTeX-mode-map
    "c" '("Compile Document" . TeX-command-run-all)
    "C" '("Clean Document" . TeX-clean)
    "e" '("Insert Environment" . LaTeX-environment)
    "i" '("Indent Line" . LaTeX-indent-line)
    "m" '("Insert Macro" . TeX-insert-marco)
    "s" '("Insert Section" . LaTeX-section)
    "t" '("Insert Citation" . citar-insert-citation)
    "v" '("View PDF" . TeX-view)))

(use-package yasnippet
  :diminish (yas-minor-mode . " 󰁨")
  :hook ((LaTeX-mode . yas-minor-mode)
         (dashboard-mode . yas-reload-all)
         (org-mode . yas-minor-mode)
         (coq-mode . yas-minor-mode)
         (post-self-insert . mjs/yas-try-expanding-auto-snippets))
  :custom (yas-triggers-in-field t)
  (yas-snippets-dirs '((expand-file-name "snippets" user-emacs-directory)))
  (yas-verbosity 4)
  :general (mjs-leader-def :keymap 'yas-keymap
             "i s" '("Snippet" . yas-insert-snippet))
  :config (use-package warnings
            :ensure nil
            :config
            (cl-pushnew '(yasnippet backquote-change)
                        warning-suppress-types
                        :test 'equal))
  (general-define-key :keymaps 'yas-keymap :states 'insert
                      "<tab>" #'mjs/yas-next-field-or-cdlatex
                      "TAB" #'mjs/yas-next-field-or-cdlatex))

(use-package cdlatex
  :diminish " "
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (cdlatex-tab . yas-expand)
         (cdlatex-tab . mjs/cdlatex-in-yas-field))
  :custom (texmathp-tex-commands '(("bmatrix" env-on)
                                   ("pmatrix" env-on)
                                   ("mathpar" env-on)))
  (cdlatex-env-alist '(("proof" "\\begin{proof}\n?\n\\end{proof}" nil)
                       ("par" "\\begin{mathpar}\n?\n\\end{mathpar}" nil)
                       ("optp" "\\begin{array}{r@{\\quad}l}\n\\min & ? \ns.t. & \n\\end{array}" nil)))
  (cdlatex-insert-auto-labels-in-env-templates nil)
  :config (general-define-key :keymaps 'LaTeX-mode-map :states 'insert
                              "<tab>" #'cdlatex-tab)
  (add-to-list 'cdlatex-command-alist '("proof" "Insert proof env" ""
                                        cdlatex-environment ("proof") t nil))
  (add-to-list 'cdlatex-command-alist '("emph" "Insert emphasis" "\\emph{?}"
                                        cdlatex-position-cursor nil t nil)))

(use-package org-table
  :ensure nil
  :demand nil
  :diminish (orgtbl-mode . " 󱝃")
  :after cdlatex
  :hook (cdlatex-mode . lazytab-cdlatex-or-orgtbl-next-field)
  ;; :init (add-hook 'cdlatex-tab-hook #'lazytab-cdlatex-or-orgtbl-next-field 90)
  :config (general-define-key :keymaps 'orgtbl-mode-map :states 'insert
                              "<tab>" #'lazytab-org-table-next-field-maybe
                              "TAB" #'lazytab-org-table-next-field-maybe)

  (add-to-list 'cdlatex-command-alist '("smat" "Insert smallmatrix env"
                                        "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
                                        lazytab-position-cursor-and-edit
                                        nil nil t))
  (add-to-list 'cdlatex-command-alist '("bmat" "Insert bmatrix env"
                                        "\\begin{bmatrix} ? \\end{bmatrix}"
                                        lazytab-position-cursor-and-edit
                                        nil nil t))
  (add-to-list 'cdlatex-command-alist '("pmat" "Insert pmatrix env"
                                        "\\begin{pmatrix} ? \\end{pmatrix}"
                                        lazytab-position-cursor-and-edit
                                        nil nil t))
  (add-to-list 'cdlatex-command-alist '("tbl" "Insert tabluar"
                                        "\\begin{center}\n\\begin{tabular} ? \\end{tabular}\\end{center}"
                                        lazytab-position-cursor-and-edit
                                        nil t nil)))

(use-package typst-ts-mode
  :mode ("\\.typ\\'" . typst-ts-mode)
  :init
  (defun mjs/typst-ts-editing-auto-fill-function ()
    "Auto Fill Function for `auto-fill-mode', allowing the prefix to be `nil'"
    (if (>= (current-column) (current-fill-column))
        (let* ((fill-prefix (typst-ts-editing-calculate-fill-prefix))
               (adaptive-fill-mode (null fill-prefix)))
          (do-auto-fill))))
  :hook (typst-ts-mode . (lambda ()
                           (progn
                             (set
                              (make-local-variable
                               'normal-auto-fill-function)
                              #'mjs/typst-ts-editing-auto-fill-function)
                             (auto-fill-mode))))
  :custom
  (typst-ts-mode-enable-raw-blocks-highlight t)
  :general (mjs-local-leader-def :keymaps 'typst-ts-mode-map
             "c" '("Compile" . typst-ts-compile)
             "#" '("Renumber List" . typst-ts-editing-item-list-renumber)
             "l" '("Insert Link" . typst-ts-mc-insert-link)
             "i" '("Insert Symbol" . typst-ts-editing-symbol-picker)
             "o" '("Open" . typst-ts-mc-open-at-point)
             "O" '("Open PDF" . typst-ts-compile-and-preview)
             "p" '(nil :which-key "Preview")
             "p o" '("Open" . typst-preview-open-browser)
             "p r" '("Restart" . typst-preview-restart)
             "p c" '("Check Connection" . typst-preview-connected-p)
             "p s" '("Start" . typst-preview-start)
             "p j" '("Jump to Point" . typst-preview-send-position)
             "s" '(nil :which-key "Search")
             "s p" '("Package" . typst-ts-mc-search-package)
             "s s" '("Symbol" . typst-ts-mc-search-typst-symbol)
             "s h" '("Handwritten Symbol" . typst-ts-mc-recognize-typst-symbol)
             "t" '(nil :which-key "Table")
             "t h" '("Swap Cell Right" . typst-ts-editing-grid-cell-right)
             "t l" '("Swap Cell Left" . typst-ts-editing-grid-cell-left)
             "t k" '("Swap Cell Up" . typst-ts-editing-grid-cell-up)
             "t j" '("Swap Cell Down" . typst-ts-editing-grid-cell-down)
             "t J" '("Swap Row Down" . typst-ts-editing-grid-row-down)
             "t K" '("Swap Row Up" . typst-ts-editing-grid-row-up)
             "w" '("Watch" . typst-ts-watch-start)
             "W" '("Watch mode" . typst-ts-watch-mode)
             "x" '("Kill Watch" . typst-ts-watch-stop))
  :config
  (put 'tp--master-file 'safe-local-variable #'stringp)
  (require 'typst-preview)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((typst-ts-mode) . ,(eglot-alternatives
                                       '("tinymist" "typst-lsp"))))))

(use-package websocket)
(use-package typst-preview
  :ensure nil
  :diminish typst-preview-mode
  :init
  (setq typst-preview-open-browser-automatically t)
  :custom
  (typst-preview-partial-rendering t)
  (typst-preview-browser "default"))

;; HACK: It's really funny if `edit-indirect' is an
;; indirect dependency of `markdown-mode'
(use-package edit-indirect)

(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :custom ((markdown-italic-underscore t)
           (markdown-asymmetric-header t)
           (markdown-gfm-additional-languages '("sh"))
           (markdown-make-gfm-checkboxes-buttons t)
           (markdown-fontify-whole-heading-line t)
           (markdown-fontify-code-blocks-natively t)
           (markdown-open-command "xdg-open")
           (markdown-content-type "application/xhtml+xml")
           (markdown-css-paths
            '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"

              "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css"))
           (markdown-xhtml-header-content
            (concat "<meta name='viewport' content='width=device-width, initial-scale=1,shrink-to-fit=no'>"
                    "<style> body { box-sizing: border-box; max-width: 740px; width: 100%;margin: 40px auto; padding: 0 10px; } </style>"
                    "<script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>"
                    "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
                    "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>")))
  :general (mjs-local-leader-def :keymaps 'markdown-mode-map
             "'" '("Edit Code Block" . markdown-edit-code-block)
             "o" '("Open Markdown" . markdown-open)
             "p" '("Preview Markdown" . markdown-preview)
             "e" '("Export Markdown" . markdown-export)
             "i" '(nil :which-key "Insert")
             "i T" '("Table of Content" . markdown-toc-generate-toc)
             "i i" '("Image" . markdown-insert-image)
             "i l" '("Link" . markdown-insert-link)
             "i -" '("Heading" . markdown-insert-hr)
             "i 1" '("Heading 1" . markdown-insert-header-atx-1)
             "i 2" '("Heading 2" . markdown-insert-header-atx-2)
             "i 3" '("Heading 3" . markdown-insert-header-atx-3)
             "i 4" '("Heading 4" . markdown-insert-header-atx-4)
             "i 5" '("Heading 5" . markdown-insert-header-atx-5)
             "i 6" '("Heading 6" . markdown-insert-header-atx-6)
             "i C" '("Code Block" . markdown-insert-gfm-code-block)
             "i P" '("Pre Region" . markdown-pre-region)
             "i Q" '("Blockquote Region" . markdown-blockquote-region)
             "i [" '("Checkbox" . markdown-insert-gfm-checkbox)
             "i b" '("Bold" . markdown-insert-bold)
             "i c" '("Inline Code" . markdown-insert-code)
             "i e" '("Italic" . markdown-insert-italic)
             "i f" '("Footnote" . markdown-insert-footnote)
             "i h" '("Header DWIM" . markdown-insert-header-dwim)
             "i i" '("Italic" . markdown-insert-italic)
             "i k" '("Kbd" . markdown-insert-kbd)
             "i p" '("Pre" . markdown-insert-pre)
             "i q" '("New Blockquote" . markdown-insert-blockquote)
             "i s" '("Strike Through" . markdown-insert-strike-through)
             "i t" '("Table" . markdown-insert-table)
             "i w" '("Wiki Link" . markdown-insert-wiki-link)
             "t" '(nil :which-key "Toggle")
             "t e" '("Inline LaTeX" . markdown-toggle-math)
             "t f" '("Code Highlights" . markdown-toggle-fontify-code-blocks-natively)
             "t i" '("Inline Images" . markdown-toggle-inline-images)
             "t l" '("URL Hiding" . markdown-toggle-url-hiding)
             "t m" '("Markup Hiding" . markdown-toggle-markup-hiding)
             "t w" '("Wiki Links" . markdown-toggle-wiki-links)
             "t x" '("GFM Checkboxes" . markdown-toggle-gfm-checkbox)))
;; FIXME: These pieces of advice break all markdown mode fontification :(
;; :config
;; (advice-add #'markdown-fontify-code-block-natively
;;             :around #'mjs/markdown-optimize-src-buffer-modes)
;; (advice-add #'markdown-match-generic-metadata
;;             :override #'mjs/markdown-disable-front-matter-fontification))

(use-package evil-markdown
  :hook (markdown-mode . evil-markdown-mode)
  :general (:keymaps 'evil-markdown-mode-map :states 'normal
                     "TAB" #'markdown-cycle
                     "S-TAB" #'markdown-shifttab
                     "M-r" #'browse-url-of-file)
  (:keymaps 'evil-markdown-mode-map :states 'insert
            "M-*" #'markdown-insert-list-item
            "M-b" #'markdown-insert-bold
            "M-i" #'markdown-insert-italic
            "M-`" #'mjs/markdown-insert-strikethrough
            "M--" #'markdown-insert-hr)
  (:keymaps 'evil-markdown-mode-map :states 'motion
            "]h" #'markdown-next-visible-heading
            "[h" #'markdown-previous-visible-heading
            "[p" #'markdown-promote
            "]p" #'markdown-demote
            "[l" #'markdown-previous-link
            "]l" #'markdown-next-link)
  :diminish evil-markdown-mode
  :config (add-hook 'evil-markdown-mode-hook #'evil-normalize-keymaps))

(use-package quarto-mode
  :mode ("\\.qmd\\'" . poly-quarto-mode))

(use-package ready-player
  :config (ready-player-mode +1))

(use-package ledger-mode
  :mode ("\\.ldg\\'" . ledger-mode)
  :custom (ledger-clear-whole-transactions 1)
  (ledger-mode-should-check-version nil)
  :diminish outline-minor-mode
  :hook (ledger-mode . outline-minor-mode))

(use-package evil-ledger
  :diminish evil-ledger-mode
  :hook (ledger-mode . evil-ledger-mode))

(use-package calfw
  :commands (mjs/open-calendar)
  :custom (calendar-week-start-day 1)
  :general (mjs-leader-def
             "C"     '(nil :which-key "Calendar")
             "C a"   '("Open Agenda on Day" . cfw:org-open-agenda-day)
             "C c"   '("Open Calendar" . mjs/open-calendar)
             "C d"   '("GOTO Date" . cfw:navi-goto-date-command)
             "C m"   '("First Day" . cfw:navi-goto-first-date-command)
             "C M"   '("Last Day" . cfw:navi-goto-last-date-command)
             "C q"   '("Quit Calendar" . cfw:org-clean-exit)
             "C r"   '("Rebuild Calendar" . cfw:refresh-calendar-buffer)
             "C v"   '(nil :which-key "Calender Views")
             "C v m" '("Month" . mjs/open-calendar)
             "C w"   '("Beginning of Week" . cfw:navi-goto-week-begin-command)
             "C W"   '("End of Week" . cfw:navi-goto-week-end-command))
  :hook (cfw:calendar-mode . (lambda ()
                               (general-define-key :states '(normal motion) :keymaps 'local
                                                   "TAB"    #'cfw:navi-next-item-command
                                                   "S-TAB"  (lambda ()
                                                              (interactive)
                                                              (cfw:navi-next-item-command -1))
                                                   "h"      (lambda ()
                                                              (interactive)
                                                              (cfw:navi-next-day-command -1))
                                                   "j"      #'cfw:navi-next-week-command
                                                   "M-j"    #'cfw:navi-next-month-command
                                                   "k"      (lambda ()
                                                              (interactive)
                                                              (cfw:navi-next-week-command -1))
                                                   "M-k"    (lambda ()
                                                              (interactive)
                                                              (cfw:navi-next-month-command -1))
                                                   "l"      #'cfw:navi-next-day-command
                                                   "RET"    #'cfw:show-details-command)))
  (cfw:details-mode . (lambda ()
                        (general-define-key :states '(normal motion) :keymaps 'local
                                            "q" #'kill-buffer-and-window)))
  :custom-face
  (cfw:face-title ((t  :foreground ,(plist-get base16-stylix-theme-colors :base0A)
                       :weight bold
                       :height 2.0)))
  (cfw:face-header ((t :foreground ,(plist-get base16-stylix-theme-colors :base0A) :weight bold)))
  (cfw:face-sunday ((t :foreground ,(plist-get base16-stylix-theme-colors :base08) :weight bold)))
  (cfw:face-saturday ((t :foreground ,(plist-get base16-stylix-theme-colors :base0D)
                         :weight bold)))
  (cfw:face-holiday ((t :background ,(plist-get base16-stylix-theme-colors :base00)
                        :foreground ,(plist-get base16-stylix-theme-colors :base08)
                        :weight bold)))
  (cfw:face-grid ((t :foreground ,(plist-get base16-stylix-theme-colors :base05))))
  (cfw:face-default-content ((t :foreground ,(plist-get base16-stylix-theme-colors :base0B))))
  (cfw:face-periods ((t :foreground ,(plist-get base16-stylix-theme-colors :base0D))))
  (cfw:face-day-title ((t :background ,(plist-get base16-stylix-theme-colors :base00))))
  (cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
  (cfw:face-annotation ((t :foreground ,(plist-get base16-stylix-theme-colors :base07)
                           :inherit cfw:face-day-title)))
  (cfw:face-disable ((t :foreground ,(plist-get base16-stylix-theme-colors :base07)
                        :inherit cfw:face-day-title)))
  (cfw:face-today-title ((t :foreground ,(plist-get base16-stylix-theme-colors :base00)
                            :background ,(plist-get base16-stylix-theme-colors :base0B)
                            :weight bold)))
  (cfw:face-today ((t :background: ,(plist-get base16-stylix-theme-colors :base02)
                      :weight bold)))
  (cfw:face-select ((t :background ,(plist-get base16-stylix-theme-colors :base04))))
  (cfw:face-toolbar ((t :foreground ,(plist-get base16-stylix-theme-colors :base0D)
                        :background ,(plist-get base16-stylix-theme-colors :base0D))))
  (cfw:face-toolbar-button-off ((t :foreground ,(plist-get base16-stylix-theme-colors :base03)
                                   :weight bold)))
  (cfw:face-toolbar-button-on ((t :foreground ,(plist-get base16-stylix-theme-colors :base07)
                                  :weight bold)))
  :config
  (require 'calfw-org)
  (require 'calfw-ical)
  (defun mjs/open-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "Green")
      (cfw:ical-create-source "Work" (f-read-text "/run/secrets/calendar/work") "Orange")
      (cfw:ical-create-source "TAA" (f-read-text "/run/secrets/calendar/taa") "Blue")
      (cfw:ical-create-source "TAA Community" (f-read-text "/run/secrets/calendar/taa-com") "Blue"))
     :view 'month))
  (evil-set-initial-state 'cfw:calendar-mode 'normal))

(use-package calfw-org
  :after calfw
  :general (mjs-leader-def
             "C c"   '("Open Calendar" . cfw:open-org-calendar)))

(use-package calfw-ical
  :after calfw)

(use-package direnv
  :defer nil
  :general (mjs-leader-def :keymaps 'override
             "d" '("Direnv Allow" . direnv-allow))
  :config 
  (direnv-mode))

(use-package sly
  :custom ((sly-symbol-completion-mode nil)
           (inferior-lisp-prgram "sbcl"))
  :config (add-to-list 'sly-contribs 'sly-asdf 'append))

(use-package sly-macrostep)

(use-package sly-asdf)

(use-package haskell-mode
  ;; These aren't working and I have no idea why...
  :diminish haskell-doc-mode
  :diminish haskell-indent-mode
  :diminish interactive-haskell-mode
  :custom ((haskell-process-args-cabal-new-repl
            '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
           (haskell-process-type 'cabal-new-repl)
           (haskell-stylish-on-save 't))
  :hook
  (haskell-mode . (lambda ()
                    (diminish 'haskell-doc-mode)
                    (diminish 'haskell-indent-mode)
                    (diminish 'interactive-haskell-mode)))
  (haskell-mode . turn-on-haskell-doc-mode)
  (haskell-mode . turn-on-haskell-indent)
  (haskell-mode . interactive-haskell-mode))

(use-package proof-general
  :mode ("\\.v\\'" . coq-mode)
  :commands coq-mode
  :hook (coq-mode . (lambda ()
                      (set-face-background 'proof-locked-face
                                           (plist-get base16-stylix-theme-colors :base02))
                      (set-face-background 'proof-queue-face
                                           (plist-get base16-stylix-theme-colors :base03))
                      (set-face-background 'proof-warning-face
                                           (plist-get base16-stylix-theme-colors :base0A))
                      (set-face-foreground 'proof-tactics-name-face
                                           (plist-get base16-stylix-theme-colors :base0C))
                      (set-face-foreground 'proof-tacticals-name-face
                                           (plist-get base16-stylix-theme-colors :base0B))
                      (set-face-foreground 'coq-solve-tactics-face
                                           (plist-get base16-stylix-theme-colors :base09))
                      (set-face-background 'coq-cheat-face
                                           (plist-get base16-stylix-theme-colors :base08))
                      (set-face-foreground 'coq-cheat-face
                                           (plist-get base16-stylix-theme-colors :base00))
                      ))
  :custom ((coq-smie-user-tokens
            '(("," . ":=")
              ("∗" . "->")
              ("-∗" . "->")
              ("∗-∗" . "->")
              ("==∗" . "->")
              ("=∗" . "->") 			;; Hack to match ={E1,E2}=∗
              ("|==>" . ":=")
              ("⊢" . "->")
              ("⊣⊢" . "->")
              ("↔" . "->")
              ("←" . "<-")
              ("→" . "->")
              ("=" . "->")
              ("==" . "->")
              ("/\\" . "->")
              ("⋅" . "->")
              (":>" . ":=")
              ("by" . "now")
              ("forall" . "now")              ;; NB: this breaks current ∀ indentation.
              ))
           (proof-splash-seen nil)
           (coq-compiler "roqc compile")
           (coq-dependency-analyzer "rocq dep")
           (coq-compile-before-require t))
  :general (mjs-local-leader-def :keymaps 'coq-mode-map
             "a" '("Active Script" . proof-toggle-active-scripting)
             "g" '(nil :which-key "Goto")
             "g e" '("Command End" . proof-goto-command-end)
             "g l" '("Locked" . proof-goto-end-of-locked)
             "g s" '("Command Start" . proof-goto-command-start)
             "i" '(nil :which-key "Information")
             "i b" '("About" . coq-About)
             "i B" '("About with All" . coq-About-with-all)
             "i c" '("Check" . coq-Check)
             "i C" '("Check with All" . coq-Check-show-all)
             "i f" '("Find Theorems" . proof-find-theorems)
             "i l" '(nil :which-key "Locate")
             "i l c" '("Constant" . coq-LocateConstant)
             "i l l" '("Library" . coq-LocateLibrary)
             "i l n" '("Notation" . coq-LocateNotation)
             "i p" '("Print" . coq-Print)
             "i P" '("Print with All" . coq-Print-with-all)
             "i s" '("Search" . coq-Search)
             "i i" '(nil :which-key "Implicit")
             "i i b" '("About with Implicits" . coq-About-with-implicits)
             "i i c" '("Check with Implicits" . coq-Check-show-implicits)
             "i i p" '("Print with Implicits" . coq-Print-with-implicits)
             "I" '(nil :which-key "Insert")
             "I c" '("Command" . coq-insert-command)
             "I e" '("End Section" . coq-end-Section)
             "I i" '("Intros" . coq-insert-intros)
             "I r" '("Requires" . coq-insert-requires)
             "I s" '("Section or Module" . coq-insert-section-or-module)
             "I t" '("Tactic" . coq-insert-tactic)
             "I T" '("Tactical" . coq-insert-tactical)
             "l" '(nil :which-key "Layout")
             "l c" '("Clear Buffers" . pg-response-clear-displays)
             "l l" '("Adjust Layout" . proof-layout-windows)
             "l p" '("Proof State" . proof-prf)
             "." '("Assert to Line" . proof-goto-point)
             "L" '("Retract to Line" . proof-retract-until-point-interactive)
             "n" '("Assert Next Command" . proof-assert-next-command-interactive)
             "m" '("Undo Last Command" . proof-undo-last-successful-command)
             "p" '(nil :which-key "Proof")
             "p i" '("Interrupt" . proof-interrupt-process)
             "p p" '("Process Buffer" . proof-process-buffer)
             "p q" '("Quit" . proof-shell-exit)
             "p r" '("Retract" . proof-retract-buffer)
             "]" '("Assert Next Command" . proof-assert-next-command-interactive)
             "[" '("Undo Last Command" . proof-undo-last-successful-command))
  (:states 'insert :keymap 'coq-mode-keymap
           "M-n" #'proof-assert-next-command-interactive
           "M-m" #'proof-undo-last-successful-command
           "M-." #'proof-goto-point)
  :diminish hs-minor-mode 
  :diminish proof-active-buffer-fake-minor-mode
  :diminish outline-minor-mode
  :diminish (holes-mode . " 󰠣"))

(use-package math-symbol-lists
  :demand t
  :init
  (defun mjs/inherit-input-method ()
    "Inherit input method from `minibuffer-selected-window'."
    (let* ((win (minibuffer-selected-window))
           (buf (and win (window-buffer win))))
      (when buf
        (activate-input-method (buffer-local-value 'current-input-method buf)))))
  :config
  (add-hook 'coq-mode-hook (lambda () (set-input-method "math")
                             (ligature-mode -1)
                             ;; Remove the :: symbol
                             (setq prettify-symbols-alist '())
                             (setq coq-prettify-symbols-alist '(("/\\" . 8743)
                                                                ("\\/" . 8744)
                                                                ("forall" . 8704)
                                                                ("fun" . 955)
                                                                ("exists" . 8707)
                                                                ("->" . 8594)
                                                                ("<-" . 8592)
                                                                ("=>" . 8658)))))
  (add-hook 'minibuffer-setup-hook #'mjs/inherit-input-method)
  (quail-define-package "math" "UTF-8" "Ω" t)
  (quail-define-rules
   ("\\fun"    955)
   ("\\mult"   8901)
   ("\\ent"    8866)
   ("\\valid"  10003)
   ("\\diamond" 9671)
   ("\\box"    9633)
   ("\\bbox"   9632)
   ("\\later"  9655)
   ("\\pred"   966)
   ("\\and"    8743)
   ("\\or"     8744)
   ("\\comp"   8728)
   ("\\ccomp"  9678)
   ("\\all"    8704)
   ("\\ex"     8707)
   ("\\to"     8594)
   ("\\sep"    8727)
   ("\\lc"     8988)
   ("\\rc"     8989)
   ("\\Lc"     9121)
   ("\\Rc"     9124)
   ("\\lam"    955)
   ("\\empty"  8709)
   ("\\Lam"    923)
   ("\\Sig"    931)
   ("\\-"      8726)
   ("\\aa"     9679)
   ("\\af"     9711)
   ("\\auth"   9679)
   ("\\frag"   9711)
   ("\\iff"    8596)
   ("\\gname"  947)
   ("\\incl"   8828)
   ("\\latert" 9654)
   ("\\update" 8669)

   ;; accents (for iLöb)
   ("\\\"o" 246)

   ;; subscripts and superscripts
   ("^^+" 8314) ("___+" 8330) ("^^-" 8315)
   ("___0" 8320) ("___1" 8321) ("___2" 8322) ("___3" 8323) ("___4" 8324)
   ("___5" 8325) ("___6" 8326) ("___7" 8327) ("___8" 8328) ("___9" 8329)

   ("___a" 8336) ("___e" 8337) ("___h" 8341) ("___i" 7522) ("___k" 8342)
   ("___l" 8343) ("___m" 8344) ("___n" 8345) ("___o" 8338) ("___p" 8346)
   ("___r" 7523) ("___s" 8347) ("___t" 8348) ("___u" 7524) ("___v" 7525) ("___x" 8339))
  (mapc (lambda (x)
          (if (cddr x)
              (quail-defrule (cadr x) (car (cddr x)))))
                                        ; need to reverse since different emacs packages disagree on whether
                                        ; the first or last entry should take priority...
                                        ; see <https://mattermost.mpi-sws.org/iris/pl/46onxnb3tb8ndg8b6h1z1f7tny> for discussion
        (reverse (append math-symbol-list-basic math-symbol-list-extended))))

(use-package company-coq
  :after proof-general
  :diminish (company-coq-mode . " 🐔")
  :hook (coq-mode . company-coq-mode)
  :general (mjs-local-leader-def :keymaps 'coq-mode-map
             "f" '("Fold" . company-coq-fold)
             "g d" '("Definition" . company-coq-jump-to-definition)
             "g p" '("Proof" . company-coq-end-of-proof)
             "i o" '("Occurrences" . company-coq-occur)
             "I l" '("Lemma from Goal" . company-coq-lemma-from-goal)
             "I m" '("Match" . company-coq-insert-match-construct)
             "h" '(nil :which-key "Help")
             "h e" '("Error" . company-coq-document-error)
             "h E" '("Browse Errors" . company-coq-browse-error-messages)
             "h h" '("Coq Documentation" . company-coq-doc)
             "u" '("Unfold" . company-coq-unfold))
  :config (add-to-list 'company-coq-disabled-features 'company))

(use-package fstar-mode
  :mode ("\\.fst\\'" . fstar-mode)
  :commands fstar-mode
  :general (mjs-local-leader-def :keymaps 'fstar-mode-map
             "n" '("Assert Next Command" . fstar-subp-advance-next)
             "m" '("Undo Last Command" . fstar-subp-retract-last)
             "." '("Assert to Line" . fstar-subp-advance-or-retract-to-point)
             "l" '("Assert to Line Lax" . fstar-subp-advance-or-retract-to-point-lax)
             "x" '("Kill Background Z3" . fstar-subp-kill-one-or-many)
             "b" '("Eval Buffer Lax" . fstar-subp-advance-to-point-max-lax)
             "r" '("Reload Buffer" . fstar-subp-reload-to-point)
             "k" '("Kill Z3" . fstar-subp-kill-z3)

             "<" '("Jump Definition" . fstar-jump-to-definition)
             ">" '("Jump Related Error" . fstar-jump-to-related-error)
             "j" '(nil :which-key "Jump")
             "j j" '("Definition" . fstar-jump-to-definition)
             "j f" '("Definition (Frame)" . fstar-jump-to-definition-other-frame)
             "j w" '("Definition (Window)" . fstar-jump-to-definition-other-window)
             "j e" '("Related Error" . fstar-jump-to-related-error)
             "j F" '("Related Error (Frame)" . fstar-jump-to-related-error-other-frame)
             "j W" '("Related Error (Window)" . fstar-jump-to-related-error-other-window)
             "j d" '("Dependency" . fstar-visit-dependency)
             "j a" '("Interface" . fstar-visit-interface-or-implementation)
             "j u" '("Unprocessed" . fstar-subp-goto-beginning-of-unprocessed)

             "h" '(nil :which-key "Help")
             "h y" '("Yank Help" . fstar-copy-help-at-point)
             "h w" '("Wiki" . fstar-browse-wiki)
             "h W" '("Wiki (Browser)" . fstar-browse-wiki-in-browser)
             "h o" '("List Options" . fstar-list-options)
             "h p" '("Print Type" . fstar-quick-peek)
             "h ." '("Display Error" . display-local-help)

             "c" '("Insert Match" . fstar-insert-match-dwim)
             "e" '("Eval" . fstar-eval)
             "E" '("Eval Custom" . fstar-eval-custom)
             "s" '("Search" . fstar-search)
             "d" '("Docs" . fstar-doc)
             "p" '("Print Term" . fstar-print)
             "q" '("Quit" . fstar-subp-kill)
             "o" '("Outline" . fstar-outline))
  (:states 'insert :keymap 'fstar-mode-map
           "M-n" #'fstar-subp-advance-next
           "M-m" #'fstar-subp-retract-one
           "M-." #'fstar-subp-advance-or-retract-to-point)
  :custom-face (fstar-subp-overlay-processed-face ((t (:background
                                                       ,(plist-get base16-stylix-theme-colors :base02)))))
  )

(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package go-mode
  :mode ("\\.go\\'" . go-ts-mode)
  :hook (go-ts-mode . flycheck-mode))

(use-package go-eldoc
  :hook (go-ts-mode . go-eldoc-setup))

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-ts-mode . flycheck-golangci-lint-setup))

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :hook (go-ts-mode . eglot-ensure))

(use-package flycheck-eglot
  :hook (emacs-startup . global-flycheck-eglot-mode))

(use-package consult-eglot
  :commands consult-eglot-symbols)

(use-package sideline
  :diminish sideline-mode)

(use-package sideline-flycheck
  :hook (flycheck-mode . sideline-mode)
  :hook (flycheck-mode . sideline-flycheck-setup)
  :init (setq sideline-backends-right '(sideline-flycheck)))

(use-package flycheck-popup-tip
  ;; Leaving this here, but I think I'm going to use
  ;; sideline-flycheck for now to match my preferred
  ;; setup from neovim.
  :custom (flycheck-popup-tip-error-prefix "⚠ ")
  :config
  (defun mjs/disable-flycheck-popup-tip-maybe-a (&rest _)
    (if (and (bound-and-true-p evil-local-mode)
             (not (evil-emacs-state-p)))
        (evil-normal-state-p)
      (and (not (region-active-p))
           (not (bound-and-true-p company-backend))
           (not (ignore-errors (>= corfu--index 0))))))
  (advice-add 'flycheck-popup-tip-show-popup :before-while #'mjs/disable-flycheck-popup-tip-maybe-a))

(use-package apheleia
  :hook (emacs-startup . apheleia-global-mode)
  ;; For some reason, it isn't getting automatically turned on
  ;; in `go-ts-mode'
  :hook (go-ts-mode . apheleia-mode)
  :custom (apheleia-mode-lighter " 󰛖")
  :config
  (push '(go-ts-mode . (lsp gofmt)) apheleia-mode-alist)
  (cl-defun mjs/format-with-eglot (&key buffer callback &allow-other-keys)
    "Format the current buffer or region with any available eglot formatter.

Won't forward the buffer to chained formatters if successful."
    (with-current-buffer buffer
      (or (with-demoted-errors "%s"
            (always (eglot-format)))
          ;; try the next chained formatter(s)
          (ignore (funcall callback)))))
  (add-to-list 'apheleia-formatters '(lsp . mjs/format-with-eglot)))

(use-package hide-mode-line
  :commands hide-mode-line-mode)

(use-package vterm
  :commands (vterm-mode vterm vterm-other-window)
  :hook (vterm-mode . hide-mode-line-mode)
  :hook (vterm-mode . (lambda () (setq confirm-kill-processes nil
                                       hscroll-margin 0)))
  :hook (vterm-mode . (lambda () (hl-line-mode -1)))
  :general
  (mjs-leader-def :keymap 'override
    "t t" '("Terminal" . vterm))
  (:states 'insert :keymap 'vterm-mode-map
           "C-q" #'vterm-send-next-key)
  :custom ((vterm-kill-buffer-on-exit t)
           (vterm-max-scrollback 5000)))

;; Packages:
;; 
;; projectile
;; dirvish + diredfl
;; quickrun
;; magit
;; makefile-executor

;; evil-tex
;; adaptive-wrap
;; latex-preview-pane
;; company-auctex
;; company-reftex
;; company-math

;; fish-mode
;; company-shell?

;; Modules:
;;
;; file templates
;; fold
;; snippets
;; electric
;; undo -> vundo, undo-fu-session, undo-fu
;; pdf
;; tree sitter

;; magit :: https://github.com/doomemacs/doomemacs/blob/2b4f762b1e6a366cfcd9ffb3e17f127c64df2657/modules/ui/vc-gutter/config.el#L130
