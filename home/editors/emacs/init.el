;; -*- lexical-binding: t; -*-

(setq user-full-name "Matt Schwennesen"
      user-login-name "matt"
      user-real-login-name "mjs"
      user-mail-address "mjschwenne@gmail.com")

(require 'use-package)
(setq use-package-verbose t
	  use-package-always-defer t
	  use-package-always-ensure t
	  use-package-expand-minimally t
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

(use-package catppuccin-theme
  :defer nil
  :custom (catppuccin-flavor 'mocha)
  :config (load-theme 'catppuccin t))

;; (set-frame-parameter nil 'alpha-background 80)

;; (add-to-list 'default-frame-alist '(alpha-background . 80))

;; Setup autoloads, I'm currently targeting user facing functions not required to load the system
(add-to-list 'load-path "/home/mjs/.emacs.d/autoloads/")
(loaddefs-generate
 "/home/mjs/.emacs.d/autoloads"
 "/home/mjs/.emacs.d/autoloads/auto.el")
(require 'auto)

(use-package diminish
  :commands diminish)

(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode 1))

(use-package general :defer nil :config
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
  :diminish evil-mode
  :custom ((evil-want-keybinding nil)
           (evil-want-integration t)
           (evil-cross-lines t)
           (evil-echo-state nil)
           (evil-undo-system 'undo-redo))
  :init (evil-mode 1)
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
                      "C-c" #'evil-window-delete))

(use-package evil-org
  :after org evil
  :diminish evil-org-mode
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-set-initial-state 'org-agenda-mode 'normal))

(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :custom (evil-collection-setup-minibuffer t)
  :init (evil-collection-init))

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
  :config (global-evil-surround-mode 1))

(use-package evil-embrace
  :after evil-surround
  :config
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-escape
  :after evil
  :diminish evil-escape-mode
  :custom (evil-escape-key-sequence "jk")
  :init (evil-escape-mode))

(use-package evil-exchange
  :after evil
  :config (evil-exchange-install))

(use-package evil-goggles
  :after evil
  :diminish evil-goggles-mode
  :init (evil-goggles-mode 1))

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
  :diminish evil-snipe-local-mode
  :custom ((evil-snipe-smart-case t)
           (evil-snipe-tab-increment t))
  :init (evil-snipe-mode +1)
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
  :diminish vimish-fold-mode)

(use-package evil-vimish-fold
  :after vimish-fold
  :diminish evil-vimish-fold-mode
  :custom (evil-vimish-fold-targets-mode '(prog-mode conf-mode text-mode))
  :init (global-evil-vimish-fold-mode))

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
  ;; Enable most ligatures in text mode, but not all of them since some can mess with formatting
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

(use-package telephone-line
  :init (telephone-line-defsegment* mjs/popup-segment ()
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
                                        :foreground ,(catppuccin-get-color 'yellow)))
         (if modifed
             (propertize "󰆓 "
                         'face `(:inherit mode-line-emphasis
                                          :foreground ,(catppuccin-get-color 'red)))
           (propertize "󰆓 "
                       'face `(:inherit mode-line-emphasis
                                        :foreground ,(catppuccin-get-color 'green)))))
       (propertize (buffer-name) 'face 'mode-line-emphasis))))
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
  (telephone-line-mode +1)
  :custom (telephone-line-lhs
           '((evil . (telephone-line-evil-tag-segment))
             (accent . (telephone-line-process-segment
                        mjs/minor-mode-segment mjs/popup-segment))
             (nil . (mjs/buffer-mod))))
  (telephone-line-rhs
   '((nil . (telephone-line-misc-info-segment
             telephone-line-atom-encoding-segment))
     (accent . (telephone-line-major-mode-segment))
     (evil . (mjs/buffer-position))))
  :config
  (set-face-foreground 'telephone-line-evil
                       (catppuccin-get-color 'base))
  (set-face-background 'telephone-line-evil-normal
                       (catppuccin-get-color 'blue))
  (set-face-background 'telephone-line-evil-insert
                       (catppuccin-get-color 'green))
  (set-face-background 'telephone-line-evil-visual
                       (catppuccin-get-color 'mauve))
  (set-face-background 'telephone-line-evil-emacs
                       (catppuccin-get-color 'red))
  (set-face-background 'telephone-line-evil-operator
                       (catppuccin-get-color 'peach))
  (set-face-background 'telephone-line-evil-motion
                       (catppuccin-get-color 'pink))
  (set-face-attribute 'telephone-line-accent-active nil
                      :foreground (catppuccin-get-color 'text)
                      :background (catppuccin-get-color 'surface1))
  (set-face-attribute 'telephone-line-accent-inactive nil
                      :foreground (catppuccin-get-color 'text)
                      :background (catppuccin-get-color 'surface0))
  (set-face-attribute 'mode-line nil
                      :foreground (catppuccin-get-color 'text)
                      :background (catppuccin-get-color 'base)))

(setq display-line-numbers-type 'relative
      display-line-numbers-current-absolute t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

(use-package vi-tilde-fringe
  :diminish vi-tilde-fringe-mode
  :init (global-vi-tilde-fringe-mode))

(use-package nerd-icons)

(use-package dashboard
  :init (defun mjs/dashboard-next-items ()
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
  (dashboard-startup-banner 'logo)
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
  (dashboard-footer-icon (nerd-icons-sucicon "nf-custom-emacs")))

(use-package popper
  :custom (popper-reference-buffers '("\\*Messages\\*"
                                      "Output\\*$"
                                      "\\*Backtrace\\*"
                                      "\\*Scratch\\*"
                                      "\\*Warnings\\*"
                                      "\\*Async Shell Command\\*"
                                      "\\*Org Agenda\\*"
                                      ("\\*Org Links\\*" . hide)
                                      help-mode
                                      "\\*info\\*"
                                      helpful-mode
                                      compilation-mode
                                      "\\*Embark Actions\\*"
                                      cfw:details-mode
                                      ))
  (popper-display-control nil)
  (popper-mode-line "")
  (display-buffer-alist '(("\\*Org Links\\*" (display-buffer-at-bottom)
                           (window-height . 2))
                          ("\\*Org Agenda\\*" (display-buffer-in-direction)
                           (direction . left)
                           (window-width . 0.5))
                          ("\\*Embark Actions\\*" (display-buffer-at-bottom)
                           (window-height . 14))
                          ("^\\*[hH]elp" (display-buffer-reuse-mode-window
                                          display-buffer-in-direction)
                           (mode . (helpful-mode help-mode))
                           (direction . right)
                           (window-width 0.5))))
  :init (popper-mode +1)
  (popper-echo-mode +1)
  (mjs-leader-def :keymaps 'override
	"p" '(nil :which-key "Popup")
	"p p" '("Toggle Popup" . popper-toggle-latest)
	"t"   '(nil :which-key "Toggle")
	"t p" '("Toggle Popup" . popper-toggle-latest)
	"p c" '("Cycle Popups" . popper-cycle)
	"p m" '("Make Popup" . popper-toggle-type)
	"p k" '("Kill Popup" . popper-kill-latest-popup))
  :config (defun mjs/escape-popups ()
            (if (eq popper-popup-status 'popup)
                (popper-toggle-latest)))
  (advice-add #'evil-force-normal-state :after #'mjs/escape-popups))

(use-package helpful
  :init (mjs-leader-def :keymaps 'override
          "h" '(nil :which-key "Help")
          "h f" '("Callable" . helpful-callable)
          "h F" '("Function" . helpful-function)
          "h t" '("Text (Face)" . describe-face)
          "h m" '("Mode" . describe-mode)
          "h M" '("Macro" . helpful-macro)
          "h x" '("Command" . helpful-command)
          "h k" '("Key" . helpful-key)
          "h K" '("Kaymap" . describe-keymap)
          "h v" '("Variable" . helpful-variable)
          "h p" '("Thing-at-Point" . helpful-at-point)
          "h s" '("Symbol" . helpful-symbol)
          "h q" '("Kill Help Buffers" . helpful-kill-buffers)))

(use-package vertico
  :custom (vertico-resize t)
  (vertico-cycle t)
  (enable-recursive-minibuffers t)
  :init (defun crm-indicator (args)
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
  ;; While my understanding is that this should go in the :config
  ;; section, it doesn't seem to actually cause the mode to be
  ;; properly enabled when called from that section for some reason.
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
  :ensure nil)

(use-package vertico-reverse
  :after vertico
  :ensure nil
  :init (vertico-reverse-mode))

(use-package marginalia
  :general (:keymaps 'minibuffer-local-map
                     "M-A" #'marginalia-cycle)
  :init (marginalia-mode))

(use-package nerd-icons-completion
  :config (nerd-icons-completion-mode))

(use-package orderless
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
	         "s y r" '("Replace" . consult-yank-replace))
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
  :custom (corfu-cycle t)
  (completion-cycle-threshold 3)
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-preselect 'prompt)
  :general (:keymaps 'corfu-map
                     "TAB" #'corfu-next
                     [tab] #'corfu-next
                     "S-TAB" #'corfu-previous
                     [backtab] #'corfu-previous)
  :init (global-corfu-mode))

(use-package cape
  :after corfu
  :hook (prog-mode . (lambda ()
                       (add-to-list 'completion-at-point-functions #'cape-keyword)))
  (text-mode . (lambda ()
                 (add-to-list 'completion-at-point-functions #'cape-dict)
                 (add-to-list 'completion-at-point-functions #'cape-dabbrev)))
  (org-mode . (lambda ()
                (add-to-list 'completion-at-point-functions #'cape-elisp-block)))
  :config (add-to-list 'completion-at-point-functions #'cape-file))

(use-package company-wordfreq
  :after cape
  :init (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-wordfreq)))

(global-prettify-symbols-mode +1)

(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "󰘧 Emacs Lisp")))

(use-package lispy
  :diminish "'󰅲"
  :init (mjs-local-leader-def :keymaps 'emacs-lisp-mode-map
          "l" '("Lispy" . lispy-mode)))

(diminish 'eldoc-mode)

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

  (custom-set-variables '(org-emphasis-alist
                          (quote (("*" bold)
                                  ("/" italic)
                                  ("_" underline)
                                  ("=" org-verbatim verbatim)
                                  ("~" org-code verbatim)
                                  ("+"
                                   (:strike-through t))
                                  ("!"
                                  (:overline t))))))

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
    "h b"    '("Mark BLOCKED" . (lambda () (interactive) (org-todo "BLOCKED")))
    "h d"    '("Mark DONE" . (lambda () (interactive) (org-todo "DONE")))
    "h H"    '("Toggle Heading" . org-toggle-heading)
    "h h"    '("Promote Heading" . org-metaright)
    "h k"    '("Mark KILLED" . (lambda () (interactive) (org-todo "KILLED")))
    "h l"    '("Demote Heading" . org-metaleft)
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
              ("kb")
              (:grouptags)
              ("comp_op")
              ("math")
              ("processes")
              ("prog")
              (:endgrouptag)
              (:startgrouptag)
              ("math")
              (:grouptags)
              ("combinatorics")
              ("lin_alg")
              ("modeling")
              ("stats")
              ("opt")
              ("complexity")
              (:endgrouptag)
              (:startgrouptag)
              ("prog")
              (:grouptags)
              ("C")
              ("lisp")
              ("pl_theory")
              (:endgrouptag)
              (:startgrouptag)
              ("comp_op")
              (:grouptags)
              ("net")
              (:endgrouptag)
              (:startgrouptag)
              ("stats")
              (:grouptags)
              ("conf_ints")
              ("hyp_tests")
              ("prob")
              ("regression")
              (:endgrouptag)
              (:startgrouptag)
              ("opt")
              (:grouptags)
              ("lin_prog")
              (:endgrouptag)
              (:startgrouptag)
              ("modeling")
              (:grouptags)
              ("association_analysis")
              ("classification")
              ("clustering")
              ("decision_tree")
              ("info_ret")
              ("recommender")
              ("text_mining")
              ("regression")
              (:endgrouptag)
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
              (:endgrouptag)
              (:startgrouptag)
              ("great_basin")
              (:grouptags)
              ;; Individual factions
              ("andorr_again")
              ("arendelle")
              ("atreides")
              ("atrxous")
              ("caerwent")
              ("death_raising")
              ("eichen")
              ("galimatias")
              ("lake")
              ("koncord")
              ("kudw")
              ("niven")
              ("roks")
              ("syndicate")
              ("trobreryn")
              ("vi")
              ("xylte")
              (:endgrouptag)
              (:startgroup)
              ("character")
              (:grouptags)
              ("player")
              ("npc")
              (:endgroup)
              (:startgrouptag)
              ("npc")
              (:grouptags)
              ("deity")
              ("elemental_beast")
              ("herald")
              (:endgrouptag)
              (:startgrouptag)
              ("roks")
              (:grouptags)
              ("valt")
              (:endgrouptag)))
           (org-agenda-start-with-log-mode t)
           (org-log-done 'time)
           (org-log-into-drawer t)
           (org-treat-insert-todo-heading-as-state-change t)
           (org-agenda-hide-tags-regexp ".")
           (org-agenda-files (list (concat org-directory "agenda/")))
           (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "BLOCKED(b)" "|" "DONE(d)" "KILLED(k)")))
           (org-agenda-custom-commands
            '(("g" "Get Things Done"
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
           (org-refile-targets '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))
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
              ("bot" "\\bot" nil "&x22A5" "_|_" "_|_" "⊥")
              ("top" "\\top" nil "&x22A4" "T" "T" "⊤")
              ("lightning" "\\lightning" nil "&x21AF" "</" "</" "↯")
              ("qed" "\\qedsymbol" nil "&x25A1" "[]" "[]" "☐")))
           (org-confirm-babel-evaluate nil))
  :diminish ((org-cdlatex-mode . " ")
             (auto-fill-function . ""))
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

         (org-mode . org-indent-mode)
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
          ("e" "Etera Session" entry
           (file "ttrpg/games/etera/notes.org")
           "\n* Session %<%Y-%m-%d>\n\n%?\n"
           :jump-to-captured t
           :immediate-finish t)
          ("g" "Graves Session" entry
           (file "ttrpg/games/graves-and-groves/sessions.org")
           "\n\n* Session %<%Y-%m-%d>\n\n%?\n"
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
           (file "ttrpg/games/obscured-realms/sessions.org")
           "\n* Session %<%Y-%m-%d>\n\n%?"
           :jump-to-captured t
           :immediate-finish t)))
  (set-face-foreground 'org-verbatim (catppuccin-get-color 'mauve))
  (set-face-attribute 'org-quote nil
                      :background (catppuccin-get-color 'mantle)
                      :extend t)
  (set-face-foreground 'org-table (catppuccin-get-color 'subtext1))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (dot . t)
     (emacs-lisp . t)
     (latex . t)
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
  (add-to-list 'org-latex-packages-alist '("" "nicematrix" t))
  (add-to-list 'org-latex-packages-alist '("" "amsthm" t))
  (add-to-list 'org-latex-packages-alist '("" "cancel" t)))


(use-package org-habit
  :ensure nil
  :config
  (set-face-foreground 'org-habit-ready-face (catppuccin-get-color 'base))
  (set-face-background 'org-habit-ready-face (catppuccin-get-color 'green))
  (set-face-background 'org-habit-ready-future-face (catppuccin-get-color 'green))
  (set-face-background 'org-habit-clear-face (catppuccin-get-color 'blue))
  (set-face-background 'org-habit-clear-future-face (catppuccin-get-color 'blue))
  (set-face-background 'org-habit-alert-face (catppuccin-get-color 'yellow))
  (set-face-background 'org-habit-alert-future-face (catppuccin-get-color 'yellow))
  (set-face-background 'org-habit-overdue-face (catppuccin-get-color 'red))
  (set-face-background 'org-habit-overdue-future-face (catppuccin-get-color 'red)))

(use-package ob-rust
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
  :init (mjs-local-leader-def :keymaps 'org-mode-map
          "o" '("Toggle Olivetti" . olivetti-mode)))

(use-package flycheck
  :diminish " 󰨮"
  :custom (flycheck-global-modes '(not org-capture-mode)))

(use-package flycheck-vale
  :config (flycheck-vale-setup))

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
  :custom (org-superstar-leading-bullet nil)
  (org-hide-leading-stars t)
  :hook (org-mode . org-superstar-mode))

(use-package svg-tag-mode
  :hook org-mode
  :custom (svg-tag-tags
           '(("^\\*+ \\(TODO\\|BLOCKED\\)" .
              ((lambda (tag)
                 (svg-lib-tag tag nil
                              :margin 0
                              :font-family "JetBrainsMono Nerd Font"
                              :font-weight 500
                              :background (catppuccin-get-color 'peach)
                              :foreground (catppuccin-get-color 'base)
                              ))))
             ("^\\*+ \\(NEXT\\)" . ((lambda (tag)
                                      (svg-lib-tag tag nil
                                                   :margin 0
                                                   :font-family "JetBrainsMono Nerd Font"
                                                   :font-weight 500
                                                   :background (catppuccin-get-color 'green)
                                                   :foreground (catppuccin-get-color 'base)))))
             ("^\\*+ \\(DONE\\|KILLED\\)" . ((lambda (tag)
                                               (svg-lib-tag tag nil
                                                            :margin 0
                                                            :font-family "JetBrainsMono Nerd Font"
                                                            :font-weight 500
                                                            :foreground (catppuccin-get-color 'overlay0)))))
             ("\\(:TODO:\\)" . ((lambda (tag)
                                  (svg-lib-tag (substring tag 1 5) nil
                                               :margin 0
                                               :font-family "JetBrainsMono Nerd Font"
                                               :font-weight 500
                                               :background (catppuccin-get-color 'peach)
                                               :foreground (catppuccin-get-color 'base)))))
             )))

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
  (add-hook 'pdf-annot-edit-contents-minor-mode-hook #'mjs/pdf-annot-update-header)
  (pdf-tools-install-noverify))

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


(use-package vulpea
  :hook (org-roam-db-autosync-mode . vulpea-db-autosync-enable))

(use-package org-cliplink
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
                 (file ,(concat org-directory "recipes.org"))
                 ,(concat "* %^{Recipe title: }\n"
                          "  :PROPERTIES:\n"
                          "  :source:\n"
                          "  :export_hugo_bundle:"
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
  :custom ((org-cite-global-bibliography '("~/Documents/zotero.bib"))
           (org-cite-csl-styles-dir "~/Zotero/styles/")
           (org-cite-export-processors '((latex biblatex)
                                         (md . (csl "ieee.csl"))
                                         (t . (csl "ieee.csl")))))
  :general (mjs-local-leader-def :keymaps 'org-mode-map
             "C" '("Citation" . org-cite-insert))
  :custom-face (org-cite ((t (:foreground ,(catppuccin-get-color 'green)))))
               (org-cite-key ((t (:foreground ,(catppuccin-get-color 'green) :slant italic)))))

(use-package citar
  :custom (citar-bibliography org-cite-global-bibliography)
  :hook ((LaTeX-mode . citar-capf-setup)
         (org-mode . citar-capf-setup))
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
  :after oc
  :custom ((org-cite-insert-processor 'citar)
           (org-cite-follow-processor 'citar)
           (org-cite-activate-processor 'citar)))

(use-package citar-embark
  :after citar
  :diminish citar-embark-mode
  :no-require
  :init (setq citar-at-point-function 'embark-act)
  :hook ((org-mode . citar-embark-mode)
         (LaTeX-mode . citar-embark-mode)))

(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . (lambda () (setq mode-name " LaTeX")))
         (LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . TeX-fold-mode)
         (LaTeX-mode . TeX-PDF-mode)
         (LaTeX-mode . mjs/preview-scale-adjustment)
         (LaTeX-mode . auto-fill-mode)
         (after-save . (lambda ()
		  				 (when (eq major-mode 'latex-mode)
						   (TeX-command-run-all nil)))))
  :custom ((TeX-newline-function #'reindent-then-newline-and-indent)
           (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)
                                    ("Zathura" "zathura --synctex-forward :: %o")))
           (TeX-view-program-selection '((output-pdf "PDF Tools")))
           (TeX-source-correlate-start-server t)
           (TeX-save-query nil)
           (TeX-engine 'luatex)
           (TeX-parse-self t)
           (TeX-auto-save t))
  :config (general-define-key :states '(insert normal) :map 'LaTeX-mode-map
                              "C-S-e" #'mjs/latex-math-from-calc)
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
         (post-self-insert . mjs/yas-try-expanding-auto-snippets))
  :custom (yas-triggers-in-field t)
  (yas-snippets-dirs '("~/.emacs.d/snippets"))
  (yas-verbosity 4)
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
                                   ("pmatrix" env-on)))
  (cdlatex-env-alist '(("proof" "\\begin{proof}\n?\n\\end{proof}" nil)
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

(use-package ledger-mode
  :custom (ledger-clear-whole-transactions 1)
  (ledger-mode-should-check-version nil))

(use-package evil-ledger
  :diminish evil-ledger-mode
  :hook (ledger-mode . evil-ledger-mode))

(use-package calfw
  :custom (calendar-week-start-day 1)
  :general (mjs-leader-def
             "C"     '(nil :which-key "Calendar")
             "C a"   '("Open Agenda on Day" . cfw:org-open-agenda-day)
             "C d"   '("GOTO Date" . cfw:navi-goto-date-command)
             "C m"   '("First Day" . cfw:navi-goto-first-date-command)
             "C M"   '("Last Day" . cfw:navi-goto-last-date-command)
             "C q"   '("Quit Calendar" . cfw:org-clean-exit)
             "C r"   '("Rebuild Calendar" . cfw:refresh-calendar-buffer)
             "C v"   '(nil :which-key "Calender Views")
             "C v m" '("Month" . (lambda ()
                                   (interactive)
                                   (cfw:open-calendar-buffer
                                    :contents-sources (list (cfw:org-create-source))
                                    :view 'month)))
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
  (cfw:face-title ((t  :foreground ,(catppuccin-get-color 'yellow)
                       :weight bold
                       :height 2.0)))
  (cfw:face-header ((t :foreground ,(catppuccin-get-color 'yellow)
                       :weight bold)))
  (cfw:face-sunday ((t :foreground ,(catppuccin-get-color 'red)
                       :weight bold)))
  (cfw:face-saturday ((t :foreground ,(catppuccin-get-color 'blue)
                         :weight bold)))
  (cfw:face-holiday ((t :background ,(catppuccin-get-color 'mantle)
                        :foreground ,(catppuccin-get-color 'red)
                        :weight bold)))
  (cfw:face-grid ((t :foreground ,(catppuccin-get-color 'subtext0))))
  (cfw:face-default-content ((t :foreground ,(catppuccin-get-color 'green))))
  (cfw:face-periods ((t :foreground ,(catppuccin-get-color 'sky))))
  (cfw:face-day-title ((t :background ,(catppuccin-get-color 'mantle))))
  (cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
  (cfw:face-annotation ((t :foreground ,(catppuccin-get-color 'text)
                           :inherit cfw:face-day-title)))
  (cfw:face-disable ((t :foreground ,(catppuccin-get-color 'subtext0)
                        :inherit cfw:face-day-title)))
  (cfw:face-today-title ((t :foreground ,(catppuccin-get-color 'base)
                            :background ,(catppuccin-get-color 'green)
                            :weight bold)))
  (cfw:face-today ((t :background: ,(catppuccin-get-color 'mantle)
                      :weight bold)))
  (cfw:face-select ((t :background ,(catppuccin-get-color 'overlay2))))
  (cfw:face-toolbar ((t :foreground ,(catppuccin-get-color 'sapphire)
                        :background ,(catppuccin-get-color 'sapphire))))
  (cfw:face-toolbar-button-off ((t :foreground ,(catppuccin-get-color 'subtext0)
                                   :weight bold)))
  (cfw:face-toolbar-button-on ((t :foreground ,(catppuccin-get-color 'subtext1)
                                   :weight bold)))
  :config
  (evil-set-initial-state 'cfw:calendar-mode 'normal))

(use-package calfw-org
  :after calfw
  :general (mjs-leader-def
             "C c"   '("Open Calendar" . cfw:open-org-calendar)))

(use-package calfw-blocks
  :ensure nil
  :after calfw
  :general (mjs-leader-def
             "C v d" '("Day" . (lambda ()
                                 (interactive)
                                 (cfw:open-calendar-buffer
                                  :contents-sources (list (cfw:org-create-source))
                                  :view 'block-day)))
             "C v D" '("3-Day" . (lambda ()
                                   (interactive)
                                   (cfw:open-calendar-buffer
                                    :contents-sources (list (cfw:org-create-source))
                                    :view 'block-3-day)))
             "C v t" '("Two Weeks" . (lambda ()
                                       (interactive)
                                       (cfw:open-calendar-buffer
                                        :contents-sources (list (cfw:org-create-source))
                                        :view 'transpose-14-day)))
             "C v w" '("Block Week" . (lambda ()
                                        (interactive)
                                        (cfw:open-calendar-buffer
                                         :contents-sources (list (cfw:org-create-source))
                                         :view 'block-week)))
             "C v W" '("Week" . (lambda ()
                                  (interactive)
                                  (cfw:open-calendar-buffer
                                   :contents-sources (list (cfw:org-create-source))
                                   :view 'transpose-two-weeks))))
  :custom ((calfw-blocks-earliest-visible-time (8 30))
           (calfw-blocks-lines-per-hour 2)))

(use-package direnv
  :general (mjs-leader-def :keymaps 'override
             "d" '("Direnv Allow" . direnv-allow))
  :init
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
