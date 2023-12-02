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
              make-backup-files nil ; Do not create backup files
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
              scroll-margin 8
              hscroll-margin 8
              scroll-conservatively 101)

(use-package esup
  :commands esup)

(use-package diminish
  :commands diminish)

(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode 1))

(use-package general :defer nil)
  
;; For some reason, the nixos emacs overlay setup doesn't like 
;; this block inside the :config block of the general use-package
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
(general-unbind :states '(insert motion visual) :keymaps 'global "M-SPC")

(use-package evil
  :diminish evil-mode
  :custom (evil-want-keybinding nil)
          (evil-want-integration t)
          (evil-cross-lines t)
          (evil-echo-state nil)
          (evil-undo-system 'undo-redo)
  :init (evil-mode 1)
  :config (evil-set-initial-state 'org-agenda-mode 'normal))

;; Make evil search similar to vim
(evil-select-search-module 'evil-search-module 'evil-search)

;; Rebind `universal-argument` to 'C-M-u' since 'C-u' now scrolls the buffer
(global-set-key (kbd "C-M-u") 'universal-argument)

;; Use visual line movements by default
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)
(general-define-key :states '(normal motion) :keymaps 'override
                    "C-h" #'evil-window-left
                    "C-j" #'evil-window-down
                    "C-k" #'evil-window-up
                    "C-l" #'evil-window-right
                    "C-c" #'evil-window-delete)

(use-package evil-org
     :after org evil
     :diminish evil-org-mode
     :hook (org-mode . evil-org-mode)
           (org-agenda-mode . (lambda ()
                                (require 'evil-org-agenda)
                                ;; This is the heart of  (evil-org-agenda-set-keys) but without the stupid decision to set the default state to motion state
                                ;; Horizontal movements have little use, thus we can override "f" and "t".
                                ;; "w", "b", "e", "ge" and their upcase counterparts are preserved.
                                (evil-define-key 'motion org-agenda-mode-map
                                    ;; Unused keys: D, X

                                    ;; open
                                    (kbd "<tab>") 'org-agenda-goto
                                    (kbd "S-<return>") 'org-agenda-goto
                                    (kbd "g TAB") 'org-agenda-goto
                                    (kbd "RET") 'org-agenda-switch-to
                                    (kbd "M-RET") 'org-agenda-recenter

                                    (kbd "SPC") 'org-agenda-show-and-scroll-up
                                    (kbd "<delete>") 'org-agenda-show-scroll-down
                                    (kbd "<backspace>") 'org-agenda-show-scroll-down

                                    ;; motion
                                    "j" 'org-agenda-next-line
                                    "k" 'org-agenda-previous-line
                                    "gj" 'org-agenda-next-item
                                    "gk" 'org-agenda-previous-item
                                    "gH" 'evil-window-top
                                    "gM" 'evil-window-middle
                                    "gL" 'evil-window-bottom
                                    (kbd "C-j") 'org-agenda-next-item
                                    (kbd "C-k") 'org-agenda-previous-item
                                    (kbd "[[") 'org-agenda-earlier
                                    (kbd "]]") 'org-agenda-later

                                    ;; manipulation
                                    ;; We follow standard org-mode bindings (not org-agenda bindings):
                                    ;; <HJKL> change todo items and priorities.
                                    ;; M-<jk> drag lines.
                                    ;; M-<hl> cannot demote/promote, we use it for "do-date".
                                    "J" 'org-agenda-priority-down
                                    "K" 'org-agenda-priority-up
                                    "H" 'org-agenda-do-date-earlier
                                    "L" 'org-agenda-do-date-later
                                    "t" 'org-agenda-todo
                                    (kbd "M-j") 'org-agenda-drag-line-forward
                                    (kbd "M-k") 'org-agenda-drag-line-backward
                                    (kbd "C-S-h") 'org-agenda-todo-previousset ; Original binding "C-S-<left>"
                                    (kbd "C-S-l") 'org-agenda-todo-nextset ; Original binding "C-S-<right>"

                                    ;; undo
                                    "u" 'org-agenda-undo

                                    ;; actions
                                    "dd" 'org-agenda-kill
                                    "dA" 'org-agenda-archive
                                    "da" 'org-agenda-archive-default-with-confirmation
                                    "ct" 'org-agenda-set-tags
                                    "ce" 'org-agenda-set-effort
                                    "cT" 'org-timer-set-timer
                                    "i" 'org-agenda-diary-entry
                                    "a" 'org-agenda-add-note
                                    "A" 'org-agenda-append-agenda
                                    "C" 'org-agenda-capture

                                    ;; mark
                                    "m" 'org-agenda-bulk-toggle
                                    "~" 'org-agenda-bulk-toggle-all
                                    "*" 'org-agenda-bulk-mark-all
                                    "%" 'org-agenda-bulk-mark-regexp
                                    "M" 'org-agenda-bulk-unmark-all
                                    "x" 'org-agenda-bulk-action

                                    ;; refresh
                                    "gr" 'org-agenda-redo
                                    "gR" 'org-agenda-redo-all

                                    ;; quit
                                    "ZQ" 'org-agenda-exit
                                    "ZZ" 'org-agenda-quit

                                    ;; display
                                    ;; "Dispatch" can prefix the following:
                                    ;; 'org-agenda-toggle-deadlines
                                    ;; 'org-agenda-toggle-diary
                                    ;; 'org-agenda-follow-mode
                                    ;; 'org-agenda-log-mode
                                    ;; 'org-agenda-entry-text-mode
                                    ;; 'org-agenda-toggle-time-grid
                                    ;; 'org-agenda-day-view
                                    ;; 'org-agenda-week-view
                                    ;; 'org-agenda-year-view
                                    "gD" 'org-agenda-view-mode-dispatch
                                    "ZD" 'org-agenda-dim-blocked-tasks

                                    ;; filter
                                    "sc" 'org-agenda-filter-by-category
                                    "sr" 'org-agenda-filter-by-regexp
                                    "se" 'org-agenda-filter-by-effort
                                    "st" 'org-agenda-filter-by-tag
                                    "s^" 'org-agenda-filter-by-top-headline
                                    "ss" 'org-agenda-limit-interactively
                                    "S" 'org-agenda-filter-remove-all

                                    ;; clock
                                    "I" 'org-agenda-clock-in ; Original binding
                                    "O" 'org-agenda-clock-out ; Original binding
                                    "cg" 'org-agenda-clock-goto
                                    "cc" 'org-agenda-clock-cancel
                                    "cr" 'org-agenda-clockreport-mode

                                    ;; go and show
                                    "." 'org-agenda-goto-today ; TODO: What about evil-repeat?
                                    "gc" 'org-agenda-goto-calendar
                                    "gC" 'org-agenda-convert-date
                                    "gd" 'org-agenda-goto-date
                                    "gh" 'org-agenda-holidays
                                    "gm" 'org-agenda-phases-of-moon
                                    "gs" 'org-agenda-sunrise-sunset
                                    "gt" 'org-agenda-show-tags

                                    "p" 'org-agenda-date-prompt
                                    "P" 'org-agenda-show-the-flagging-note

                                    ;; 'org-save-all-org-buffers ; Original binding "C-x C-s"

                                    ;; Others
                                    "+" 'org-agenda-manipulate-query-add
                                    "-" 'org-agenda-manipulate-query-subtract)
                                )))

   (use-package evil-collection
     :after evil
     :diminish evil-collection-unimpaired-mode
     :custom (evil-collection-setup-minibuffer t)
     :init (evil-collection-init))

   (use-package evil-args
     :after evil
     :custom (evil-args-delimiters '(" ")) ; defualt value is '("," ";")
                                           ; may want to investiage major-mode dependent
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
     :config (add-hook 'org-mode-hook 'embrace-org-mode-hook)
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
     :after evil
     :commands evil-lion-left evil-lion-right
     :general (:states 'normal
                       "g a " #'evil-lion-left
                       "g A " #'evil-lion-right))

   ;; TODO move this from the default keymap to match nvim or vice versa
   (use-package evil-nerd-commenter
     :after evil
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
     :custom (evil-snipe-smart-case t)
             (evil-snipe-tab-increment t)
     :init (evil-snipe-mode +1)
           (evil-snipe-override-mode +1))

   (use-package evil-visualstar
     :commands (evil-visualstar/begin-search
                evil-visualstar/begin-search-forward
                evil-visualstar/begin-search-backwards)
     :custom (evil-visualstar/persistent nil)
     :init (evil-define-key* 'visual 'global
             "*" #'evil-visualstar/begin-search-forward
             "#" #'evil-visualstar/begin-search-backward))

   (use-package vimish-fold
     :after evil
     :diminish vimish-fold-mode)

   (use-package evil-vimish-fold
     :after vimish-fold
     :diminish evil-vimish-fold-mode
     :init (setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
           (global-evil-vimish-fold-mode))

(defun mjs/restart-server ()
  "Restart the emacs daemon"
  (interactive)
  (server-force-delete)
  (while (server-running-p)
    (sleep-for 1))
  (server-start))

(defun mjs/kill-all-buffers (&optional buffer-list interactive)
  "Kill all buffers and close their windows"
  (interactive (list (buffer-list) t))
  (if (null buffer-list)
      (message "No buffers to kill")
    (save-some-buffers)
    (delete-other-windows)
    (when (memq (current-buffer) buffer-list)
      ;; :TODO: replace with switching to dashboard buffer
      (switch-to-buffer (get-scratch-buffer-create)))
    (mapc #'kill-buffer buffer-list)
    (message "Killed %d buffers"
             (- (length buffer-list)
                (length (cl-remove-if-not #'buffer-live-p buffer-list))))))

(use-package restart-emacs
  :commands restart-emacs)

(defun mjs/restart-emacs ()
  (interactive)
  (restart-emacs (list "--with-profile" chemacs-profile-name)))

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

(defun mjs/switch-buffer ()
  (interactive)
  (consult-buffer '(consult--source-buffer)))

(defun mjs/copy-this-file (new-path &optional force-p)
  "Copy the current buffer's file to NEW-PATH then open NEW-PATH.

If FORCE-P, overwrite the destination file, should it exist, without
confirmation. FORCE-P can also be invoked with the prefix argument."
  (interactive
   (list (read-file-name "Copy file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (copy-file old-path new-path (or force-p 1))
    (find-file new-path)
    (message "File copied to %S" (abbreviate-file-name new-path))))

(defun mjs/delete-this-file (&optional path force-p)
  "Delete PATH and kill any open buffers referencing it.

 If PATH is not specified, use the current buffer's file.

 If FORCE-P, delete without confirmation."
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         current-prefix-arg))
  ;; :TODO: I think that this can be simpiled quite a bit...
  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (and path (abbreviate-file-name path))))
    (unless path
      (user-error "Buffer is not visiting any file."))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (let* ((buf (current-buffer))
           (windows (get-buffer-window-list buf)))
      (unwind-protect
          (progn (delete-file path t) t)
        (if (file-exists-p path)
            (error "Failed to delete %S" short-path)
          (dolist (window (cl-remove-if-not #'window-live-p windows))
            (with-selected-window window
              ;; In order to prevent the other windows from displaying
              ;; things like Help buffers, limit the possible options
              ;; the window can fall back onto to buffers with files as
              ;; this is the buffers I am activly working on with an
              ;; overwhelming majority.
              (unless (buffer-file-name (window-buffer))
                (previous-buffer)
                (unless (buffer-file-name (window-buffer))
                  ;; :TODO: replace with switching to dashboard buffer
                  (switch-to-buffer (get-scratch-buffer-create))))))
          ;; If recentf mode is enabled, pruge the file from the list of
          ;; recent files.
          (when (bound-and-true-p recentf-mode)
            (recentf-remove-if-non-kept path))
          (message "Deleted %S" short-path))))))

(defun mjs/move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH.

If FORCE-P, overwrite the destination file, should it exist,
without confirmation. To invoke FORCE-P interactively, call with
a prefix argument."
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file."))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (when (directory-name-p new-path)
      (setq new-path (concat new-path (file-name-nondirectory old-path))))
    (make-directory (file-name-directory new-path) 't)
    (rename-file old-path new-path (or force-p 1))
    (set-visited-file-name new-path t t)
    (when (bound-and-true-p recentf-mode)
      (recentf-remove-if-non-kept path))
    (message "File moved to %S" (abbreviate-file-name new-path))))

(defun mjs/yank-buffer-path (&optional root)
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or (buffer-file-name (buffer-base-buffer))
                        (bound-and-true-p list-buffers-directory)))
      (let ((path (abbreviate-file-name
                   (if root
                       (file-relative-name filename root)
                     filename))))
        (kill-new path)
        (if (string= path (car kill-ring))
            (message "Copied path: %s" path)
          (user-error "Could not copy filename in current buffer.")))
    (error "Couldn't find filename in current buffer.")))

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
                                        "emacs.org"))))
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
                                        (mjs/yank-buffer-path
                                         default-director))))

(global-auto-revert-mode +1)
(diminish 'auto-revert-mode)

(use-package emojify
  :hook (after-init . global-emojify-mode))

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
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
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
  (ligature-set-ligatures 'text-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "||=" "||>"
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

(use-package catppuccin-theme
  :defer nil
  :custom (catppuccin-flavor 'mocha)
  :config (load-theme 'catppuccin t))

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
        (telephone-line-mode +1)
        :custom (telephone-line-lhs
                 '((evil . (telephone-line-evil-tag-segment))
                   (accent . (telephone-line-process-segment
                              telephone-line-minor-mode-segment mjs/popup-segment))
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
        (set-face-attribute 'telephone-line-accent-active t
                            :foreground
                            (catppuccin-get-color 'text)
                            :background
                            (catppuccin-get-color 'surface1))
        (set-face-attribute 'mode-line t
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
    "d" '("Open Dashboard" . dashboard-open))
  :commands (dashboard-jump-to-agenda dashboard-jump-to-recents)
  :general 
  (:keymaps #'dashboard-mode-map :states 'normal
            "a" #'dashboard-jump-to-agenda
            "r" #'dashboard-jump-to-recents)
  :custom (dashboard-startup-banner 'logo)
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
  )

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
                                      compilation-mode))
  (popper-display-control nil)
  (popper-mode-line "")
  (display-buffer-alist '(("\\*Org Links\\*" (display-buffer-at-bottom)
                           (window-height . 2))
                          ("\\*Org Agenda\\*" (display-buffer-in-direction)
                           (direction . left)
                           (window-width . 0.5))
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
  (mjs-leader-def :keymaps 'override
	"c"     '(nil :which-key "Consult")
	"c b"   '("Buffer" . consult-buffer)
	"c c"   '("Complex Command" . consult-complex-command)
	"c e"   '("Compile Error" . consult-compile-error)
	"c f"   '("Recent Files" . consult-recent-file)
	"c g"   '(nil :which-key "External Search")
	"c g g" '("Grep" . consult-grep)
	"c g r" '("Ripgrep" . consult-ripgrep)
	"c g f" '("Find" . consult-find)
	"c g l" '("Locate" . consult-locate)
	"c h"   '(nil :which-key "Help")
	"c h i" '("Emacs Info" . consult-info)
	"c h m" '("UNIX Manual" . consult-man)
	"c m"   '(nil :which-key "Modes")
	"c m m" '("Minor Modes" . consult-minor-mode-menu)
	"c m c" '("Mode Commands" . consult-mode-command)
	"c M"   '("Macro" . consult-kmacro)
	"c n"   '(nil :which-key "Navigation")
	"c n i" '("imenu" . consult-imenu)
	"c n I" '("Multi-imenu" . consult-imenu-multi)
	"c n l" '("Goto Line" . consult-goto-line)
	"c n m" '("Goto Mark" . consult-mark)
	"c n M" '("Goto Global Mark" . consult-global-mark)
	"c n o" '("Outline" . consult-outline)
	"c o"   '(nil :which-key "Org")
	"c o a" '("Agenda" . consult-org-agenda)
	"c o h" '("Heading" . consult-org-heading)
	"c r"   '("Registers" . consult-register)
	"c s"   '(nil :which-key "Search")
	"c s l" '("Line" . consult-line)
	"c s m" '("Multi-buffer line" . consult-line-multi)
	"c t"   '("Themes" . consult-theme)
	"c y"   '(nil :which-key "Yank")
	"c y k" '("Kill Ring" . consult-yank-from-kill-ring)
	"c y p" '("Pop" . consult-yank-pop)
	"c y r" '("Replace" . consult-yank-replace))
  :custom (register-preview-function #'consult-register-format)
  (register-preview-delay 0.5)
  :config 
  (advice-add #'register-preview :override #'consult-register-window))

(use-package embark
  :commands embark-act embark-dwim embark-bindings
  :init (mjs-leader-def :keymaps 'override
          "E" '(nil :which-key "Embark")
          "E a" '("Embark Act"       . embark-act)
          "E A" '("Embark DWIM"      . embark-dwim)
          "h e" '("Emark Bindings" . embark-bindings))
  :custom (prefix-help-command #'embark-prefix-help-command)
  (embark-prompter #'embark-completing-read-prompter))
                                        ; :init (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target))

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

(defun mjs/org-babel-remove-result-blocks (remove-all)
  (interactive "P")
  (let ((pos (point)))
    (org-babel-map-src-blocks nil
      (if (or remove-all (< pos end-block))
          (org-bable-remove-results)))))

(mjs-leader-def :keymaps 'override
  "a"   '("Agenda" . org-agenda)
  "A"   '("GTD Agenda" . (lambda () (interactive) (org-agenda nil "g")))
  "n"   '(nil :which-key "Notes")
  "n a" '("Agenda" . org-agenda)
  "n c" '("GOTO Clock" . org-clock-goto)
  "n l" '("Store Link" . org-store-link)
  "n R" '("Refile DWIM" . mjs/org-refile-dwim)
  "n s" '("Search Notes" . org-search-view)
  "n t" '("TODO List" . org-todo-list)
  "n T" '("Tag View" . org-tags-view))

(use-package org
  :init (mjs-local-leader-def :states '(normal insert visual motion)
          :keymaps 'org-mode-map
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
          "c"      '(nil :which-key "Clock")
          "c e"    '("Set Effort" . org-set-effort)
          "c E"    '("Increase Effort" . org-inc-effort)
          "c i"    '("Clock-in" . org-clock-in)
          "c o"    '("Clock-out" . org-clock-out)
          "c g"    '("Goto Current Clock" . org-clock-goto)
          "c c"    '("Cancel Clock" . org-clock-cancel)
          "c r"    '("Report" . org-clock-report)
          "C"      '("Capture" . org-capture)
          "d"      '(nil :which-key "Date")
          "d d"    '("Deadline" . org-deadline)
          "d s"    '("Schedule" . org-schedule)
          "d t"    '("Time Stamp" . org-time-stamp)
          "d T"    '("Inactive Time Stamp" . org-time-stamp-inactive)
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
          "t"      '(nil :which-key "Toggle")
          "t s"    '("Toggle Sub/Superscripts" . (lambda ()
                                                   (interactive)
                                                   (setq org-pretty-entities-include-sub-superscripts
                                                         (not org-pretty-entities-include-sub-superscripts)))))
         (evil-define-key 'normal calendar-mode-map
           (kbd "RET") #'org-calendar-select)
  :custom (org-fontify-quote-and-verse-blocks t)
  (org-src-fontify-natively nil)
  (org-pretty-entities t)
  (org-highlight-latex-and-related '(native latex))
  :diminish (org-cdlatex-mode . " ")
  :hook ((org-mode . turn-on-org-cdlatex)
         (org-mode . (lambda () (setq mode-name " Org")))
         (org-agenda-mode . (lambda () (setq mode-name "󰃮 Agenda"))))
  :config (set-face-foreground 'org-verbatim (catppuccin-get-color 'mauve))
          (set-face-attribute 'org-quote nil
                              :background (catppuccin-get-color 'mantle)
                              :extend t)
          (set-face-foreground 'org-table (catppuccin-get-color 'subtext1)))

(defun mjs/org-fix-newline-and-indent (&optional indent _arg _interactive)
  "Mimic `newline-and-indent' in src blocks w/ lang-appropriate indentation."
  (when (and org-src-tab-acts-natively (org-in-src-block-p t))
    (save-window-excursion
      (org-babel-do-in-edit-buffer
       (call-interactively #'indent-for-tab-command)))))

(advice-add #'org-return :after #'mjs/org-fix-newline-and-indent)

(defun mjs/org-return (&optional indent arg interactive)
  "Automatically indent when calling `org-return'."
  (interactive)
  (org-return electric-indent-mode))
(general-define-key :states 'insert :keymaps 'org-mode-map "RET" #'mjs/org-return)

(setq tab-always-indent nil)
;; (setq org-src-preserve-indentation t)

(defun mjs/org-dwim-at-point (&optional arg)
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- citation: follow it
- headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
  subtree; update statistics cookies/checkboxes and ToCs.
- clock: update its time.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- timestamp: open an agenda view for the time-stamp date/range at point.
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- src block: execute it
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive "P")
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        ((or `citation `citation-reference)
         (org-cite-follow context arg))

        (`headline
         (cond ((memq (bound-and-true-p org-goto-map)
                      (current-active-maps))
                (org-goto-ret))
               ((and (fboundp 'toc-org-insert-toc)
                     (member "TOC" (org-get-tags)))
                (toc-org-insert-toc)
                (message "Updating table of contents"))
               ((string= "ARCHIVE" (car-safe (org-get-tags)))
                (org-force-cycle-archived))
               ((or (org-element-property :todo-type context)
                    (org-element-property :scheduled context))
                (org-todo
                 (if (eq (org-element-property :todo-type context) 'done)
                     (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                         'todo)
                   'done))))
         ;; Update any metadata or inline previews in this subtree
         (org-update-checkbox-count)
         (org-update-parent-todo-statistics)
         (when (and (fboundp 'toc-org-insert-toc)
                    (member "TOC" (org-get-tags)))
           (toc-org-insert-toc)
           (message "Updating table of contents"))
         (let* ((beg (if (org-before-first-heading-p)
                         (line-beginning-position)
                       (save-excursion (org-back-to-heading) (point))))
                (end (if (org-before-first-heading-p)
                         (line-end-position)
                       (save-excursion (org-end-of-subtree) (point))))
                (overlays (ignore-errors (overlays-in beg end)))
                (latex-overlays
                 (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                             overlays))
                (image-overlays
                 (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                             overlays)))
           (+org--toggle-inline-images-in-subtree beg end)
           (if (or image-overlays latex-overlays)
               (org-clear-latex-preview beg end)
             (org--latex-preview-region beg end))))

        (`clock (org-clock-update-time-maybe))

        (`footnote-reference
         (org-footnote-goto-definition (org-element-property :label context)))

        (`footnote-definition
         (org-footnote-goto-previous-reference (org-element-property :label context)))

        ((or `planning `timestamp)
         (org-follow-timestamp-link))

        ((or `table `table-row)
         (if (org-at-TBLFM-p)
             (org-table-calc-current-TBLFM)
           (ignore-errors
             (save-excursion
               (goto-char (org-element-property :contents-begin context))
               (org-call-with-arg 'org-table-recalculate (or arg t))))))

        (`table-cell
         (org-table-blank-field)
         (org-table-recalculate arg)
         (when (and (string-empty-p (string-trim (org-table-get-field)))
                    (bound-and-true-p evil-local-mode))
           (evil-change-state 'insert)))

        (`babel-call
         (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies arg)))

        ((or `src-block `inline-src-block)
         (org-babel-execute-src-block arg))

        ((or `latex-fragment `latex-environment)
         (org-latex-preview arg))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (if (or (equal (org-element-property :type lineage) "img")
                   (and path (image-type-from-file-name path)))
               (+org--toggle-inline-images-in-subtree
                (org-element-property :begin lineage)
                (org-element-property :end lineage))
             (org-open-at-point arg))))

        (`paragraph
         (+org--toggle-inline-images-in-subtree))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
           (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           (+org--toggle-inline-images-in-subtree
            (org-element-property :begin context)
            (org-element-property :end context))))))))
(general-define-key :states 'normal :keymaps 'org-mode-map "RET"
                    #'mjs/org-dwim-at-point)

(defun mjs--org-insert-item (direction)
  (let ((context (org-element-lineage
                  (org-element-context)
                  '(table table-row headline inlinetask item plain-list)
                  t)))
    (pcase (org-element-type context)
      ;; Add a new list item (carrying over checkboxes if necessary)
      ((or `item `plain-list)
       (let ((orig-point (point)))
         ;; Position determines where org-insert-todo-heading and `org-insert-item'
         ;; insert the new list item.
         (if (eq direction 'above)
             (org-beginning-of-item)
           (end-of-line))
         (let* ((ctx-item? (eq 'item (org-element-type context)))
                (ctx-cb (org-element-property :contents-begin context))
                ;; Hack to handle edge case where the point is at the
                ;; beginning of the first item
                (beginning-of-list? (and (not ctx-item?)
                                         (= ctx-cb orig-point)))
                (item-context (if beginning-of-list?
                                  (org-element-context)
                                context))
                ;; Horrible hack to handle edge case where the
                ;; line of the bullet is empty
                (ictx-cb (org-element-property :contents-begin item-context))
                (empty? (and (eq direction 'below)
                             ;; in case contents-begin is nil, or contents-begin
                             ;; equals the position end of the line, the item is
                             ;; empty
                             (or (not ictx-cb)
                                 (= ictx-cb
                                    (1+ (point))))))
                (pre-insert-point (point)))
           ;; Insert dummy content, so that `org-insert-item'
           ;; inserts content below this item
           (when empty?
             (insert " "))
           (org-insert-item (org-element-property :checkbox context))
           ;; Remove dummy content
           (when empty?
             (delete-region pre-insert-point (1+ pre-insert-point))))))
      ;; Add a new table row
      ((or `table `table-row)
       (pcase direction
         ('below (save-excursion (org-table-insert-row t))
                 (org-table-next-row))
         ('above (save-excursion (org-shiftmetadown))
                 (+org/table-previous-row))))

      ;; Otherwise, add a new heading, carrying over any todo state, if
      ;; necessary.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; I intentionally avoid `org-insert-heading' and the like because they
         ;; impose unpredictable whitespace rules depending on the cursor
         ;; position. It's simpler to express this command's responsibility at a
         ;; lower level than work around all the quirks in org's API.
         (pcase direction
           (`below
            (let (org-insert-heading-respect-content)
              (goto-char (line-end-position))
              (org-end-of-subtree)
              (insert "\n" (make-string level ?*) " ")))
           (`above
            (org-back-to-heading)
            (insert (make-string level ?*) " ")
            (save-excursion (insert "\n"))))
         (run-hooks 'org-insert-heading-hook)
         (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                     (todo-type    (org-element-property :todo-type context)))
           (org-todo
            (cond ((eq todo-type 'done)
                   ;; Doesn't make sense to create more "DONE" headings
                   (car (+org-get-todo-keywords-for todo-keyword)))
                  (todo-keyword)
                  ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))
;; I use these instead of `org-insert-item' or `org-insert-heading' because they
;; impose bizarre whitespace rules depending on cursor location and many
;; settings. These commands have a much simpler responsibility.
(defun mjs/org-insert-item-below (count)
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  (dotimes (_ count) (mjs--org-insert-item 'below)))

(defun mjs/org-insert-item-above (count)
  "Inserts a new heading, table cell or item above the current one."
  (interactive "p")
  (dotimes (_ count) (mjs--org-insert-item 'above)))

(general-define-key :states '(normal insert)
                    :keymaps '(org-mode-map evil-org-mode-map)
                    "C-<return>"   #'mjs/org-insert-item-below
                    "C-S-<return>" #'mjs/org-insert-item-above)

(customize-set-variable 'org-directory "~/Documents/")

(add-hook 'org-mode-hook 'auto-fill-mode)
(diminish 'auto-fill-function)
(customize-set-variable 'org-startup-with-inline-images t)
(customize-set-variable 'org-image-actual-width '(600))
(customize-set-variable 'org-startup-align-all-tables t)
(customize-set-variable 'org-startup-folded 'showall)

(use-package ob-rust
  :after org
  :demand t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (emacs-lisp . t)
   (latex . t)
   (python . t)
   (R . t)
   (java . t)
   (rust . t)))

(require 'org-tempo)

;; There should be a better way to do this, but I haven't found it yet
(add-to-list 'org-structure-template-alist '("sC" . "src C"))
(add-to-list 'org-structure-template-alist '("cp" . "src cpp"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("st" . "src latex"))
(add-to-list 'org-structure-template-alist '("p" . "src python"))
(add-to-list 'org-structure-template-alist '("r" . "src rust"))
(add-to-list 'org-structure-template-alist '("R" . "src R"))
(add-to-list 'org-structure-template-alist '("j" . "src java"))
(add-to-list 'org-structure-template-alist '("t" . "LaTeX latex"))

(use-package eros
  :after org
  :hook (org-mode . eros-mode))

(setq org-tag-persistent-alist
      '((:startgroup)
        ("knowledge_base")
        ("great_basin")
        ("etera")
        ("obscured_realms")
        (:endgroup)
        ("needs_merge")
        (:startgrouptag)
        ("knowledge_base")
        (:grouptags)
        ("computer_operation")
        ("mathematics")
        ("processes")
        ("programming")
        (:endgrouptag)
        (:startgrouptag)
        ("mathematics")
        (:grouptags)
        ("combinatorics")
        ("linear_algebra")
        ("modeling")
        ("statistics")
        ("optimization")
        (:endgrouptag)
        (:startgrouptag)
        ("programming")
        (:grouptags)
        ("C")
        ("lisp")
        (:endgrouptag)
        (:startgrouptag)
        ("computer_operation")
        (:grouptags)
        ("network")
        (:endgrouptag)
        (:startgrouptag)
        ("statistics")
        (:grouptags)
        ("confidence_intervals")
        ("hypothesis_tests")
        ("probability")
        ("regression")
        (:endgrouptag)
        (:startgrouptag)
        ("optimization")
        (:grouptags)
        ("linear_programming")
        (:endgrouptag)
        (:startgrouptag)
        ("modeling")
        (:grouptags)
        ("association_analysis")
        ("classification")
        ("clustering")
        ("decision_tree")
        ("information_retrieval")
        ("recommender")
        ("text_mining")
        ("regression")
        (:endgrouptag)
        (:startgrouptag)
        ("great_basin")
        (:grouptags)
        ("character")
        ("event")
        ("faction")
        ("location")
        ("object")
        ("session")
        ("stat")
        ;; Start of individual factions
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

(setq org-agenda-start-with-log-mode t)
;; Log the time a task is completed in a property drawer.
(setq org-log-done 'time)
(setq org-log-into-drawer t)
;; hide tags from agenda view, I'll probably be using the `#+CATEGORY'
;; more often anyways
(setq org-agenda-hide-tags-regexp ".")

(customize-set-variable 'org-agenda-files
                        (list (concat org-directory "agenda/")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "BLOCKED(b)" "|" "DONE(d)" "KILLED(k)")))
(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

(defun mjs/agenda-time-format (type)
  (let* ((current-date (format-time-string "%Y-%m-%d" (current-time)))
         (timestamp (format-time-string "%Y-%m-%d" (if (equal type 'deadline)
                                                      (org-get-deadline-time (point))
                                                    (org-get-scheduled-time (point)))))
         (difference (days-between timestamp current-date))
         (time-remaining (cond ((eq difference 0) "today")
                               ((<= (abs difference) 1)
                                (if (< difference 0)
                                    (format "%i day ago" (abs difference))
                                  (format "%i day" difference)))
                               ((> difference 1) (format "%i days" difference))
                               ((< difference -1) (format "%i days ago" (abs difference))))))
    (format "%s : %s" timestamp time-remaining)))

(setq org-agenda-custom-commands
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
          (tags "SCHEDULED<=\"<today>\"-TODO=\"DONE\"-TODO=\"KILLED\""
                     ((org-agenda-overriding-header "\nScheduled\n")
                      (org-agenda-prefix-format " %i %-12:c [%(mjs/agenda-time-format 'scheduled)] ")))
          (todo "NEXT"
                ((org-agenda-prefix-format " %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks\n")))
          (tags-todo "inbox"
                     ((org-agenda-prefix-format " %?-12t% s")
                      (org-agenda-overriding-header "\nInbox\n")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n")))))))

(use-package org-contacts
  :after org
  :custom (org-contacts-files
           (list (concat org-directory "contacts.org"))))

(mjs-leader-def :keymaps 'org-capture-mode-map
  "C"   '(nil :which-key "Capture")
  "C f" '("Finish Capture" . org-capture-finalize)
  "C k" '("Abort Capture" . org-capture-kill)
  "C r" '("Refile Capture" . org-capture-refile))
(mjs-leader-def :predicate '(not (bound-and-true-p org-capture-mode))
  "C" '("Capture" . org-capture))

(defun mjs/org-capture-update-header ()
  (setq header-line-format
        (format "%s%s%s"
                (propertize (abbreviate-file-name
                             (buffer-file-name (buffer-base-buffer)))
                            'face 'font-lock-string-face)
                " ⟶ "
                (concat
                 "Capture Buffer. Finish "
                 (propertize "SPC C f" 'face 'help-key-binding)
                 ", refile "
                 (propertize "SPC C r" 'face 'help-key-binding)
                 ", abort "
                 (propertize "SPC C k" 'face 'help-key-binding)
                 " in normal mode."
                 ))))

(add-hook 'org-capture-mode-hook #'mjs/org-capture-update-header)

(defun mjs/named-capture (prompt base-dir)
  (let* ((character-name (read-string prompt))
         (file-name (expand-file-name
                     (concat base-dir
                             (downcase
                              (string-replace " " "-"
                                              (replace-regexp-in-string
                                               "[^[:alnum:] ]" ""
                                               character-name)))
                             ".org")
                     org-directory)))
    (setq mjs--capture-title character-name)
    (set-buffer (org-capture-target-buffer file-name))
    (goto-char (point-min))))

(defun mjs/class-capture ()
  (let* ((class (completing-read "Class: "
                                 '("cs400" "cs525" "cs880" "cs900")
                                 nil t))
         (file-name (expand-file-name
                     (concat "classes/" class "/"
                             (format-time-string "%Y-%m-%d" (current-time))
                             "-" class ".org")
                     org-directory)))
    (setq mjs--capture-title class)
    (set-buffer (org-capture-target-buffer file-name))
    (goto-char (point-min))))

(defun mjs/capture-insert-id ()
  (save-excursion
    (goto-char (point-min))
    (org-id-get-create)))

(add-hook 'org-capture-mode-hook (lambda () (flycheck-mode -1)))
(add-hook 'org-capture-mode-hook (lambda () (require 'diminish)
                                   (diminish 'org-capture-mode " 󰄀")
                                   (diminish 'narrow " 󰝔")))

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
        ("g" "Great Basin")
        ("gc" "Great Basin Character" plain
         (function (lambda ()
                     (mjs/named-capture
                      "Character Name: "
                      "ttrpg/great-basin/characters/")))
         (file "ttrpg/great-basin/characters/template.org")
         ;; The docs say this has to be a 'nullary function' and
         ;; even thought it /is/ a nullary function if it's not
         ;; wrapped in the lmabda I get an error.
         :hook (lambda () (mjs/capture-insert-id)))
        ("ge" "Great Basin Event" plain
         (function (lambda ()
                     (mjs/named-capture
                      "Event Name: "
                      "ttrpg/great-basin/events/")))
         (file "ttrpg/great-basin/events/template.org")
         :hook (lambda () (mjs/capture-insert-id)))
        ("gl" "Great Basin Location" plain
         (function (lambda ()
                     (mjs/named-capture
                      "Location Name: "
                      "ttrpg/great-basin/locations/")))
         (file "ttrpg/great-basin/locations/template.org")
         :hook (lambda () (mjs/capture-insert-id)))
        ("go" "Great Basin Object" plain
         (function (lambda ()
                     (mjs/named-capture
                      "Object Name: "
                      "ttrpg/great-basin/objects/")))
         (file "ttrpg/great-basin/objects/template.org")
         :hook (lambda () (mjs/capture-insert-id)))
        ;; :TODO: Replace this with something not dependent on Eamcs restarts
        ("gr" "Great Basin Session Record" plain
         (file ,(format "ttrpg/great-basin/sessions/great-basin-%s.org"
                        (org-read-date nil nil "Wed")))
         (file "ttrpg/great-basin/sessions/template.org")
         :jump-to-captured t
         :immediate-finish t)
        ("gR" "Great Basin Public Session Record" plain
         (file ,(format "ttrpg/great-basin/public/session-recaps/great-basin-%s.org"
                        (org-read-date nil nil "-Wed")))
         (file "ttrpg/great-basin/public/session-recaps/template.org")
         :jump-to-captured t)
        ("gs" "Great Basin Stat Block" plain
         (function (lambda ()
                     (mjs/named-capture
                      "Stat Block Name: "
                      "ttrpg/great-basin/stat-blocks/")))
         (file "ttrpg/great-basin/stat-blocks/template.org")
         :hook (lambda () (mjs/capture-insert-id)))
        ("i" "Inbox" entry
         (file "agenda/inbox.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U")
         :empty-lines 1
         :prepend t)
        ("k" "Knowledge Base" plain
         (function (lambda ()
                     (mjs/named-capture
                      "Node Name: "
                      "knowledge-base/")))
         ,(concat "#+filetags: :knowledge_base:\n"
                  "#+author: %(user-full-name)\n"
                  "#+title: %(format mjs--capture-title)\n\n%?")
         :hook (lambda () (mjs/capture-insert-id)))
        ("m" "Meeting" entry
         (file+headline "agenda/agenda.org" "Future")
         ,(concat "* %? :meeting:\n"
                  "SCHEDULED: %^{Meeting Time}T"))
        ("n" "Meeting Notes" entry
         (file "agenda/notes.org")
         ,(concat "* Notes (%a)\n"
                  "/Entered on/ %U\n\n%?"))))

;; Save the buffers after refile
(advice-add #'org-refile :after #'org-save-all-org-buffers)

(setq org-refile-targets
      '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(customize-set-variable 'org-startup-with-latex-preview t)
(customize-set-variable 'org-preview-latex-default-process 'dvisvgm)
;; Automatically set the scale depending on the scale of the monitor. Is is so that on `luna'
;; the fragments are rendered at the correct size and not huge.
(add-hook 'org-mode-hook (lambda ()
                           (plist-put org-format-latex-options
                                      :scale (if-let ((scale (alist-get 'scale-factor
                                                                        (car (display-monitor-attributes-list)))))
                                                 (/ 1 scale)
                                               1.0))))
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 1.0))
(add-to-list 'org-latex-packages-alist '("" "sfmath" t))
(add-to-list 'org-latex-packages-alist '("margin=1in" "geometry" t))
(add-to-list 'org-latex-packages-alist '("" "parskip" t))
(add-to-list 'org-latex-packages-alist '("" "nicematrix" t))
(add-to-list 'org-latex-packages-alist '("" "amsthm" t))
(add-to-list 'org-latex-packages-alist '("" "cancel" t))
(setq org-entities-user
      '(("mathbbR" "\\mathbb{R}" nil "&x211D" "R" "R" "ℝ")
        ("mathbbE" "\\mathbb{E}" nil "&x1D53C" "E" "E" "𝔼")
        ("lightning" "\\lightning" nil "&x21AF" "</" "</" "↯")
        ("qed" "\\qedsymbol" nil "&x25A1" "[]" "[]" "☐")))

;; (use-package org-auctex
;;   :hook (org-mode . org-auctex-mode))

(defun mjs/resize-org-latex-overlays ()
  (interactive)
  (cl-loop for o in (car (overlay-lists))
           if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
           do (plist-put (cdr (overlay-get o 'display))
                         :scale (expt text-scale-mode-step
                                      text-scale-mode-amount))))
(add-hook 'text-scale-mode-hook (lambda ()
                                  (if (and text-scale-mode
                                           (eq major-mode 'org-mode))
                                      (mjs/resize-org-latex-overlays)
                                    (if (eq major-mode 'org-mode)
                                        (mjs/resize-org-latex-overlays)))))

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

;; While it might seem weird to put the LaTeX configuration here,
;; I primarily expect to be using it in `org-mode', so I'd like it all in
;; one place

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
           (TeX-view-program-list '(("Zathura" "zathura --synctex-forward :: %o")))
           (TeX-view-program-selection '((output-pdf "Zathura")))
           (TeX-save-query nil))
  :init 
  (defun mjs/preview-scale-adjustment ()
    (setq preview-scale-function
          (lambda ()
            (* 0.8 (funcall (preview-scale-from-face))))))
  (defun mjs/latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point"
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-langauge latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))))
  :config (general-define-key :states '(insert normal) :map 'LaTeX-mode-map
                              "C-S-e" #'mjs/latex-math-from-calc)
  (mjs-local-leader-def :keymaps 'LaTeX-mode-map
    "c" '("Compile Document" . TeX-command-run-all)
    "e" '("Insert Environment" . LaTeX-environment)
    "i" '("Indent Line" . LaTeX-indent-line)
    "m" '("Insert Macro" . TeX-insert-marco)
    "s" '("Insert Section" . LaTeX-section)
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
  :init (defun mjs/yas-try-expanding-auto-snippets ()
          (when (bound-and-true-p yas-minor-mode)
            (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
              (yas-expand))))
  (defun mjs/cdlatex-in-yas-field ()
    ;; Check if we're at the end of the Yas field
    (when-let* ((_ (overlayp yas--active-field-overlay))
                (end (overlay-end yas--active-field-overlay)))
      (if (>= (point) end)
          ;; Call yas-next-field if cdlatex can't expand here
          (let ((s (thing-at-point 'sexp)))
            (unless (and s (assoc (substring-no-properties s)
                                  cdlatex-command-list-comb))
              (yas-next-field-or-maybe-expand)
              t))
        ;; Otherwise expand and jump to the correct location
        (let (cdlatex-tab-hook minp)
          (setq minp
                (min (save-excursion (cdlatex-tab)
                                     (point))
                     (overlay-end yas--active-field-overlay)))
          (goto-char minp) t))))
  (defun mjs/yas-next-field-or-cdlatex nil
    "Jump to the next Yas field correctly with cdlatex active"
    (interactive)
    (if (or (bound-and-true-p cdlatex-mode)
            (bound-and-true-p org-cdlatex-mode))
        (cdlatex-tab)
      (yas-next-field-or-maybe-expand)))
  :config (use-package warnings
            :ensure nil
            :config
            (cl-pushnew '(yasnippet backquote-change)
                        warning-suppress-types
                        :test 'equal))
  (general-define-key :keymaps 'yas-keymap :states 'insert
                      "<tab>" #'mjs/yas-next-field-or-cdlatex
                      "TAB" #'mjs/yas-next-field-or-cdlatex))

;; (use-package yasnippet-snippets
;;   :after yasnippet)

(use-package cdlatex
  :diminish " "
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (cdlatex-tab . yas-expand)
         (cdlatex-tab . mjs/cdlatex-in-yas-field)
         (cdlatex-tab . LaTeX-indent-line))
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
  :init (add-hook 'cdlatex-tab-hook #'lazytab-cdlatex-or-orgtbl-next-field 90)
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
                                        nil t nil))
  (defun lazytab-position-cursor-and-edit ()
    (interactive)
    (cdlatex-position-cursor)
    (lazytab-orgtbl-edit))
  (defun lazytab-orgtbl-edit ()
    (if org-cdlatex-mode
        (advice-add 'org-ctrl-c-ctrl-c :after #'lazytab-orgtbl-replace)
      (advice-add 'orgtbl-ctrl-c-ctrl-c :after #'lazytab-orgtbl-replace)
      (orgtbl-mode 1))
    (open-line 1)
    (insert "\n|"))
  (defun lazytab-orgtbl-replace (_)
    (interactive "P")
    (unless (org-at-table-p) (user-error "Not at a table"))
    (let* ((table (org-table-to-lisp))
           params
           (replacement-table (if (or t (texmathp))
                                  (lazytab-orgtbl-to-amsmath table params)
                                (orgtbl-to-latex table params))))
      (kill-region (org-table-begin) (org-table-end))
      (open-line 1)
      (push-mark)
      (insert replacement-table)
      (align-regexp (region-beginning) (region-end) "\\([:space:]*\\)& ")
      (if org-cdlatex-mode
          (advice-remove 'org-ctrl-c-ctrl-c #'lazytab-orgtbl-replace)
        (advice-remove 'orgtbl-ctrl-c-ctrl-c #'lazytab-orgtbl-replace)
        (orgtbl-mode -1))))
  (defun lazytab-orgtbl-to-amsmath (table params)
    (orgtbl-to-generic
     table
     (org-combine-plists
      '(:splice t
                :lstart ""
                :lend " \\\\"
                :sep " & "
                :hline nil
                :llend "")
      params)))
  (defun lazytab-cdlatex-or-orgtbl-next-field ()
    (when (and (bound-and-true-p orgtbl-mode)
               (org-table-p)
               (looking-at "[[:space:]]*\\(?:|\\|$\\)")
               (let ((s (thing-at-point 'sexp)))
                 (not (and s (assoc s cdlatex-command-alist-comb)))))
      (call-interactively #'org-table-next-field)
      t))
  (defun lazytab-org-table-next-field-maybe ()
    (interactive)
    (if (bound-and-true-p cdlatex-mode)
        (cdlatex-tab)
      (org-table-next-field)))
  :config (general-define-key :keymaps 'orgtbl-mode-map :states 'insert
                              "<tab>" #'lazytab-org-table-next-field-maybe
                              "TAB" #'lazytab-org-table-next-field-maybe))

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
          "t o" '("Toggle Olivetti" . olivetti-mode)))

(use-package flycheck
  :diminish " 󰨮"
  :custom (flycheck-global-modes '(not org-capture-mode)))
;; :init (global-flycheck-mode))

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

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-indent-mode-hook (lambda () (diminish 'org-indent-mode)))

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
                             "o" '("Outline" . pdf-outline)
                             "p" '("Goto Page" . pdf-view-goto-page))
                           (mjs-local-leader-def :keymaps 'pdf-annot-edit-contents-minor-mode-map
                             "P"   '(nil :which-key "PDF Annotations")
                             "P f" '("Finalize Annotation" . (lambda () (interactive) (pdf-annot-edit-contents-finalize t t)))
                             "P k" '("Kill Annotation" . pdf-annot-edit-contents-abort)
                             "P s" '("Save Annotation" . pdf-annot-edit-contents-commit))))
         (pdf-view-mode . (lambda ()
                            (set (make-local-variable 'evil-default-cursor) (list nil)))))
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
  (add-hook 'pdf-annot-edit-contents-minor-mode-hook #'mjs/pdf-annot-update-header)
  (pdf-tools-install-noverify))

(use-package saveplace-pdf-view
  :after pdf-view)

(use-package org-noter)
  ;; :config
  ;; Your org-noter config ........
  ;; (require 'org-noter-pdf "~/.config/emacs-configs/mjs/straight/repos/org-pdftools/org-noter-pdftools.el"))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
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

(defun mjs/strip-org-roam-links ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[id:[^]]*\\]\\[\\([^]]*\\)\\]\\]" nil t)
      (replace-match "\\1" nil nil))))

(defun mjs/org-refile-dwim (target)
  (interactive "FDestination File: \n")
  (unless (org-at-heading-p)
    (error "Point not at org heading! Aborting"))
  (if (file-exists-p target)
      ;; File does exist, use regular `org-refile'
      (org-refile nil nil target nil)
    (let ((org-tags (map 'list #'substring-no-properties (org-get-tags)))
          (org-heading (substring-no-properties (org-get-heading)))
          (new-node-id nil)
          (old-buf (current-buffer))
          (new-buf (get-buffer-create (file-name-nondirectory target))))
      (org-copy-subtree nil 'cut)
      (switch-to-buffer new-buf)
      (when org-tags ; Insert tags when then exist
        (insert "#+filetags: ")
        (dolist (tag org-tags)
          (insert (concat ":" tag)))
        (insert ":\n"))
      (insert (concat "#+title: " org-heading "\n\n"))
      (write-file target) ; org-roam can only create ids for buffers visiting a file
      (org-id-get-create)
      (setq new-node-id (org-roam-id-at-point))
      (org-paste-subtree)
      (kill-whole-line 2) ; Assumed format is heading followed by blank line
      (org-next-visible-heading 1)
      (when (org-at-heading-p) ; If there is another heading, promote it to first level
        (while (not (eql 1 (nth 0 (org-heading-components))))
          (org-promote-subtree)))
      (goto-char (point-min))
      (write-file target)
      (with-current-buffer old-buf
        (insert (concat "[[id:" new-node-id "][" org-heading "]]\n\n"))))))

(use-package org-roam
  :custom (org-roam-directory (file-truename org-directory))
          (org-roam-completion-everywhere t)
          (org-roam-node-display-template
           (concat "${title:*} "
                   (propertize "${tags:30}" 'face 'org-tag)))
  :init (mjs-leader-def :keymaps 'override
             "n r"   '(nil :which-key "Roam")
             "n r a" '("Add Alias" . org-roam-alias-add)
             "n r A" '("Remove Alias" . org-roam-alias-remove)
             "n r b" '("Toggle Roam Buffer" . org-roam-buffer-toggle)
             "n r f" '("Find Node" . (lambda () (interactive) (org-roam-node-find nil "^")))
             "n r F" '("Find Ref" . org-roam-ref-find)
             "n r g" '("Graph" . org-roam-graph)
             "n r i" '("Insert Link" . org-roam-node-insert)
             "n r c" '("Roam Capture" . org-roam-capture)
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
             "m f" '("Find Node" . (lambda () (interactive) (org-roam-node-find nil "^")))
             "m F" '("Find Ref" . org-roam-ref-find)
             "m g" '("Graph" . org-roam-graph)
             "m i" '("Insert Link" . org-roam-node-insert)
             "m c" '("Roam Capture" . org-roam-capture)
             "m s" '("Roam Sync" . org-roam-db-sync)
             "m S" '("Strip Roam Links" . mjs/strip-org-roam-links)
             "m d" '("Daily" . org-roam-dailies-capture-today)
             "m r" '("Random Node" . org-roam-node-random)
             "m t" '("Add Tags" . org-roam-tag-add)
             "m T" '("Remove Tags" . org-roam-tag-remove))
  :general (:states 'insert :keymaps 'org-mode-map
                    "C-f" #'org-roam-node-insert
                    "C-S-f" #'org-insert-link)
  :hook (org-mode . org-roam-db-autosync-mode)
  ;; Taken from https://github.com/org-roam/org-roam/pull/2219/files
  :config (defconst mjs/org-roam-bracket-completion-re
            "\\[\\(?:\\[id:\\([^z-a]*?\\)]\\)?\\[\\([^z-a]+?\\)]]"
            "Regex for completion within link brackets.
Matches both empty links (i.e. \"[[|]]\") and existing \"id:\"
links (e.x. \"[[id:01234][|]]\").")
  (defun mjs/org-roam-complete-link-at-point()
    "Complete \"id:\" link at point to an existing Org-roam node.
Places the completed string into the link description. If there
was already an \"id:\" link at point, overwrites it to fit the
new description."
    (let (roam-p start end)
      (when (org-in-regexp mjs/org-roam-bracket-completion-re 1)
        (setq old-id (match-string 1)
              start (match-beginning 2)
              end (match-end 2))
        (list start end
            (org-roam--get-titles)
            :exit-function
            (lambda (str &rest _)
              (let ((node-id (org-roam-node-id
                              (org-roam-node-from-title-or-alias
                               (substring-no-properties str)))))
                (cond (old-id (delete-char
                               (- 0 (length str) (length old-id) 2))
                              (insert node-id "][" str))
                      (t (delete-char (- 0 (length str)))
                         (insert "id:" node-id "][" str)))
                (forward-char 2)))))))
  (defun mjs/org-roam-complete-everywhere ()
    "Complete symbol at point as a link completion to an Org-roam node.
This is a `completion-at-point' function, and is active when
`org-roam-completion-everywhere' is non-nil.
Unlike `org-roam-complete-link-at-point' this will complete even
outside of the bracket syntax for links (i.e. \"[[id:|]]\"),
hence \"everywhere\"."
    (when (and org-roam-completion-everywhere
               (thing-at-point 'word)
               (not (save-match-data (org-in-regexp org-link-any-re))))
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (list (car bounds) (cdr bounds)
              (org-roam--get-titles)
              :exit-function
              (lambda (str _status)
                (delete-char (- (length str)))
                  (insert "[[id:"
                      (org-roam-node-id (org-roam-node-from-title-or-alias
                                         (substring-no-properties str)))
                      "][" str "]]"))
              ;; Proceed with the next completion function if the returned titles
              ;; do not match. This allows the default Org capfs or custom capfs
              ;; of lower priority to run.
              :exclusive 'no))))
  (advice-add #'org-roam-complete-link-at-point :override #'mjs/org-roam-complete-link-at-point)
  (advice-add #'org-roam-complete-everywhere :override #'mjs/org-roam-complete-everywhere))
      

(use-package vulpea
  :hook (org-roam-db-autosync-mode . vulpea-db-autosync-enable))

(defun mjs/search-org-files (query)
  "Return a list of files in the org directory containing QUERY"
  (split-string
   (shell-command-to-string (format "rg \"%s\" %s" query org-directory))
   ":.*\n" t))

(defun mjs/move-and-update-file-links (source-file dest-dir &optional search-dir)
  "Move SOURCE-FILE to DEST-DIR, updating all org file links in SEARCH-DIR"
    (interactive "fSource File: \nDDestination Directory: \n")
    (let* ((search-dir (if (stringp search-dir) search-dir org-directory))
           (source-file-name (file-name-nondirectory source-file))
           (dest-file (file-name-concat dest-dir source-file-name))
           (file-regexp
            (format
             "\\[\\[\\(file\\|pdf\\):\\([^]:]*%s\\)\\(::[0-9]+\\)?\\]\\[\\([^]]*\\)\\]\\]"
                             source-file-name)))

      (rename-file source-file dest-file t)
      ; Iterate over all files in the search directory referencing the source file
      (dolist (file (mjs/search-org-files source-file-name))
        (with-current-buffer (find-file-noselect file) ; Open that file as a buffer
        ;; Here is where the find and replace can happen
        (let ((relative-dest-file (file-relative-name dest-file
                                                      (file-name-directory file))))
          (message (concat "Searching with: " file-regexp))
          (goto-char (point-min))
          (while (re-search-forward file-regexp nil t)
            (replace-match relative-dest-file nil nil nil 2)
            )
        (save-buffer)
        ))
      )))

(defun mjs/move-dir-update-file-links (source-dir dest-dir &optional search-dir)
  "Move SOURCE-DIR into DEST-DIR, updating all file links in SEARCH-DIR referencing files in SOURCE-DIR"
  (interactive "DSource Directory: \nDDestination Directory: \n")
  (let ((search-dir (if (stringp search-dir) search-dir org-directory))
        (dest-file-base (file-name-concat dest-dir
                                          (file-name-nondirectory
                                           (directory-file-name source-dir)))))
    (dolist (file (directory-files-recursively source-dir ".*"))
      (let ((dest-file-dir (expand-file-name
                            (file-relative-name (file-name-directory file) source-dir)
                            dest-file-base)))
        (unless (file-directory-p dest-file-dir)
          (make-directory dest-file-dir))
        (mjs/move-and-update-file-links file dest-file-dir search-dir)
      ))
    (delete-directory source-dir)
  ))

(defun mjs/regenerate-file-links (src &optional search-dir kill)
  "Regenerate file links in SRC org file by searching SEARCH-DIR and updating file paths"
  (interactive (list (current-buffer)))
  (let* ((search-dir (if (stringp search-dir) search-dir org-directory))
         (src-buf (cond
               ((bufferp src) src)
               ((stringp src) (find-file-noselect src)) ; Assume this is a filename
               (t (current-buffer))))
         (src-file (buffer-file-name src-buf))
         (file-link-regexp
          "\\[\\[\\(file\\|pdf\\):\\([^]:]*\\)\\(::[0-9]+\\)?\\]\\(\\[[^]]*\\]\\)\\]"))
    (with-current-buffer src-buf
      (save-excursion (goto-char (point-min)) ; Move the point to start of buffer
                      (while (re-search-forward file-link-regexp nil t)
                        (let ((rel-dest-file (string-trim (file-relative-name
                                                           (shell-command-to-string
                                                            (format "find %s -name \"%s\""
                                                                    search-dir
                                                                    (file-name-nondirectory
                                                                     (match-string 2))))
                                                           (file-name-directory src-file)))))
                          (replace-match rel-dest-file nil nil nil 2) ; Replace only the file path
                          ))
                      (save-buffer)
                      ))
    (when kill (kill-buffer src-buf))
    ))

(defun mjs/regenerate-file-links-globally (&optional dir)
  (interactive "DDirectory: \n")
  (let ((dir (if (stringp dir) dir org-directory)))
    (dolist (file (directory-files-recursively dir ".*\\.org"))
      (mjs/regenerate-file-links file nil 'kill)
    )
))

(mjs-leader-def :keymaps 'org-mode-map
  "f M" '("Move File & Update Links" . mjs/move-and-update-file-links))

(use-package org-cliplink
  :init (mjs-local-leader-def :keymaps 'org-mode-map
             "l c" '("Paste URL" . mjs/clean-org-cliplink)
             "l C" '("Paste Raw URL" . org-cliplink))
        (defun mjs/clean-org-cliplink ()
          (interactive)
          (org-cliplink-insert-transformed-title
          (org-cliplink-clipboard-content)     ;take the URL from the CLIPBOARD
          (lambda (url title)
              (let* ((parsed-url (url-generic-parse-url url)) ;parse the url
                (clean-title
                  (cond
                  ;; if the host is one of these, cleanup the title
                  ((string= (url-host parsed-url) "aonprd.com")
                    (replace-regexp-in-string " - .*" "" title))
                  ((string= (url-host parsed-url) "en.wikipedia.org")
                   (replace-regexp-in-string " - .*" "" title))
                  ((string= (url-host parsed-url) "github.com")
                   (replace-regexp-in-string "^Github - " "" title))
                  ;; otherwise keep the original title
                  (t title))))
          ;; forward the title to the default org-cliplink transformer
          (org-cliplink-org-mode-link-transformer url clean-title))))))

(use-package org-chef
  :config (add-to-list 'org-capture-templates
                       `("r" "Recipe" entry
                         (file ,(concat org-directory "recipes.org"))
                         "%(org-chef-get-recipe-from-url)"
                         :empty-lines 1) t)
          (add-to-list 'org-capture-templates
                       `("R" "Manual Recipe" entry
                         (file ,(concat org-directory "recipes.org"))
                         ,(concat "* %^{Recipe title: }\n"
                                  "  :PROPERTIES:\n"
                                  "  :source-url:\n"
                                  "  :servings:\n"
                                  "  :prep-time:\n"
                                  "  :cook-time:\n"
                                  "  :ready-in:\n"
                                  "  :END:\n\n"
                                  "** Ingredients\n\n"
                                  "   %?\n\n"
                                  "** Directions\n\n")) t))

(use-package ox-pandoc
  :init (mjs-local-leader-def :keymap 'org-mode-map
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
  :init (mjs-local-leader-def :keymaps 'org-mode-map
             "e h" '(nil :which-key "Hugo")
             "e h d" '("DWIM" . org-hugo-export-wim-to-md)
             "e h a" '("All Subtrees" . (lambda () (org-hugo-wim-to-md t)))
             "e h o" '("File" . org-hugo-export-to-md))
        (defun mjs/hugo-blowfish-thumbnail (&rest args)
          (interactive)
          (if-let ((hugo-info (org-collect-keywords '("hugo_base_dir" "hugo_section")))
                   (base-dir (nth 1 (nth 0 hugo-info))) 
                   (base-section (nth 1 (nth 1 hugo-info)))
                   (thumbnail (or (org-entry-get (point) "blowfish_thumbnail")
                                  (nth 1 (car (org-collect-keywords '("blowfish_thumbnail"))))))
                   (thumbnail-path (file-name-concat base-dir "assets"
                                                     thumbnail))
                   (bundle-name (or (org-entry-get (point) "export_hugo_bundle")
                                    (nth 1 (car (org-collect-keywords '("hugo_bundle"))))))
                   (dest-path (concat
                                (file-name-concat base-dir
                                                  "content"
                                                  base-section
                                                  bundle-name
                                                  "featured")
                                (file-name-extension thumbnail t))))
            (copy-file thumbnail-path dest-path t)
          (user-error "Could not move blowfish thumbnail image.")))
  :config 
        (advice-add #'org-hugo--export-subtree-to-md
                    :after #'mjs/hugo-blowfish-thumbnail)
        (advice-add #'org-hugo--export-file-to-md
                      :after #'mjs/hugo-blowfish-thumbnail))

(use-package org-tree-slide
  :commands org-tree-slide-mode
  :diminish " 󰐨"
  :init (mjs-local-leader-def :keymaps 'org-mode-map
          "p" '("Present" . org-tree-slide-mode))
  :general (:states '(normal insert motion) :keymaps 'org-tree-slide-mode-map
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

(require 'org-timeblock)
(mjs-leader-def :keymaps 'override
  "B" '("Time Blocks" . org-timeblock))

(use-package ledger-mode
  :custom (ledger-clear-whole-transactions 1)
  (ledger-mode-should-check-version nil))

(use-package evil-ledger
  :hook (ledger-mode . evil-ledger-mode))
