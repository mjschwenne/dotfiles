;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Matt Schwennesen"
      user-mail-address "mjschwenne@gmail.com")

;;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15 :weight 'light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
;; (load-theme 'doom-tokyo-night t)

(use-package! autothemer
  :ensure t)

(load-theme 'catppuccin-mocha t)

(set-face-underline 'link t)
(custom-theme-set-faces 'catppuccin-mocha
                       '(org-block ((t (:background "#181825"))))
                       '(org-block-begin-line ((t (:background "#181825" :foreground "#7f849c"))))
                       '(org-block-end-line ((t (:background "#181825" :foreground "#7f849c"))))
                       '(evil-goggles-default-face ((t (:background "#313244" :extend t)))))

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(map! :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right)

; Make evil-snipe search the whole buffer by default
(customize-set-variable 'evil-snipe-scope 'buffer)

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

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(add-hook 'org-mode-hook 'auto-fill-mode)
;; Image previewing in org mode
(customize-set-variable 'org-startup-with-inline-images t)
;; Normally, this would be set with `customize-set-variable`, but DOOM Emacs has a function
;; which is loaded as part of their org initialization which overrides this variable.
;; While I could change that, I want to be able to pull the DOOM repo without issue so
;; I'm using advice to update that value after anytime the function in question is called.
(advice-add '+org-init-appearance-h :after (lambda () (customize-set-variable 'org-image-actual-width '(600))))

(setq org-agenda-start-with-log-mode t)
;; Log the time a task is completed in a property drawer.
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(advice-add 'org-refile :after 'org-save-all-org-buffers) ;; Save the buffers after refile

(customize-set-variable 'org-agenda-files '("~/org/tasks/"))

(customize-set-variable 'org-agenda-custom-commands
                        '(("d" "Daily Schedule"
                          ((agenda ""
                                   ((org-agenda-span 'day)
                                    (org-agenda-use-time-grid nil)
                                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (cpp . t)
   (emacs-lisp . t)
   (latex . t)
   (python . t)
   (R . t)))

(require 'org-tempo)
;; There should be a better way to do this, but I haven't found it yet
(add-to-list 'org-structure-template-alist '("sC" . "src C"))
(add-to-list 'org-structure-template-alist '("scpp" . "src cpp"))
(add-to-list 'org-structure-template-alist '("sel" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sl" . "src latex"))
(add-to-list 'org-structure-template-alist '("spy" . "src python"))
(add-to-list 'org-structure-template-alist '("sR" . "src R"))

(map! :map org-capture-map
      :leader
      :prefix ("C" . "Capture")
      :desc "Finsh Capture" :n "f" #'org-capture-finalize
      :desc "Abort Capture" :n "k" #'org-capture-kill
      :desc "Refile Capture" :n "r" #'org-capture-refile)

;; DOOM already modifies the header line to add the target file to the header line.
;; This function is ripped from that with the addition of the `concat' portion which I
;; wrote.
(defun mjs/org-capture-update-header ()
  (setq header-line-format
        (format "%s%s%s"
                (propertize (abbreviate-file-name (buffer-file-name (buffer-base-buffer)))
                                'face 'font-lock-string-face)
                org-eldoc-breadcrumb-separator
                (concat
                 "Capture Buffer. Finish "
                 (propertize "SPC C f" 'face 'help-key-binding)
                 ", refile "
                 (propertize "SPC C r" 'face 'help-key-binding)
                 ", abort "
                 (propertize "SPC C k" 'face 'help-key-binding)
                 " in normal mode."
                 ))))

(advice-add '+org-show-target-in-capture-header-h :override 'mjs/org-capture-update-header)

(use-package! org-appear
  :config (custom-set-variables '(org-hide-emphasis-markers t)
                                '(org-appear-autolinks t)
                                '(org-appear-trigger 'manual))
  :hook (org-mode . (lambda ()
                       (org-appear-mode t)
                       (add-hook 'evil-insert-state-entry-hook
                                 #'org-appear-manual-start nil t)
                       (add-hook 'evil-insert-state-exit-hook
                                 #'org-appear-manual-stop nil t)))
  )

(setq langtool-language-tool-jar "/home/mjs/.local/share/nvim/site/pack/packer/start/vim-grammarous/misc/LanguageTool-5.9/languagetool-commandline.jar")
(require 'langtool)

(customize-set-variable 'org-startup-with-latex-preview t)

(use-package! org-fragtog
 :hook (org-mode . (lambda ()
                     (add-hook 'evil-insert-state-entry-hook (lambda ()
                                                               (when (eq major-mode 'org-mode)
                                                                 (org-fragtog-mode +1))))
                     (add-hook 'evil-insert-state-exit-hook (lambda ()
                                                              (when (eq major-mode 'org-mode)
                                                                (progn
                                                                  (org-fragtog-mode -1)
                                                                  (if (org-inside-LaTeX-fragment-p) (org-latex-preview))))))))
)

;; Org-roam settings are directory local variables set for various directory similar to
;; Obsidian vaults.
(setq org-roam-directory "~/org")
(setq org-roam-dailies-directory "~/org/daily")
(setq org-roam-dailies-capture-templates
      (let ((head
             (concat "#+title: %<%Y-%m-%d (%A)>\n#+startup: showall\n* Daily Overview\n"
                     "#+begin_src emacs-lisp :results value raw\n"
                     "(mjs/get-daily-agenda \"%<%Y-%m-%d>\")\n"
                     "#+end_src\n"
                     "* [/] Do Today\n* [/] Possibly Today\n* Journal\n")))
        `(("j" "journal" entry
           "* %<%H:%M> %?"
           :if-new (file+head+olp "%<%Y-%m-%d>.org" ,head ("Journal")))
          ("t" "do today" item
           "[ ] %a"
           :if-new (file+head+olp "%<%Y-%m-%d>.org" ,head ("Do Today"))
           :immediate-finish t)
          ("m" "possibly today" item
           "[ ] %a"
           :if-new (file+head+olp "%<%Y-%m-%d>.org" ,head ("Possibly Today"))
           :immediate-finish t))))

(defun mjs/create-lecture-note ()
  (interactive)
  (let ((class (completing-read "Class: "
                                '("cs3000" "ma4710" "ma4790" "hu3120" "hu3693")))
        (buffer (get-buffer-create "Lecture Notes")))
    (set-buffer buffer)
    (insert (concat "#+filetags: " class "\n#+title: " class
                    " (" (format-time-string "%d %B %Y" (current-time)) ")\n\n"))
    (write-file (concat "/home/mjs/org/01-classes/" class "/"
                        (format-time-string "%Y-%m-%d" (current-time)) "-" class ".org"))
    (org-id-get-create)
    (switch-to-buffer buffer)))

(map! :map doom-leader-map
      "l" #'mjs/create-lecture-note)

;; Turn off link completeion everywhere
(setq org-roam-completion-everywhere nil)
;; Make a quick, easy to access keybinding to insert a link with roam
(map! :map org-mode-map
      :i "C-f" #'org-roam-node-insert
      "C-S-f" (lambda () (interactive)
                (let ((curent-prefix-arg '(4)))
                  (call-interactively #'org-insert-link))))

(defun mjs/toggle-and-mark-done ()
  "Toggle the current checkbox, follow the link under point and mark it as done"
  (interactive)
  (org-toggle-checkbox)
  (org-open-at-point)
  (org-todo 'done))

(map! :map org-mode-map
      :leader
      :localleader
      :prefix ("r" . "roam")
      :desc "Mark Daily Complete" :n "x" #'mjs/toggle-and-mark-done)

(defun mjs/get-daily-agenda (&optional date)
  "Return the agenda for requested day as a string"
  (interactive)
  (let ((file (make-temp-file "daily-agenda" nil ".txt")))
    (org-agenda nil "d" nil)
    (when date (org-agenda-goto-date date))
    (org-agenda-write file nil nil "*Org Agenda*")
    (kill-buffer)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (kill-line 2)
      (while (re-search-forward "^  " nil t)
        (replace-match "- " nil nil))
      (buffer-string))))

(defun mjs/org-roam-make-agenda-link ()
  (interactive)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char pos)
        (org-roam-dailies-capture-today)))))

(map! :map org-agenda-mode-map
      :leader
      :localleader
      :desc "Perform a task Today" :n "t" #'mjs/org-roam-make-agenda-link)

(use-package! vulpea
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-mode)))

(use-package! olivetti-mode
  :init (setq olivetti-body-width 100)
  :hook org-mode)

(map! :map org-mode-map
      :leader
      :localleader
      :prefix ("u" . "transclusions")
      :desc "Add Transclusion" :n "a" #'org-transclusion-add
      :desc "Activate All Transclusions" :n "u" #'org-transclusion-mode
      :desc "Remove Transclusion" :n "r" #'org-transclusion-remove)
