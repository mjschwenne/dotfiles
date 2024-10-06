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
(set-frame-parameter (selected-frame) 'alpha-background '90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/")

(map! :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right)

;; Make evil-snipe search the whole buffer by default
(customize-set-variable 'evil-snipe-scope 'buffer)
(customize-set-variable 'evil-want-C-u-scroll t)

;; Configuration for Built-in Modes

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
(setq confirm-kill-emacs nil)

;; Setup autoloads, I'm currently targeting user facing functions not required to load the system
(add-to-list 'load-path (expand-file-name "autoloads" doom-user-dir))
(loaddefs-generate
 (expand-file-name "autoloads" doom-user-dir)
 (expand-file-name "autoloads/auto.el" doom-user-dir))
(require 'auto)

(after! vertico
  (setq vertico-resize t)
  (vertico-reverse-mode t))

;; Ligatures
;; Enable them in text modes like org and markdown...
;; (add-to-list '+ligatures-alist
;;              '(text-mode
;;                "|||>" "<|||" "<==>" "<!--" "####" "~~>" "||=" "||>"
;;                ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
;;                "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
;;                "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
;;                "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
;;                "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
;;                "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
;;                "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
;;                ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
;;                "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
;;                "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
;;                "?=" "?." "??" ";;" "/*" "/>" "//" "__" "~~" "(*" "*)"
;;                "\\\\" "://"))
;; while removing them from coq mode so that I can see what's a unicode symbol and
;; that's not.
(add-to-list '+ligatures-in-modes 'coq-mode t)

(use-package! org
  :after org
  :custom ((org-entities-user
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
           (org-startup-with-latex-preview t)
           (org-preview-latex-default-process 'dvisvgm)
           (org-startup-align-all-tables t)
           (org-startup-folded 'showall)
           (org-startup-with-inline-images t)
           (org-image-actual-width 600)
           (org-ellipsis " ▾")
           (org-agenda-start-with-log-mode t)
           (org-log-done 'time)
           (org-log-into-drawer t)
           (org-treat-insert-todo-heading-as-state-change t)
           (org-agenda-start-day nil)
           (org-agenda-hide-tags-regexp ".")
           (org-agenda-files (list (concat org-directory "agenda/")))
           (org-agenda-custom-commands
            '(("d" "MJS Daily"
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
                (tags "SCHEDULED<=\"<today>\"-TODO=\"DONE\"-TODO=\"KILL\"-STYLE=\"habit\""
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
           (org-outline-path-complete-in-steps nil))
  :hook ((org-mode . auto-fill-mode))
  :config
  ;; Update one very important keybinding
  (map! :map org-mode-map :n "RET" #'+org/dwim-at-point)
  ;; Regular org capture templates
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
          ("g" "Grackle" entry
           (file "agenda/grackle.org")
           ,(concat "* %<%Y-%m-%d> %?\n"
                    "** Recap\n"
                    "** Goals\n"
                    "*** Primary\n"
                    "*** Secondary\n")
           :empty-lines 1
           :immediate-finish t
           :jump-to-captured t)
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
          ("t" "Tabletop Gaming")
          ("te" "Etera Session" entry
           (file "ttrpg/games/etera/notes.org")
           "* Session %<%Y-%m-%d>\n\n%?"
           :empty-lines 1
           :jump-to-captured t
           :immediate-finish t)
          ("tg" "Graves Session" entry
           (file "ttrpg/games/graves-and-groves/sessions.org")
           "* Session %<%Y-%m-%d>\n\n%?"
           :empty-lines 1
           :prepend t
           :jump-to-captured t
           :immediate-finish t)
          ("to" "Obscured Realms Session" entry
           (file "ttrpg/games/obscured-realms/sessions.org")
           "* Session %<%Y-%m-%d>\n\n%?"
           :empty-lines 1
           :jump-to-captured t
           :immediate-finish t)))
  ;; Load build-in org modules
  (add-to-list 'org-modules 'org-habit t)
  (advice-add #'org-agenda-finalize :before #'mjs/org-agenda-mark-habits)
  (add-to-list 'org-modules 'org-tempo t)

  ;; Load some helpful latex pacakges for all latex fragments
  (add-to-list 'org-latex-packages-alist '("" "sfmath" t))
  (add-to-list 'org-latex-packages-alist '("margin=1in" "geometry" t))
  (add-to-list 'org-latex-packages-alist '("" "parskip" t))
  (add-to-list 'org-latex-packages-alist '("" "nicematrix" t))
  (add-to-list 'org-latex-packages-alist '("" "amsthm" t))
  (add-to-list 'org-latex-packages-alist '("" "cancel" t))

  ;; Load babel langagues
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (dot . t)
     (emacs-lisp . t)
     (latex . t)
     (python . t)
     (R . t)
     (java . t))))

(use-package! org-habit
  :custom-face
  (org-habit-ready-face ((t (:background "#8fbcbb"))))
  (org-habit-ready-future-face ((t (:background "#8fbcbb"))))
  (org-habit-clear-face ((t (:background "#5e81ac"))))
  (org-habit-clear-future-face ((t (:background "#5e81ac"))))
  (org-habit-alert-face ((t (:background "#ebcb8b"))))
  (org-habit-alert-future-face ((t (:background "#ebcb8b"))))
  (org-habit-overdue-face ((t (:background "#bf616a"))))
  (org-habit-overdue-future-face ((t (:background "#bf616a")))))

(use-package! org-tempo
  :custom (org-structure-template-alist '(("t" . "LaTeX latex")
                                          ("j" . "src java")
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
                                          ("l" . "export latex")
                                          ("q" . "quote")
                                          ("s" . "src")
                                          ("v" . "verse"))))

(after! org-roam
  (setq org-roam-directory (file-truename org-directory)))

(use-package! vulpea
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-mode)))

(use-package! olivetti-mode
  :init (setq olivetti-body-width 100)
  :hook org-mode)

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

(use-package! org-fragtog
  :hook (org-mode . (lambda ()
                      (add-hook 'evil-insert-state-entry-hook (lambda ()
                                                                (when (eq major-mode 'org-mode)
                                                                  (org-fragtog-mode +1))))
                      (add-hook 'evil-insert-state-exit-hook (lambda ()
                                                               (when (eq major-mode 'org-mode)
                                                                 (progn
                                                                   (org-fragtog-mode -1)
                                                                   (if (org-inside-LaTeX-fragment-p) (org-latex-preview)))))))))

(use-package! org-modern
  :init (global-org-modern-mode)
  :config (set-face-attribute 'org-modern-done nil
                              :background "#4c566a" :foreground "#eceff4"))

;; Citations
(use-package! oc
  :custom ((org-cite-global-bibliography '("~/Documents/zotero.bib"))
           (org-cite-csl-styles-dir "~/Zotero/sytles/")
           (org-cite-export-processors '((latex biblatex)
                                         (md . (csl "ieee.csl"))
                                         (t . (csl "ieee.csl")))))
  :custom-face
  (org-cite ((t (:foreground "#a3be8c"))))
  (org-cite-key ((t (:foreground "#a3be8c" :slant italic))))
  :config
  (map! :localleader
        :map org-mode-map
        :desc "Citation"
        :n "C" #'org-cite-insert))

(use-package! citar
  :after oc
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

(use-package! citar-org
  :after citar
  :custom ((org-cite-insert-processor 'citar)
           (org-cite-follow-processor 'citar)
           (org-cite-activate-processor 'citar)))

(use-package! citar-embark
  :after citar
  :diminish citar-embark-mode
  :no-require
  :init (setq citar-at-point-function 'embark-act)
  :hook ((org-mode . citar-embark-mode)
         (LaTeX-mode . citar-embark-mode)))

;; Coq
(use-package! proof-general
  :hook (coq-mode . (lambda ()
                      (set-face-background 'proof-locked-face
                                           "#3b4252")))
  :custom
  (coq-smie-user-tokens
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
     )
   )
  )

(use-package! math-symbol-lists
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
   ("\\fun"    ?λ)
   ("\\mult"   ?⋅)
   ("\\ent"    ?⊢)
   ("\\valid"  ?✓)
   ("\\diamond" ?◇)
   ("\\box"    ?□)
   ("\\bbox"   ?■)
   ("\\later"  ?▷)
   ("\\pred"   ?φ)
   ("\\and"    ?∧)
   ("\\or"     ?∨)
   ("\\comp"   ?∘)
   ("\\ccomp"  ?◎)
   ("\\all"    ?∀)
   ("\\ex"     ?∃)
   ("\\to"     ?→)
   ("\\sep"    ?∗)
   ("\\lc"     ?⌜)
   ("\\rc"     ?⌝)
   ("\\Lc"     ?⎡)
   ("\\Rc"     ?⎤)
   ("\\lam"    ?λ)
   ("\\empty"  ?∅)
   ("\\Lam"    ?Λ)
   ("\\Sig"    ?Σ)
   ("\\-"      ?∖)
   ("\\aa"     ?●)
   ("\\af"     ?◯)
   ("\\auth"   ?●)
   ("\\frag"   ?◯)
   ("\\iff"    ?↔)
   ("\\gname"  ?γ)
   ("\\incl"   ?≼)
   ("\\latert" ?▶)
   ("\\update" ?⇝)

   ;; accents (for iLöb)
   ("\\\"o" ?ö)

   ;; subscripts and superscripts
   ("^^+" ?⁺) ("__+" ?₊) ("^^-" ?⁻)
   ("__0" ?₀) ("__1" ?₁) ("__2" ?₂) ("__3" ?₃) ("__4" ?₄)
   ("__5" ?₅) ("__6" ?₆) ("__7" ?₇) ("__8" ?₈) ("__9" ?₉)

   ("__a" ?ₐ) ("__e" ?ₑ) ("__h" ?ₕ) ("__i" ?ᵢ) ("__k" ?ₖ)
   ("__l" ?ₗ) ("__m" ?ₘ) ("__n" ?ₙ) ("__o" ?ₒ) ("__p" ?ₚ)
   ("__r" ?ᵣ) ("__s" ?ₛ) ("__t" ?ₜ) ("__u" ?ᵤ) ("__v" ?ᵥ) ("__x" ?ₓ))
  (mapc (lambda (x)
          (if (cddr x)
              (quail-defrule (cadr x) (car (cddr x)))))
                                        ; need to reverse since different emacs packages disagree on whether
                                        ; the first or last entry should take priority...
                                        ; see <https://mattermost.mpi-sws.org/iris/pl/46onxnb3tb8ndg8b6h1z1f7tny> for discussion
        (reverse (append math-symbol-list-basic math-symbol-list-extended))))

;; Programming
(use-package! rainbow-mode
  :hook prog-mode)

;; Media
(use-package! ready-player
  :config (ready-player-mode +1))

;; LaTeX
(after! latex
  (add-hook! LaTeX-mode
             #'prettify-symbols-mode
             #'auto-fill-mode
             #'TeX-fold-mode
             #'TeX-PDF-mode)
  (customize-set-variable 'TeX-newline-function #'reindent-then-newline-and-indent)
  (customize-set-variable 'TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)
                                                   ("Zathura" "zathura --synctex-forward :: %o")))
  (customize-set-variable 'TeX-view-program-selection '((output-pdf "PDF Tools")))
  (customize-set-variable 'TeX-source-correlate-start-server t)
  (customize-set-variable 'TeX-save-query nil)
  (customize-set-variable 'TeX-engine 'luatex)
  (customize-set-variable 'TeX-parse-self t)
  (customize-set-variable 'TeX-auto-save t)
  (map! :map LaTeX-mode-map :localleader
        :desc "Compile Document" :n "c" #'TeX-command-run-all
        :desc "Clean Document" :n "C" #'TeX-clean
        :desc "Indent Line" :n "i" #'LaTeX-indent-line
        :desc "View PDF" :n "v" #'TeX-view))

(after! yasnippet
  (add-hook! 'post-self-insert-hook #'mjs/yas-try-expanding-auto-snippets)
  (customize-set-variable 'yas-triggers-in-field t)
  (map! :map yas-keymap
        :i "<tab>" #'mjs/yas-next-field-or-cdlatex
        :i "TAB" #'mjs/yas-next-field-or-cdlatex))

(after! cdlatex
  (add-hook! LaTeX-mode-hook #'turn-on-cdlatex)
  (add-hook! cdlatex-tab-hook #'yas-expand #'mjs/cdlatex-in-yas-field)
  (customize-set-variable 'textmathp-tex-commands '(("bmatrix" env-on)
                                                    ("pmatrix" env-on)
                                                    ("mathpar" env-on)))
  (customize-set-variable 'cdlatex-env-alist '(("proof" "\\begin{proof}\n?\n\\end{proof}" nil)
                                               ("optp" "\\begin{array}{r@{\\quad}l}\n\\min & ? \ns.t. & \n\\end{array}" nil)))
  (add-to-list 'cdlatex-command-alist '("proof" "Insert proof env" ""
                                        cdlatex-environment ("proof") t nil))
  (add-to-list 'cdlatex-command-alist '("emph" "Insert emphasis" "\\emph{?}"
                                        cdlatex-position-cursor nil t nil))
  (map! :map LaTeX-mode-map
        :i "<tab>" #'cdlatex-tab))

(use-package! texpresso
  :hook LaTeX-mode
  :config
  (map! :map LaTeX-mode-map :localleader :desc "TeXpresso" :n "t" #'texpresso))
