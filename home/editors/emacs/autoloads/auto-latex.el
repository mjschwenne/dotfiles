;;;###autoload
(defun mjs/preview-scale-adjustment ()
  (setq preview-scale-function
        (lambda ()
          (* 0.8 (funcall (preview-scale-from-face))))))

;;;###autoload
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

;;;###autoload
(defun mjs/yas-try-expanding-auto-snippets ()
  (when (bound-and-true-p yas-minor-mode)
    (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
      (yas-expand))))

;;;###autoload
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

;;;###autoload
(defun mjs/yas-next-field-or-cdlatex nil
  "Jump to the next Yas field correctly with cdlatex active"
  (interactive)
    (if (or (bound-and-true-p cdlatex-mode)
            (bound-and-true-p org-cdlatex-mode))
        (cdlatex-tab)
      (yas-next-field-or-maybe-expand)))

;;;###autoload
(defun lazytab-position-cursor-and-edit ()
  (interactive)
  (cdlatex-position-cursor)
  (lazytab-orgtbl-edit))

;;;###autoload
(defun lazytab-orgtbl-edit ()
  (if org-cdlatex-mode
      (advice-add 'org-ctrl-c-ctrl-c :after #'lazytab-orgtbl-replace)
    (advice-add 'orgtbl-ctrl-c-ctrl-c :after #'lazytab-orgtbl-replace)
    (orgtbl-mode 1))
  (open-line 1)
  (insert "\n|"))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun lazytab-cdlatex-or-orgtbl-next-field ()
  (when (and (bound-and-true-p orgtbl-mode)
             (org-table-p)
             (looking-at "[[:space:]]*\\(?:|\\|$\\)")
             (let ((s (thing-at-point 'sexp)))
               (not (and s (assoc s cdlatex-command-alist-comb)))))
    (call-interactively #'org-table-next-field)
    t))

;;;###autoload
(defun lazytab-org-table-next-field-maybe ()
  (interactive)
  (if (bound-and-true-p cdlatex-mode)
      (cdlatex-tab)
    (org-table-next-field)))

(provide 'auto-latex)
