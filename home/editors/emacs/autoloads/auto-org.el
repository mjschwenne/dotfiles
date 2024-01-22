;;;###autoload
(defun mjs/org-babel-remove-result-blocks (remove-all)
  (interactive "P")
  (let ((pos (point)))
    (org-babel-map-src-blocks nil
      (if (or remove-all (< pos end-block))
          (org-bable-remove-results)))))

;;;###autoload
(defun mjs/org-return (&optional indent arg interactive)
  "Automatically indent when calling `org-return'."
  (interactive)
  (org-return electric-indent-mode))

;;;###autoload
(defun mjs/org-capture-update-header ()
  (setq header-line-format
        (format "%s%s%s"
                (propertize (abbreviate-file-name
                             (buffer-file-name (buffer-base-buffer)))
                            'face 'font-lock-string-face)
                " ⟶ "
                (concat
                 "Capture Buffer. Finish "
                 (propertize "SPC c f" 'face 'help-key-binding)
                 ", refile "
                 (propertize "SPC c r" 'face 'help-key-binding)
                 ", abort "
                 (propertize "SPC c k" 'face 'help-key-binding)
                 " in normal mode."
                 ))))

;;;###autoload
(defun mjs/org-get-todo-keywords-for (&optional keyword)
  "Returns the list of todo keywords that KEYWORD belongs to."
  (when keyword
    (cl-loop for (type . keyword-spec)
             in (cl-remove-if-not #'listp org-todo-keywords)
             for keywords =
             (mapcar (lambda (x) (if (string-match "^\\([^(]+\\)(" x)
                                     (match-string 1 x)
                                   x))
                     keyword-spec)
             if (eq type 'sequence)
             if (member keyword keywords)
             return keywords)))

;;;###autoload
(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

;;;###autoload
(defun mjs/org-toggle-inline-images-in-subtree (&optional beg end refresh)
  "Refresh inline image previews in the current heading/tree."
  (let* ((beg (or beg
                  (if (org-before-first-heading-p)
                      (save-excursion (point-min))
                    (save-excursion (org-back-to-heading) (point)))))
         (end (or end
                  (if (org-before-first-heading-p)
                      (save-excursion (org-next-visible-heading 1) (point))
                    (save-excursion (org-end-of-subtree) (point)))))
         (overlays (cl-remove-if-not (lambda (ov) (overlay-get ov 'org-image-overlay))
                                     (ignore-errors (overlays-in beg end)))))
    (dolist (ov overlays nil)
      (delete-overlay ov)
      (setq org-inline-image-overlays (delete ov org-inline-image-overlays)))
    (when (or refresh (not overlays))
      (org-display-inline-images t t beg end)
      t)))

;;;###autoload
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
                     (or (car (mjs/org-get-todo-keywords-for (org-element-property :todo-keyword context)))
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
           (mjs/org-toggle-inline-images-in-subtree beg end)
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
               (mjs/org-toggle-inline-images-in-subtree
                (org-element-property :begin lineage)
                (org-element-property :end lineage))
             (org-open-at-point arg))))

        (`paragraph
         (mjs/org-toggle-inline-images-in-subtree))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
           (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           (mjs/org-toggle-inline-images-in-subtree
            (org-element-property :begin context)
            (org-element-property :end context))))))))

;;;###autoload
(defun mjs/org-table-previous-row ()
  "Go to the previous row (same column) in the current table. Before doing so,
re-align the table if necessary. (Necessary because org-mode has a
`org-table-next-row', but not `org-table-previous-row')"
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (if (and org-table-automatic-realign
           org-table-may-need-update)
      (org-table-align))
  (let ((col (org-table-current-column)))
    (beginning-of-line 0)
    (when (or (not (org-at-table-p)) (org-at-table-hline-p))
      (beginning-of-line))
    (org-table-goto-column col)
    (skip-chars-backward "^|\n\r")
    (when (org-looking-at-p " ")
      (forward-char))))

;;;###autoload
(defun mjs/org-insert-item (direction)
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
                 (mjs/org-table-previous-row))))

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
                   (car (mjs/org-get-todo-keywords-for todo-keyword)))
                  (todo-keyword)
                  ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))

;;;###autoload
(defun mjs/org-insert-item-below (count)
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  (dotimes (_ count) (mjs/org-insert-item 'below)))

;;;###autoload
(defun mjs/org-insert-item-above (count)
  "Inserts a new heading, table cell or item above the current one."
  (interactive "p")
  (dotimes (_ count) (mjs/org-insert-item 'above)))

;;;###autoload
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

;;;###autoload
(defun mjs/org-agenda-mark-habits ()
  "Mark all habits in current agenda for graph display.

When run just before `org-agenda-finalize' (such as by advice;
unfortunately, `org-agenda-finalize-hook' is run too late), this
has the effect of displaying consistency graphs for these habits."
  (when (not (get-text-property (point) 'org-series))
    (let ((cursor (point))
          item data)
      (while (setq cursor (next-single-property-change cursor 'org-marker))
        (setq item (get-text-property cursor 'org-marker))
        (when (and item (org-is-habit-p item))
          (with-current-buffer (marker-buffer item)
            (setq data (org-habit-parse-todo item)))
          (put-text-property cursor
                             (next-single-property-change cursor 'org-marker)
                             'org-habit-p data))))))

;;;###autoload
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

;;;###autoload
(defun mjs/class-capture ()
  (let* ((class (completing-read "Class: "
                                 '("cs400" "cs502" "cs538" "cs710" "cs760")
                                 nil t))
         (file-name (expand-file-name
                     (concat "classes/" class "/"
                             (format-time-string "%Y-%m-%d" (current-time))
                             "-" class ".org")
                     org-directory)))
    (setq mjs--capture-title class)
    (set-buffer (org-capture-target-buffer file-name))
    (goto-char (point-min))))

;;;###autoload
(defun mjs/capture-insert-id ()
  (save-excursion
    (goto-char (point-min))
    (org-id-get-create)))

;;;###autoload
(defun mjs/resize-org-latex-overlays ()
  (interactive)
  (cl-loop for o in (car (overlay-lists))
           if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
           do (plist-put (cdr (overlay-get o 'display))
                         :scale (expt text-scale-mode-step
                                      text-scale-mode-amount))))

;;;###autoload
(defun mjs/strip-org-roam-links ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[id:[^]]*\\]\\[\\([^]]*\\)\\]\\]" nil t)
      (replace-match "\\1" nil nil))))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun mjs/search-org-files (query)
  "Return a list of files in the org directory containing QUERY"
  (split-string
   (shell-command-to-string (format "rg \"%s\" %s" query org-directory))
   ":.*\n" t))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun mjs/regenerate-file-links-globally (&optional dir)
  (interactive "DDirectory: \n")
  (let ((dir (if (stringp dir) dir org-directory)))
    (dolist (file (directory-files-recursively dir ".*\\.org"))
      (mjs/regenerate-file-links file nil 'kill)
    )
))

;;;###autoload
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
                  ((string= (url-host parsed-url) "2e.aonprd.com")
                    (replace-regexp-in-string " - .*" "" title))
                  ((string= (url-host parsed-url) "en.wikipedia.org")
                   (replace-regexp-in-string " - .*" "" title))
                  ((string= (url-host parsed-url) "github.com")
                   (replace-regexp-in-string "^Github - " "" title))
                  ;; otherwise keep the original title
                  (t title))))
          ;; forward the title to the default org-cliplink transformer
          (org-cliplink-org-mode-link-transformer url clean-title)))))

;;;###autoload
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

;;;###autoload
(defun mjs/parent-tag (tag)
  "Search `org-tag-persistent-alist' for the parent of TAG"
  (if-let ((end-idx (cl-position `(,tag) org-tag-persistent-alist :test #'equal))
		   (group-start-idx (cl-position '(:grouptags) org-tag-persistent-alist
										 :test #'equal :from-end t :end end-idx)))

	  (car (nth (- group-start-idx 1) org-tag-persistent-alist))))

;;;###autoload
(defun mjs/all-parent-tags (tag)
  "Find all parent tags of TAG in `org-tag-persistent-alist'.

Returns a list including TAG itself."
  (let ((current-tag tag)
        (tag-list `(,tag)))
    (while-let ((parent-tag (mjs/parent-tag current-tag)))
      (progn
        (setq current-tag parent-tag)
        (push current-tag tag-list)))
    tag-list))

;;;###autoload
(defun mjs/org-auto-tags--set (tags)
  "Prompt user for more tags"
  (interactive
   (list
    (completing-read-multiple
     "Tag(s): " (org-roam-tag-completions) nil nil nil nil
     (s-join "," mjs/org-auto-tags--current-list))))
  (setq mjs/org-auto-tags--current-list tags))


;;;###autoload
(cl-defun mjs/org-auto-tags--set-by-context
    (context &key (context-plist mjs/org-context-plist))
  "Set `mjs/org-auto-tags--current-list' by CONTEXT.

Prompt for CONTEXT from CONTEXT-PLIST."
  (interactive
   (list
    (completing-read
     "Context: " (mjs/org-context-list-completing-read))))
  (setq mjs/org-auto-tags--current-list
        (plist-get
         (plist-get
          context-plist (intern (concat ":" context)))
         :tags)))

;;;###autoload
(defun mjs/org-roam-find-node
    (&optional
     other-window
     initial-input)
  "Call `org-roam-node-find' based on set tags."
  (interactive current-prefix-arg)
  (org-roam-node-find
   other-window
   initial-input
   #'mjs/org-roam-filter-context-fn
   nil
   :templates (mjs/org-roam-templates-context-fn)))
  
;;;###autoload
(defun mjs/org-roam-capture (&optional goto keys)
  "Call `org-roam-capture' with templates from context."
  (interactive "P")
  (org-roam-capture
   goto
   keys
   :filter-fn #'mjs/org-roam-filter-context-fn
   :templates (mjs/org-roam-templates-context-fn)))

;;;###autoload
(defun mjs/org-roam-node-insert ()
  "Call `org-roam-node-insert' based on context tags."
  (interactive)
  (org-roam-node-insert
   #'mjs/org-roam-filter-context-fn
   :templates (mjs/org-roam-templates-context-fn)))

(provide 'auto-org)
