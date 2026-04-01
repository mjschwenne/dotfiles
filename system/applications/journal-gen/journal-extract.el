;; -*- lexical-binding: t; -*-

;;; journal-extract.el --- Extract daily journal data to JSON for Typst rendering
;;;
;;; Usage:
;;;   emacs --batch -l journal-extract.el \
;;;         --eval '(mjs/je-extract-journal "/path/to/YYYY-MM-DD.org")'
;;;
;;; Outputs a JSON object to stdout:
;;;   {
;;;     "date": "2026-03-31 Tuesday",
;;;     "planned":   [{"todo": "NEXT", "priority": null, "deadline": null, "heading": "..."}],
;;;     "plan_text": "Free text from Plan for Today",
;;;     "completed": [{"todo": "DONE", "priority": null, "deadline": null, "heading": "..."}]
;;;   }

;;; Code:

(require 'org)
(require 'org-element)
(require 'json)

(defun mjs/je-cell-text (cell)
  "Extract plain text from a TABLE-CELL element.
Returns nil for empty cells, handling both plain strings and link nodes."
  (when cell
    (let* ((contents (org-element-contents cell))
           (text (mapconcat
                  (lambda (item)
                    (cond
                     ((stringp item) (string-trim item))
                     ((eq (org-element-type item) 'link)
                      (string-trim
                       (mapconcat (lambda (c) (if (stringp c) c ""))
                                  (org-element-contents item) "")))
                     (t "")))
                  contents "")))
      (if (string-empty-p text) nil text))))

(defun mjs/je-section-of (headline)
  "Return the direct section child of HEADLINE, or nil."
  (seq-find (lambda (el) (eq (org-element-type el) 'section))
            (org-element-contents headline)))

(defun mjs/je-extract-table-rows (headline)
  "Extract data rows from the first table in HEADLINE's section.
Skips the header row and horizontal rule rows.
Returns a list of alists with keys: todo, priority, deadline, heading."
  (when-let ((section (mjs/je-section-of headline)))
    (when-let ((table (org-element-map section 'table #'identity nil t)))
      (let (rows header-seen)
        (org-element-map table 'table-row
          (lambda (row)
            (when (eq (org-element-property :type row) 'standard)
              (if (not header-seen)
                  (setq header-seen t)
                (let ((cells (org-element-contents row)))
                  (push `((todo     . ,(mjs/je-cell-text (nth 0 cells)))
                          (priority . ,(mjs/je-cell-text (nth 1 cells)))
                          (deadline . ,(mjs/je-cell-text (nth 2 cells)))
                          (heading  . ,(mjs/je-cell-text (nth 3 cells))))
                        rows))))))
        (nreverse rows)))))

(defun mjs/je-find-headline (parent title)
  "Find the first headline with TITLE anywhere under PARENT."
  (org-element-map parent 'headline
    (lambda (hl)
      (when (string= (org-element-property :raw-value hl) title)
        hl))
    nil t))

(defun mjs/je-org-to-typst (text)
  "Convert org inline markup in TEXT to Typst equivalents.
Handles verbatim, code, italic, underline, and strikethrough.
Bold (*...*) is identical in both and requires no conversion.
Note: literal # in text may interfere with Typst rendering."
  (thread-last text
               (replace-regexp-in-string "[[:space:]]*\n[[:space:]]*" " ")
               (replace-regexp-in-string "=\\([^=\n]+\\)=" "`\\1`")
               (replace-regexp-in-string "~\\([^~\n]+\\)~" "`\\1`")
               (replace-regexp-in-string "/\\([^/\n]+\\)/" "_\\1_")
               (replace-regexp-in-string "_\\([^_\n]+\\)_" "#underline[\\1]")
               (replace-regexp-in-string "\\+\\([^+\n]+\\)\\+" "#strike[\\1]")))

(defun mjs/je-extract-paragraph-text (headline)
  "Extract and concatenate all paragraph text in HEADLINE's section.
Org inline markup is converted to Typst equivalents."
  (when-let ((section (mjs/je-section-of headline)))
    (let (texts)
      (org-element-map section 'paragraph
        (lambda (p)
          (push (string-trim
                 (buffer-substring-no-properties
                  (org-element-property :contents-begin p)
                  (org-element-property :contents-end p)))
                texts)))
      (mjs/je-org-to-typst (string-join (nreverse texts) "\n\n")))))

(defun mjs/je-extract-journal (file)
  "Extract journal data from FILE and print as JSON to stdout."
  (let* ((buf (find-file-noselect (expand-file-name file)))
         (json-str
          (with-current-buffer buf
            (let* ((tree (org-element-parse-buffer))
                   (title (org-element-map tree 'keyword
                            (lambda (kw)
                              (when (string= (org-element-property :key kw) "TITLE")
                                (org-element-property :value kw)))
                            nil t))
                   (planning-hl    (mjs/je-find-headline tree "Planning"))
                   (plan-today-hl  (mjs/je-find-headline planning-hl "Plan for Today"))
                   (review-hl      (mjs/je-find-headline tree "Review"))
                   (reflection-hl  (mjs/je-find-headline review-hl "Reflection"))
                   (tomorrow-hl    (mjs/je-find-headline review-hl "Plan for Tomorrow"))
                   (data `((date       . ,title)
                           (planned    . ,(vconcat (mjs/je-extract-table-rows planning-hl)))
                           (plan_text  . ,(or (mjs/je-extract-paragraph-text plan-today-hl) ""))
                           (completed  . ,(vconcat (mjs/je-extract-table-rows review-hl)))
                           (reflection . ,(or (mjs/je-extract-paragraph-text reflection-hl) ""))
                           (tomorrow . ,(or (mjs/je-extract-paragraph-text tomorrow-hl) "")))))
              (json-encode data)))))
    (kill-buffer buf)
    (princ json-str)
    (terpri)))

(provide 'journal-extract)
;;; journal-extract.el ends here
