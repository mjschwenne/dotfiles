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
;;;
;;; Every string field except "date" is Typst *markup*, so journal.typ must
;;; render it with `eval(..., mode: "markup")'.  Text is converted by walking
;;; the org parse tree rather than by regexp, so markup nested inside links
;;; (a link description like [[file:...][Read /Relax!/]]) survives, and any
;;; character that is special to Typst -- `$' above all -- is escaped
;;; automatically.  Nothing needs to be escaped by hand in the org file.

;;; Code:

(require 'org)
(require 'org-element)
(require 'json)

;;;; Org markup -> Typst markup

(defconst mjs/je-typst-escape-re
  "\\\\\\([[:punct:]]\\)\\|\\([]$*_`~@<>[/\\#]\\)"
  "Match either an author's backslash escape or a Typst special character.
Both alternatives are rewritten to a Typst escape, so `\\$5' and `$5'
in the org file render identically.  Hyphen and period are deliberately
absent, so Typst keeps forming en/em dashes and ellipses in prose.")

(defun mjs/je-typst-text (text)
  "Convert plain org TEXT into escaped Typst markup.
Newlines are folded into spaces, since paragraph filling in the org
file is not meaningful to Typst."
  (replace-regexp-in-string
   mjs/je-typst-escape-re
   (lambda (match)
     (concat "\\" (or (match-string 1 match) (match-string 2 match))))
   (replace-regexp-in-string "[ \t]*\n[ \t]*" " " text)
   t t))

(defun mjs/je-typst-raw (value)
  "Render VALUE as inline Typst raw text."
  (format "#raw(\"%s\")"
          (replace-regexp-in-string "[\\\"]" "\\\\\\&" (or value ""))))

(defun mjs/je-objects-to-typst (objects)
  "Convert a list of org OBJECTS to a Typst markup string."
  (mapconcat #'mjs/je-object-to-typst objects ""))

(defun mjs/je-object-to-typst (object)
  "Convert a single org OBJECT to a Typst markup string.
Emphasis is emitted in function form (#emph[...] rather than _..._)
because Typst's shorthands only take effect at word boundaries.
Whitespace trailing an object belongs to the object rather than to the
next text node, so it is restored from :post-blank."
  (if (stringp object)
      (mjs/je-typst-text object)
    (concat
     (mjs/je-object-body-to-typst object)
     (make-string (or (org-element-property :post-blank object) 0) ?\s))))

(defun mjs/je-object-body-to-typst (object)
  "Convert OBJECT itself, ignoring any whitespace that trails it."
  (let ((contents (org-element-contents object)))
    (pcase (org-element-type object)
      ('bold           (format "#strong[%s]"    (mjs/je-objects-to-typst contents)))
      ('italic         (format "#emph[%s]"      (mjs/je-objects-to-typst contents)))
      ('underline      (format "#underline[%s]" (mjs/je-objects-to-typst contents)))
      ('strike-through (format "#strike[%s]"    (mjs/je-objects-to-typst contents)))
      ((or 'code 'verbatim) (mjs/je-typst-raw (org-element-property :value object)))
      ('entity         (mjs/je-typst-text (or (org-element-property :utf-8 object) "")))
      ('line-break     "#linebreak()")
      ;; Only braced sub/superscripts are intentional; org parses the bare
      ;; form eagerly, which would otherwise mangle words like snake_case.
      ((and (or 'subscript 'superscript) type)
       (let ((body (mjs/je-objects-to-typst contents)))
         (if (org-element-property :use-brackets-p object)
             (format (if (eq type 'subscript) "#sub[%s]" "#super[%s]") body)
           (concat (if (eq type 'subscript) "\\_" "\\^") body))))
      ;; A bracket link renders its description; a bare link, its target.
      ('link (if contents
                 (mjs/je-objects-to-typst contents)
               (mjs/je-typst-text (or (org-element-property :raw-link object) ""))))
      (_ (if contents
             (mjs/je-objects-to-typst contents)
           (mjs/je-typst-text
            (substring-no-properties
             (or (org-element-interpret-data object) ""))))))))

;;;; Extraction

(defun mjs/je-cell-text (cell)
  "Extract Typst markup from a TABLE-CELL element.
Returns nil for empty cells."
  (when cell
    (let ((text (string-trim (mjs/je-objects-to-typst (org-element-contents cell)))))
      (if (string-empty-p text) nil text))))

(defun mjs/je-section-of (headline)
  "Return the direct section child of HEADLINE, or nil."
  (seq-find (lambda (el) (eq (org-element-type el) 'section))
            (org-element-contents headline)))

(defun mjs/je-extract-table-rows (headline)
  "Extract data rows from the first table in HEADLINE's section.
Skips the header row and horizontal rule rows.
Returns a list of alists with keys: todo, priority, deadline, heading."
  (when-let* ((section (mjs/je-section-of headline))
              (table (org-element-map section 'table #'identity nil t)))
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
      (nreverse rows))))

(defun mjs/je-find-headline (parent title)
  "Find the first headline with TITLE anywhere under PARENT."
  (org-element-map parent 'headline
    (lambda (hl)
      (when (string= (org-element-property :raw-value hl) title)
        hl))
    nil t))

(defun mjs/je-extract-paragraph-text (headline)
  "Extract every paragraph in HEADLINE's section as Typst markup.
Paragraphs are joined by a blank line, which Typst reads as a break."
  (when-let* ((section (mjs/je-section-of headline)))
    (let (texts)
      (org-element-map section 'paragraph
        (lambda (p)
          (let ((text (string-trim (mjs/je-objects-to-typst (org-element-contents p)))))
            (unless (string-empty-p text)
              (push text texts)))))
      (string-join (nreverse texts) "\n\n"))))

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
