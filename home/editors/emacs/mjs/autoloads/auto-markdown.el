;;; auto-markdown --- auto-loading markdown functions -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; auto-loading functions for my personal Emacs configuration

;;; Code:

;;;###autoload
(defun mjs/markdown-optimize-src-buffer-modes (fn &rest args)
  "Inhibits the major mode hooks when fontifying markdown src blocks.

Merely wraps the call to `FN' with `ARGS' with the function `delay-mode-hooks'."
  (delay-mode-hooks (apply fn args)))

;;;###autoload
(defun mjs/markdown-disable-front-matter-fontification (&rest _)
  "HACK: Prevent mis-fontification of YAML metadata blocks in `markdown-mode'."
  (ignore (goto-char (point-max))))

;;;###autoload
(defun mjs/markdown-insert-strikethrough ()
  "Surround region in github strike-through delimiters."
  (interactive)
  (let ((regexp "\\(^\\|[^\\]\\)\\(\\(~\\{2\\}\\)\\([^ \n	\\]\\|[^ \n	]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(\\3\\)\\)")
        (delim "~~"))
    (if (markdown-use-region-p)
        ;; Active region
        (cl-destructuring-bind (beg . end)
            (markdown-unwrap-things-in-region
             (region-beginning) (region-end)
             regexp 2 4)
          (markdown-wrap-or-insert delim delim nil beg end))
      ;; Bold markup removal, bold word at point, or empty markup insertion
      (if (thing-at-point-looking-at regexp)
          (markdown-unwrap-thing-at-point nil 2 4)
        (markdown-wrap-or-insert delim delim 'word nil nil)))))

(provide 'auto-markdown)
;;; auto-markdown.el ends here
