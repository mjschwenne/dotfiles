;;;###autoload
(defun mjs/restart-server ()
  "Restart the emacs daemon"
  (interactive)
  (server-force-delete)
  (while (server-running-p)
    (sleep-for 1))
  (server-start))

;;;###autoload
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

;;;###autoload
(defun mjs/restart-emacs ()
  (interactive)
  (restart-emacs (list "--with-profile" chemacs-profile-name)))

;;;###autoload
(defun mjs/switch-buffer ()
  (interactive)
  (consult-buffer '(consult--source-buffer)))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun mjs/delete-this-file (&optional trash)
  "Delete the file associated with `current-buffer'.
If TRASH is non-nil, the file is deleted without being moved to trash."
  (interactive "P")
  (when-let ((file (or (buffer-file-name)
                       (user-error "Current buffer is not visiting a file")))
             ((y-or-n-p "Delete this file? ")))
    (delete-file file trash)
    (kill-buffer (current-buffer))
    ;; If recentf mode is enabled, pruge the file from the list of
    ;; recent files.
    (when (bound-and-true-p recentf-mode)
      (recentf-remove-if-non-kept path))
    (message "Deleted %S" (abbreviate-file-name file))))

;;;###autoload
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
      (recentf-remove-if-non-kept old-path))
    (message "File moved to %S" (abbreviate-file-name new-path))))

(provide 'auto-buffer)
