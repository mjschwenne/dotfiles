(customize-set-variable 'crafted-startup-inhibit-splash nil)

  ;; Set transparency
  (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
