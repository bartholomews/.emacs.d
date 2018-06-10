;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					; Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode git
  "Toggle Git minor mode."
  ;; The initial value
  nil
  ;; The indicator for the mode line.
  " Git"
  ;; The minor mode bindings
  `((,(kbd "C-c s") . magit-status)))

(add-hook 'magit-mode-hook 'git) ;; add to Magit mode by default

(global-set-key (kbd "C-x g") 'magit-status)
