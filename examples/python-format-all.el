;; Apply code formatters for the python mode using format-all package.
((nil . ((format-all-formatters . (("Python" isort black)))))
 (python-ts-mode
  (eval add-hook 'before-save-hook #'format-all-buffer nil 'local)))
