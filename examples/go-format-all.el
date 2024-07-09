((nil . ((format-all-formatters . (("Go" goimports gci gofumpt)))))
 (go-ts-mode
  (eval add-hook 'before-save-hook #'format-all-buffer nil 'local)))
