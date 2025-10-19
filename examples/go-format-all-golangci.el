((nil . ((format-all-formatters . (("Go" goimports golangci-lint)))))
 (go-ts-mode
  (eval add-hook 'before-save-hook #'format-all-buffer nil 'local)))
