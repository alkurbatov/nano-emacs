((nil . ((format-all-formatters . (("Go" goimports
                                    (gci "--skip-generated" "--custom-order" "-s" "standard" "-s" "default" "-s" "prefix(github.com/alkurbatov/guppy)" "-s" "blank" "-s" "dot")
                                    gofumpt)))))
 (go-ts-mode
  (eval add-hook 'before-save-hook #'format-all-buffer nil 'local)))
