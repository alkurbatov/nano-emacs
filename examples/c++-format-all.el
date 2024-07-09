;; Apply code formatters for the c++ mode using format-all package.
((nil . ((format-all-formatters . (("C++" clang-format)))))
 (c++-ts-mode
  (eval add-hook 'before-save-hook #'format-all-buffer nil 'local)))
