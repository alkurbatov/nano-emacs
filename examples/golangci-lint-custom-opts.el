((go-ts-mode
  (eval progn
        (setq-local flymake-collection-golangci-lint-args '("--config" "../golangci-lint-template/.golangci.yml")))))
