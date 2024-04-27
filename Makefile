DEFAULT_GOAL := help

.PHONY: help
help: ## Display this help screen
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: install
install: ## Install Emacs configuration into the current system
	./scripts/install

.PHONY: clean-install
clean-install: ## Force cleanup of installed packages and install Emacs configuration into the current system
	CLEAN=1 ./scripts/install
