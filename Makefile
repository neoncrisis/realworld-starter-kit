# STACK_INSTALL_ROOT=$(shell stack path --local-install-root)

help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

server: ## Run the server in development mode
	@which ghcid > /dev/null || stack install ghcid
	@ghcid \
		--command "stack ghci haskell-servant-realworld" \
		--restart "package.yaml" \
		--restart "stack.yaml" \
		--restart ".ghci" \
		--warnings \
		--test "DevelMain.update"

tests: ## Run the server in development mode
	@which ghcid > /dev/null || stack install ghcid
	@ghcid \
		--command "stack ghci :spec" \
		--restart "package.yaml" \
		--restart "stack.yaml" \
		--restart ".ghci" \
		--warnings \
		--test ":main"
