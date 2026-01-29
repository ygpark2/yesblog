TAILWIND_DIR=./config/front
TAILWIND_INPUT=$(TAILWIND_DIR)/tailwind.input.css
TAILWIND_CONFIG=$(TAILWIND_DIR)/tailwind.config.js
TAILWIND_OUTPUT=./static/css/tailwind.css

.PHONY: tailwind
tailwind:
	@command -v tailwindcss >/dev/null 2>&1 || (cd $(TAILWIND_DIR) && npm install)
	npx --prefix $(TAILWIND_DIR) tailwindcss -c $(TAILWIND_CONFIG) -i $(TAILWIND_INPUT) -o $(TAILWIND_OUTPUT) --minify

.PHONY: rebuild
rebuild:
	@stack clean && stack build

.PHONY: start
start:
	@stack run yesblog

.PHONY: dev-start
dev-start:
	# @stack exec -- yesod devel
	@stack build --flag yesblog:dev && stack exec yesblog

.PHONY: start-bg
start-bg:
	@nohup stack run yesblog > ./yesblog.log 2>&1 & echo $$! > ./yesblog.pid

.PHONY: stop
stop:
	@if [ -f ./yesblog.pid ]; then \
		kill $$(cat ./yesblog.pid) || true; \
		rm -f ./yesblog.pid; \
	else \
		echo "No yesblog.pid found."; \
	fi

.PHONY: clean
clean:
	@rm ./data/*.sqlite*

.PHONY: restart
restart:
	$(MAKE) rebuild && $(MAKE) start
