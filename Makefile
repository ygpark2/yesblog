TAILWIND_DIR=./config/front
TAILWIND_INPUT=$(TAILWIND_DIR)/tailwind.input.css
TAILWIND_CONFIG=$(TAILWIND_DIR)/tailwind.config.js
TAILWIND_OUTPUT=./static/css/tailwind.css
FRONTEND_DIR=./frontend

.PHONY: tailwind
tailwind:
	@command -v tailwindcss >/dev/null 2>&1 || (cd $(TAILWIND_DIR) && npm install)
	npx --prefix $(TAILWIND_DIR) tailwindcss -c $(TAILWIND_CONFIG) -i $(TAILWIND_INPUT) -o $(TAILWIND_OUTPUT) --minify

.PHONY: frontend-build
frontend-build:
	@command -d $(FRONTEND_DIR)/node_modules >/dev/null 2>&1 || (cd $(FRONTEND_DIR) && npm install)
	@cd $(FRONTEND_DIR) && npm run build

.PHONY: rebuild
rebuild:
	@stack clean && stack build

.PHONY: start
start:
	@stack run yesblog

.PHONY: dev-start
dev-start:
	# @stack exec -- yesod devel
	@YESOD_PORT=$${YESOD_PORT:-3900}; export YESOD_PORT; \
	$(MAKE) frontend-build && \
	stack build --flag yesblog:dev && \
	echo "YesBlog dev server: http://localhost:$$YESOD_PORT" && \
	stack exec yesblog

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
