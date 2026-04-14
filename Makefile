.DEFAULT_GOAL := help
.PHONY: dev preview review yoink commit push help

dev: ## Hot reload (requires cargo-watch)
	cargo watch -w den_app/src -w den_macros/src -i den_macros/src/lib.rs \
		-s 'touch den_macros/src/lib.rs && cargo run --bin den_app'

preview: ## Gera preview/preview.html com todas as páginas
	cargo run --bin preview

review: ## Copia REVIEW_PROMPT.md + diff para o clipboard
	@{ cat REVIEW_PROMPT.md; printf '\n'; git diff HEAD; } | xclip -selection clipboard
	@printf '\033[32mReview prompt + diff copiado para o clipboard.\033[0m\n'

yoink: ## Copia o diff do último commit para o clipboard
	@git diff HEAD~1 HEAD | xclip -selection clipboard
	@printf '\033[32mDiff do último commit copiado.\033[0m\n'

commit: ## Gera mensagem de commit com IA e commita (interativo)
	@git diff --quiet HEAD && git diff --cached --quiet && [ -z "$$(git ls-files --others --exclude-standard)" ] \
		&& printf '\033[33mNenhuma mudança para commitar.\033[0m\n' && exit 0 || true
	@printf '\033[36mMudanças:\033[0m\n'; git status --short; echo
	@DIFF=$$(git diff HEAD); \
	UNTRACKED=$$(git ls-files --others --exclude-standard); \
	MSG=$$(printf 'Gere UMA mensagem de commit curta (max 72 chars) em inglês, formato convencional (feat/fix/refactor/docs/chore: descrição). Responda SOMENTE a mensagem, sem aspas.\n\nDiff:\n%s\n\nArquivos novos: %s' "$$DIFF" "$$UNTRACKED" | claude --print 2>/dev/null); \
	[ -z "$$MSG" ] && printf '\033[33mNão foi possível gerar. Digite:\033[0m ' && read -r MSG; \
	printf '\n\033[36mMensagem:\033[0m %s\n\n' "$$MSG"; \
	printf '\033[33mConfirmar? [Y/n/e(editar)] \033[0m'; read -r C; \
	case "$$C" in n|N) exit 0;; e|E) printf 'Digite a mensagem: '; read -r MSG;; esac; \
	git add -A && git commit -m "$$MSG" && printf '\033[32mCommit criado!\033[0m\n'

push: ## Commita com mensagem gerada por IA e faz push
	@git add -A
	@MSG=$$(git diff --cached | claude -p --model haiku \
		"Generate a short commit message in English, conventional commit format (feat/fix/refactor/docs/chore: description). Max 72 chars. Return ONLY the message, no quotes."); \
	[ -z "$$MSG" ] && printf '\033[33mDigite a mensagem:\033[0m ' && read -r MSG; \
	printf '\033[36mMessage:\033[0m %s\n' "$$MSG"; \
	git commit -m "$$MSG" && git push -u origin HEAD

help: ## Lista os comandos disponíveis
	@grep -E '^[a-zA-Z_-]+:.*##' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*## "}; {printf "  \033[36m%-10s\033[0m %s\n", $$1, $$2}'
