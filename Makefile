dev: ## Hot reload: monitora alterações e re-roda a aplicação
	cargo watch -w den_app/src -w den_macros/src -x 'run -p den_app'

review: ## Copia diff para clipboard com prompt de code review
	@echo -e "Antes de tudo procure boas práticas para aplicações Rust e egui, depois encarne um desenvolvedor full-stack Rust Angular sênior. Faça um code review das alterações abaixo, considere que você está fazendo code review de uma aplicação em Rust que usa proc macros para compilar templates HTML + SCSS em código egui nativo em compile time (o dev escreve HTML para estrutura, SCSS para estilos, e Rust para lógica — a macro transforma tudo em chamadas egui automaticamente). Seja especialmente chato com: magic numbers (devem estar em um config.rs ou constantes nomeadas), variáveis inline sem nome descritivo, erros genéricos fora de um error.rs centralizado, unwrap() sem justificativa (proponha solução com ? ou expect com mensagem), e warnings do clippy (rode cargo clippy mentalmente e aponte violações).\n\n$$(git diff HEAD)" | xclip -selection clipboard
	@echo "\033[32mDiff copiado para o clipboard com prompt de review.\033[0m"

commit: ## Gera mensagem de commit com IA e commita
	@echo "\033[36mAnalisando mudanças...\033[0m"
	@if git diff --quiet HEAD 2>/dev/null && git diff --cached --quiet 2>/dev/null && [ -z "$$(git ls-files --others --exclude-standard)" ]; then \
		echo "\033[33mNenhuma mudança para commitar.\033[0m"; \
		exit 0; \
	fi
	@echo ""
	@echo "\033[36mMudanças detectadas:\033[0m"
	@git status --short
	@echo ""
	@MSG=$$(git diff HEAD --stat 2>/dev/null | tail -1); \
	DIFF=$$(git diff HEAD 2>/dev/null); \
	UNTRACKED=$$(git ls-files --others --exclude-standard); \
	PROMPT="Gere UMA mensagem de commit curta (max 72 chars) em inglês, no formato convencional (feat/fix/refactor/docs/chore: descrição). Responda SOMENTE a mensagem, sem explicação, sem aspas. Baseado nestas mudanças:\n\nStats: $$MSG\n\nDiff (resumo):\n$$(echo "$$DIFF" | head -200)\n\nArquivos novos: $$UNTRACKED"; \
	COMMIT_MSG=$$(echo "$$PROMPT" | claude --print 2>/dev/null); \
	if [ -z "$$COMMIT_MSG" ]; then \
		echo "\033[33mNão foi possível gerar mensagem. Digite manualmente:\033[0m"; \
		read -r COMMIT_MSG; \
	fi; \
	echo ""; \
	echo "\033[36mMensagem:\033[0m $$COMMIT_MSG"; \
	echo ""; \
	printf "\033[33mConfirmar? [Y/n/e(editar)] \033[0m"; \
	read -r CONFIRM; \
	case "$$CONFIRM" in \
		n|N) echo "Cancelado."; exit 0 ;; \
		e|E) printf "\033[33mDigite a mensagem:\033[0m "; read -r COMMIT_MSG ;; \
	esac; \
	git add -A && git commit -m "$$COMMIT_MSG" && \
	echo "" && echo "\033[32mCommit criado com sucesso!\033[0m"
