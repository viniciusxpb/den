.DEFAULT_GOAL := help
.PHONY: dev preview test lint-css-rules review yoink commit push c component help

dev: ## Hot reload (requires cargo-watch)
	cargo watch -w den_app/src -w den_macros/src -i den_macros/src/lib.rs \
		-s 'touch den_macros/src/lib.rs && cargo run --bin den_app'

preview: ## Gera preview/preview.html com todas as páginas
	cargo run --bin preview

test: lint-css-rules ## Roda guardas + cargo test + clippy (deny warnings) em todo o workspace
	cargo test --workspace --all-targets
	cargo clippy --workspace --all-targets -- -D warnings

# Guarda da regra "toda propriedade CSS é Option<T>" em StyleRule/DenVisual.
# Documentação completa: den_macros/src/types/style.rs (topo do arquivo) e REVIEW_PROMPT.md (regra 6).
# Quebra build se algum modelo (ou humano) tentar voltar pra enum/bool direto, que mata cascade.
# Procura SÓ nos 2 arquivos onde a regra se aplica — não polui outras estruturas.
.PHONY: lint-css-rules
lint-css-rules:
	@FILES="den_macros/src/types/style.rs den_macros/src/types/resolved.rs"; \
	BAD_FIELDS=$$(grep -nE 'pub +(display|width|height|cursor_pointer|flex_grow|position|flex_direction|align_items|justify_content|box_shadows|overflow|transform): +(DisplayMode|WidthValue|PositionKind|FlexDirection|AlignItems|JustifyContent|OverflowKind|Transform2d|bool) *,|pub +box_shadows: +Vec<' $$FILES 2>/dev/null || true); \
	BAD_MERGE=$$(grep -nE 'if +other\.(display|width|height|position|flex_direction|align_items|justify_content|overflow|transform) +!= +' $$FILES 2>/dev/null || true); \
	BAD_BOOL=$$(grep -nE 'if +other\.(cursor_pointer|flex_grow) *\{' $$FILES 2>/dev/null || true); \
	BAD_VEC=$$(grep -nE 'if +!? *other\.(box_shadows)\.is_empty\(\)' $$FILES 2>/dev/null || true); \
	if [ -n "$$BAD_FIELDS" ] || [ -n "$$BAD_MERGE" ] || [ -n "$$BAD_BOOL" ] || [ -n "$$BAD_VEC" ]; then \
		printf '\033[31m✗ VIOLAÇÃO da regra "toda propriedade CSS é Option<T>"\033[0m\n'; \
		printf '\033[33m  Doc: den_macros/src/types/style.rs (topo) + REVIEW_PROMPT.md regra 6\033[0m\n'; \
		printf '\033[33m  Quebra cascade silenciosamente — :hover não consegue resetar pro default.\033[0m\n\n'; \
		[ -n "$$BAD_FIELDS" ] && printf '\033[31mCampo direto (não Option):\033[0m\n%s\n\n' "$$BAD_FIELDS"; \
		[ -n "$$BAD_MERGE" ]  && printf '\033[31mmerge_from comparando com default (não is_some()):\033[0m\n%s\n\n' "$$BAD_MERGE"; \
		[ -n "$$BAD_BOOL" ]   && printf '\033[31mmerge_from de bool sem Option (não dá pra unsetar):\033[0m\n%s\n\n' "$$BAD_BOOL"; \
		[ -n "$$BAD_VEC" ]    && printf '\033[31mmerge_from de Vec via is_empty() (não distingue None de Some(vec![])):\033[0m\n%s\n\n' "$$BAD_VEC"; \
		exit 1; \
	fi
	@printf '\033[32m✓ Regra Option<T> respeitada.\033[0m\n'

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

c: component  ## Atalho para `component`

component: ## Cria nova página em pages/<nome> (uso: make c [name=header-page])
	@NAME="$(name)"; \
	if [ -z "$$NAME" ]; then \
		printf '\033[36mNome (kebab-case, ex: header-page):\033[0m '; \
		read -r NAME; \
	fi; \
	if [ -z "$$NAME" ]; then printf '\033[31mNome obrigatório.\033[0m\n'; exit 1; fi; \
	if ! printf '%s' "$$NAME" | grep -qE '^[a-z][a-z0-9]*(-[a-z0-9]+)*$$'; then \
		printf '\033[31mUse kebab-case lowercase (ex: header-page).\033[0m\n'; exit 1; \
	fi; \
	SNAKE=$$(printf '%s' "$$NAME" | tr '-' '_'); \
	PASCAL=$$(printf '%s' "$$NAME" | awk -F'-' '{ for (i=1; i<=NF; i++) printf "%s%s", toupper(substr($$i,1,1)), substr($$i,2) }'); \
	DIR="den_app/src/pages/$$SNAKE"; \
	if [ -e "$$DIR" ]; then printf '\033[31mJá existe: %s\033[0m\n' "$$DIR"; exit 1; fi; \
	mkdir -p "$$DIR"; \
	printf '#[allow(clippy::module_inception)]\nmod %s;\npub use %s::%s;\n' "$$SNAKE" "$$SNAKE" "$$PASCAL" > "$$DIR/mod.rs"; \
	{ \
		printf 'use crate::AppRoute;\n'; \
		printf 'use den_layout::{DenRouteState, DenRouter};\n'; \
		printf 'use eframe::egui;\n\n'; \
		printf '#[derive(Default)]\n'; \
		printf 'pub struct %s;\n\n' "$$PASCAL"; \
		printf 'impl %s {\n' "$$PASCAL"; \
		printf '    pub fn render(\n'; \
		printf '        &mut self,\n'; \
		printf '        ui: &mut egui::Ui,\n'; \
		printf '        __den_scale: f32,\n'; \
		printf '        __den_router: &mut DenRouter<AppRoute>,\n'; \
		printf '        __den_route_state: &mut DenRouteState,\n'; \
		printf '    ) {\n'; \
		printf '        den_macros::den_template!("pages/%s/%s", self);\n' "$$SNAKE" "$$SNAKE"; \
		printf '    }\n'; \
		printf '}\n'; \
	} > "$$DIR/$$SNAKE.rs"; \
	printf '<div class="%s-shell">\n    <h1 class="%s-title">%s</h1>\n</div>\n' "$$NAME" "$$NAME" "$$PASCAL" > "$$DIR/$$SNAKE.html"; \
	printf '.%s-shell {\n    padding: 24px;\n}\n\n.%s-title {\n    font-size: 24px;\n    font-weight: 700;\n}\n' "$$NAME" "$$NAME" > "$$DIR/$$SNAKE.scss"; \
	printf 'mod %s;\npub use %s::%s;\n' "$$SNAKE" "$$SNAKE" "$$PASCAL" >> den_app/src/pages/mod.rs; \
	sed -i "s/^use crate::pages::{/use crate::pages::{$$PASCAL, /" den_app/src/routes.rs; \
	BRACE_LINE=$$(grep -n '^}' den_app/src/routes.rs | head -1 | cut -d: -f1); \
	sed -i "$${BRACE_LINE}i\    $$PASCAL," den_app/src/routes.rs; \
	printf '\033[32m✓ Página %s criada em %s\033[0m\n' "$$PASCAL" "$$DIR"; \
	printf '  Registrada em pages/mod.rs e routes.rs.\n'; \
	printf '  Pra navegar via F2 ou outro hotkey: edite den_app/src/main.rs.\n'

help: ## Lista os comandos disponíveis
	@grep -E '^[a-zA-Z_-]+:.*##' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*## "}; {printf "  \033[36m%-10s\033[0m %s\n", $$1, $$2}'
