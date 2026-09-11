# Atomic Spec — AI Agent Skill

Скилл для Claude Code, который оркестрирует разработку по методологии Atomic Spec.

## Установка

1. Скопируйте директорию `skills/atomic-spec-orchestrator/` в `~/.claude/skills/atomic-spec-orchestrator/`:

```bash
cp -r skills/atomic-spec-orchestrator/ ~/.claude/skills/atomic-spec-orchestrator/
```

2. Переименуйте файлы:
```
~/.claude/skills/atomic-spec-orchestrator/
├── SKILL.md
├── references/
│   ├── analyst.md
│   ├── developer.md
│   └── tester.md
└── assets/
    └── atom-template.spec.md
```

3. Claude Code автоматически обнаружит скилл и будет использовать его при работе с `*.spec.md` файлами.

## Использование

Скилл активируется автоматически когда вы:
- Просите спроектировать фичу или написать требования
- Декомпозируете задачу на атомы
- Работаете с файлами в директории `/specs/`
- Упоминаете "атом", "спек", "Atomic Spec", "domain rules", "acceptance criteria"

## Роли

Скилл переключается между тремя ролями:
- **Аналитик** — бизнес-требования, DR, AC
- **Разработчик** — Tech Spec, код, API
- **Тестировщик** — Test Plan, тесты, покрытие
