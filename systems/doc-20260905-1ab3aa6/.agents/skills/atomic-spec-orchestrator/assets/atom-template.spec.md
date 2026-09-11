---
id: {DOMAIN}-{MODULE}-{NNN}
type: use-case                    # system | domain | aggregate | use-case | scenario
parent: {PARENT_ID}
title: "{Title}"
status: draft                     # draft → review → ready → implemented → deprecated
change_class: additive            # breaking | additive | compatible
actors: []
emits: []
consumes: []
owners:
  analyst: ""
  developer: ""
  tester: ""
tags: []
created: {DATE}
updated: {DATE}
---

## § Intent

{Актор} {действие} чтобы {бизнес-цель}.
После успеха: {что изменилось в системе}.
После неудачи: {что происходит}.

## § Domain Rules

- **DR-1**: {Субъект} {условие} {ограничение}. {Что происходит при нарушении}.

## § Acceptance Criteria

```gherkin
Scenario: {Happy path} 
  Given {предусловие}
  When {действие актора}
  Then {ожидаемый результат}

Scenario: {Negative — DR-1 violation}
  Given {предусловие}
  When {действие, нарушающее DR-1}
  Then {результат нарушения}
```

## § Domain Model Touch

- **Aggregate**: {Name} ({create|update|delete})
- **Events emitted**: {EventName} { field1, field2 }
- **Events consumed**: —
- **Invariants**: {описание}
- **Read Models affected**: {список}

## § Constraints

- **[PERF]** {требование к производительности}
- **[SEC]** {требование к безопасности}
- **[IDMP]** {требование к идемпотентности}
- **[A11Y]** {требование к доступности}

## § Open Questions

- **OQ-1** [{blocking|non-blocking}]: {вопрос}

## § Decision Log

- **DL-1** ({DATE}): {решение}. Причина: {обоснование}.

## § Tech Spec

<!-- Заполняется Разработчиком -->

### API

### Data Schema

### Architecture

## § Test Plan

<!-- Заполняется Тестировщиком -->

### Coverage Matrix

| Источник | ID | Описание | Тест-кейс | Статус |
|----------|-----|----------|-----------|--------|

### Test Cases

## § Implementation Notes

<!-- Заполняется Разработчиком -->

### Trade-offs

### Dependencies

### Known Limitations

### File Map
