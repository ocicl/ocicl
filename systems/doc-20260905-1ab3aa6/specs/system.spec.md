---
id: DOC-SYSTEM
type: system
title: "40ants-doc"
status: ready
change_class: compatible
actors: [Library author, Documentation reader]
emits: [DocumentationRendered]
consumes: [DocumentationSource]
owners:
  analyst: "@codex"
  developer: "@codex"
  tester: "@codex"
tags: [documentation, common-lisp]
created: 2026-09-03
updated: 2026-09-03
---

## § Intent

40ants-doc lets library authors keep documentation near their source and render it into publishable formats for documentation readers.

## § Domain Rules

- **DR-1**: A documented entity has a stable symbolic identity and can be resolved while rendering. An unresolved entity is reported to the author.
- **DR-2**: A rendered document must not reference a local file that was not included in its output. A missing local file fails the build.

## § Acceptance Criteria

```gherkin
Scenario: Render documentation
  Given an author has documented a library
  When the author renders its documentation in a supported format
  Then the reader receives a document whose local references resolve within the output
```

## § Domain Model Touch

- **Aggregate**: Documentation source (extend)
- **Events emitted**: DocumentationRendered
- **Events consumed**: DocumentationSource
- **Invariants**: local references resolve to output artifacts

## § Constraints

- **[COMPAT]** Existing documented entities and local Markdown images retain their current rendering behaviour.

## § Open Questions

None.

## § Decision Log

- **DL-1** (2026-09-03): Declarative local assets belong to the documentation-rendering domain because their value is realised only during output generation.
