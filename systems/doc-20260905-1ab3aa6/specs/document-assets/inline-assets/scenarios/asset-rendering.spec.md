---
id: DOC-ASSET-INLINE-001-SC-001
type: scenario
parent: DOC-ASSET-INLINE-001
title: "Render a declared asset as an inline image"
status: implemented
change_class: additive
actors: [Library author, Documentation reader]
emits: [AssetRendered]
consumes: [AssetDeclaration, DocumentationSource]
owners:
  analyst: "@codex"
  developer: "@codex"
  tester: "@codex"
tags: [assets, regression]
created: 2026-09-03
updated: 2026-09-04
---

## § Intent

A library author uses a declared asset name in documentation so a reader sees the declared image in the resulting HTML or Markdown file.

## § Domain Rules

- **DR-1**: A resolvable asset name is represented by an image in the selected output format.
- **DR-2**: The image target exists in the output directory and is addressed relative to the page.
- **DR-3**: A declared asset is recognized when implicit uppercase-code formatting is disabled and when used as a direct DEFSECTION entry.

## § Acceptance Criteria

```gherkin
Scenario: Output contains a declared image
  Given an author has declared @DEMO.GIF for an existing local image
    And a documentation page contains @DEMO.GIF
  When the author renders the page as HTML or Markdown
  Then the page contains image syntax for @DEMO.GIF
    And the referenced image file is present in the output directory

Scenario: Explicit or non-implicit asset inclusion
  Given an author has declared @DEMO.GIF for an existing local image
  When the author disables implicit uppercase-code formatting or uses @DEMO.GIF as a direct DEFSECTION entry
  Then the page contains image syntax for @DEMO.GIF
```

## § Domain Model Touch

- **Aggregate**: Asset registry (query)
- **Events emitted**: AssetRendered
- **Events consumed**: AssetDeclaration, DocumentationSource
- **Invariants**: image representation and copied target agree

## § Constraints

- **[COMPAT]** Standard images and standard references are unaffected.

## § Open Questions

None.

## § Decision Log

- **DL-1** (2026-09-03): HTML and Markdown are both first-class outputs for this scenario.
