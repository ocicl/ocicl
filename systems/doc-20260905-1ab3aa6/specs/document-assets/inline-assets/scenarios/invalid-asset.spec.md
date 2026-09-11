---
id: DOC-ASSET-INLINE-001-SC-002
type: scenario
parent: DOC-ASSET-INLINE-001
title: "Reject an invalid declared asset"
status: implemented
change_class: additive
actors: [Library author]
emits: []
consumes: [AssetDeclaration, DocumentationSource]
owners:
  analyst: "@codex"
  developer: "@codex"
  tester: "@codex"
tags: [assets, validation]
created: 2026-09-03
updated: 2026-09-04
---

## § Intent

A library author receives a clear error before publication when a declared asset has no usable source or cannot be placed safely in the output.

## § Domain Rules

- **DR-1**: A missing source file prevents rendering and identifies the asset name and source.
- **DR-2**: A target outside the output root or shared by a different asset prevents rendering and identifies the conflict.

## § Acceptance Criteria

```gherkin
Scenario: Missing source
  Given an author has declared @MISSING.GIF for an absent file
    And documentation contains @MISSING.GIF
  When the author renders documentation
  Then rendering fails with the name @MISSING.GIF and the missing source

Scenario: Unsafe or colliding target
  Given an author has declared an asset with an unsafe or already claimed target path
    And documentation contains its name
  When the author renders documentation
  Then rendering fails with the asset name and target-path reason
```

## § Domain Model Touch

- **Aggregate**: Asset registry (validate)
- **Events emitted**: []
- **Events consumed**: AssetDeclaration, DocumentationSource
- **Invariants**: sources exist; targets are safe and unique

## § Constraints

- **[SEC]** Rendering cannot write an asset outside the output root.

## § Open Questions

None.

## § Decision Log

- **DL-1** (2026-09-03): Validation occurs while rendering so declarations can precede generated files, but an invalid referenced asset cannot silently yield broken output.
