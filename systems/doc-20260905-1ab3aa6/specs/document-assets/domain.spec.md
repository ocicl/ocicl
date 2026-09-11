---
id: DOC-ASSET
type: domain
parent: DOC-SYSTEM
title: "Document assets"
status: implemented
change_class: additive
actors: [Library author, Documentation reader]
emits: [AssetRegistered, AssetRendered]
consumes: [AssetDeclaration, DocumentationSource]
owners:
  analyst: "@codex"
  developer: "@codex"
  tester: "@codex"
tags: [assets, images, rendering]
created: 2026-09-03
updated: 2026-09-04
---

## § Intent

A library author declares a local visual asset once and refers to it by its symbolic name in documentation so that readers see the image in every rendered document. A declaration that cannot produce a safe, unambiguous output asset prevents a misleading document from being built.

## § Domain Rules

- **DR-1**: Each asset name identifies exactly one source file and one relative output path within a documentation build. A repeated name with different data is rejected.
- **DR-2**: An asset source file must exist and be readable when documentation is rendered. Otherwise rendering fails and identifies the asset.
- **DR-3**: An output path must remain inside the output directory and must not be claimed by a different asset. An unsafe or colliding path is rejected.
- **DR-4**: Every resolved asset reference renders as an image, not as a textual cross-reference or a hyperlink.
- **DR-5**: Each referenced asset file is copied at most once per output directory, while every use receives a path relative to its page.
- **DR-6**: Existing local Markdown images and non-asset references retain their present behaviour.

## § Acceptance Criteria

```gherkin
Scenario: Declared asset is rendered as an image
  Given an author has declared the asset @DEMO.GIF for an existing local file
    And documentation contains @DEMO.GIF
  When the author renders HTML documentation
  Then the output contains an image for @DEMO.GIF
    And the declared file is present at its output path

Scenario: The same asset is rendered in Markdown
  Given an author has declared the asset @DEMO.GIF for an existing local file
    And documentation contains @DEMO.GIF
  When the author renders Markdown documentation
  Then the output contains Markdown image markup for @DEMO.GIF
    And the declared file is present at its output path

Scenario: Asset use on a nested page
  Given an author has declared an existing local asset
    And a documentation page nested below the output root contains its name
  When the author renders documentation
  Then the image source is relative to the nested page

Scenario: Missing asset file is rejected
  Given an author has declared an asset whose source file is absent
  When the author renders documentation containing its name
  Then rendering fails and identifies the declared asset

Scenario: Colliding output paths are rejected
  Given two assets are declared for the same output path
  When the author renders documentation that uses them
  Then rendering fails and identifies the collision
```

## § Domain Model Touch

- **Aggregate**: Asset registry (new)
- **Entities**: Asset declaration { name, source, output path, description }
- **Events emitted**: AssetRegistered, AssetRendered
- **Events consumed**: AssetDeclaration, DocumentationSource
- **Invariants**: unique name; safe and unique output path; source exists; references become images
- **Read Models affected**: HTML documentation, Markdown documentation

## § Constraints

- **[COMPAT]** The feature is additive and does not change the public behaviour of existing `local-image` users.
- **[IDMP]** Rendering an asset multiple times or rendering the same document again yields one output file per declared target path.
- **[SEC]** Relative target paths containing traversal outside the output directory are rejected.
- **[PERF]** Asset lookup is constant-time with respect to the number of declarations.

## § Open Questions

None.

## § Decision Log

- **DL-1** (2026-09-03): A bare asset symbol represents the image itself in all supported formats, per requester confirmation.
- **DL-2** (2026-09-03): The declaration's relative source path is also the default output path; an explicit output-path option may be added only if needed for collision resolution.
- **DL-3** (2026-09-03): Copying occurs in a format-independent build stage so Markdown output does not depend on the HTML emitter's side effects.
