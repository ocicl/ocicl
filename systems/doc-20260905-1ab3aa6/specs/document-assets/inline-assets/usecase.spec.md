---
id: DOC-ASSET-INLINE-001
type: use-case
parent: DOC-ASSET
title: "Render declared image inline"
status: implemented
change_class: breaking
actors: [Library author, Documentation reader]
emits: [AssetRegistered, AssetRendered]
consumes: [AssetDeclaration, DocumentationSource]
owners:
  analyst: "@codex"
  developer: "@codex"
  tester: "@codex"
tags: [assets, inline-image, html, markdown]
created: 2026-09-03
updated: 2026-09-05
---

## § Intent

A library author declares an image once and places its name in documentation so a documentation reader sees that image in the rendered format and requested size. If the declaration cannot be resolved safely, rendering stops instead of emitting a broken image.

## § Domain Rules

- **DR-1**: A declared asset name resolves to exactly one existing source file. A missing source fails rendering and identifies the name.
- **DR-2**: A declared asset name in documentation becomes an image in each supported output format. It must not be rendered as text or a navigation link.
- **DR-3**: The image file is copied to one safe, unique path below the output root. A path collision or traversal attempt fails rendering.
- **DR-4**: Image source paths are relative to the page that contains the image, including nested pages.
- **DR-5**: Repeated occurrences of the same asset do not duplicate its output file.
- **DR-6**: A declared asset name in prose is recognized even when implicit uppercase-code formatting is disabled.
- **DR-7**: A declared asset symbol may be used directly as a DEFSECTION entry and becomes an image.
- **DR-8**: `DEFIMAGE` is the sole declaration API; `DEFASSET` is unavailable.
- **DR-9**: `:WIDTH` and `:HEIGHT` are positive pixel counts. Each supplied value is emitted as an image attribute; an omitted value is not emitted.
- **DR-10**: Markdown without dimensions uses Markdown image syntax. Markdown with dimensions uses an HTML `img` element so both dimensions are preserved.

## § Acceptance Criteria

```gherkin
Scenario: Render a declared image in HTML (→ DR-1, DR-2, DR-3)
  Given an author has declared an existing asset named @DEMO.GIF
    And a documentation page contains @DEMO.GIF
  When the author renders HTML documentation
  Then the page contains an image for @DEMO.GIF
    And its source file exists below the output root

Scenario: Render a declared image in Markdown (→ DR-2, DR-3)
  Given an author has declared an existing asset named @DEMO.GIF
    And a documentation page contains @DEMO.GIF
  When the author renders Markdown documentation
  Then the page contains image markup for @DEMO.GIF
    And its source file exists below the output root

Scenario: Render the image from a nested page (→ DR-4)
  Given an author has declared an existing asset named @DEMO.GIF
    And a page two directories below the output root contains @DEMO.GIF
  When the author renders documentation
  Then the emitted image source reaches the copied file using a relative path

Scenario: Render a repeated image (→ DR-5)
  Given an author has declared an existing asset named @DEMO.GIF
    And two documentation pages contain @DEMO.GIF
  When the author renders documentation
  Then one output file is created for @DEMO.GIF

Scenario: Render with implicit uppercase-code formatting disabled (→ DR-2, DR-6)
  Given an author has declared an existing asset named @DEMO.GIF
    And a documentation page contains @DEMO.GIF
    And implicit uppercase-code formatting is disabled
  When the author renders documentation
  Then the page contains an image for @DEMO.GIF

Scenario: Render an explicit asset entry (→ DR-2, DR-7)
  Given an author has declared an existing asset named @DEMO.GIF
    And a documentation section contains @DEMO.GIF as a direct entry
  When the author renders documentation
  Then the page contains an image for @DEMO.GIF

Scenario: Scale a declared image (→ DR-8, DR-9, DR-10)
  Given an author has declared @DEMO.GIF with only :WIDTH 240
  When the author renders HTML or Markdown documentation
  Then the output contains width 240 and no height attribute
    And Markdown contains an HTML img element

Scenario: Reject an unusable declaration (→ DR-1, DR-3)
  Given an author has declared an asset with an absent source or unsafe output path
  When the author renders documentation containing its name
  Then rendering fails with an error that identifies the asset and reason
```

## § Domain Model Touch

- **Aggregate**: Asset registry (create and query)
- **Events emitted**: AssetRegistered, AssetRendered
- **Events consumed**: AssetDeclaration, DocumentationSource
- **Invariants**: declared source exists; target is safe and unique; rendered reference is an image

## § Constraints

- **[COMPAT]** Existing image syntax and regular documentation references remain unchanged.
- **[IDMP]** Copying the same registered asset is idempotent per output directory.
- **[SEC]** No asset declaration can write outside the selected output root.
- **[PERF]** Resolution of a declared asset does not scan the full declaration set.

## § Open Questions

None.

## § Decision Log

- **DL-1** (2026-09-03): The source-path-derived output path is the v1 default, avoiding an additional public option until a concrete need arises.
- **DL-2** (2026-09-03): Alt text defaults to the asset symbol's name and can be expanded in a follow-up feature if richer metadata is required.

## § Tech Spec

### Public API

`40ants-doc-full/assets:defimage` registers a symbol, a local pathname
designator, and optional `:target-filename`, `:description`, `:width`, and
`:height` metadata. Width and height are positive pixels. The default target is
the source's relative pathname. Only explicitly supplied dimensions are emitted.

### Rendering pipeline

1. The existing XREF pass recognizes dotted asset names such as `@DEMO.GIF`.
2. When implicit uppercase-code formatting is disabled, the XREF pass extracts
   only registered asset names.
3. `replace-images` changes an XREF with a registered symbol into a
   `local-image` node before normal XREF replacement.
4. A registered symbol used directly as a DEFSECTION entry is transformed into
   its asset and then into a `local-image` by the builder.
5. The builder copies all `local-image` nodes once per target before emitting
   pages, independent of the selected document format.
6. HTML emits an `img` element with supplied dimensions. Markdown emits Markdown
   image syntax without dimensions and an HTML `img` element with dimensions,
   all calculated relative to the page being rendered.

### Validation

Asset resolution checks source existence, rejects absolute or parent-directory
target paths, and rejects targets registered by another asset name.

## § Implementation Notes

### File Map

- `full/assets.lisp` — image registry, `defimage`, validation, and XREF replacement.
- `full/commondoc/image.lisp` — shared pre-render copying and HTML/Markdown
  image emission.
- `full/builder.lisp` — invokes asset replacement and the common copying step.
- `full/commondoc/xref.lisp` — recognizes dotted symbol names.
- `test/assets.lisp` — HTML, Markdown, missing source, unsafe target, and
  collision coverage, plus disabled implicit-code and direct-entry regression
  coverage.

### Trade-offs

The registry is process-local, matching existing load-time documentation
definitions. Image names are symbol identities, so declarations are naturally
scoped by package.

### Known Limitations

Image dimensions are not read from source files; browsers preserve aspect ratio
when only one HTML dimension is supplied.

## § Test Plan

| Test case | Requirement | Verification |
| --- | --- | --- |
| TC-001 | HTML rendering; DR-1, DR-2, DR-3 | Render an asset page, assert the copied file and its `img` source. |
| TC-002 | Markdown rendering; DR-2, DR-3, DR-4 | Render a nested Markdown page, assert the copied file and `../assets/...` image source. |
| TC-003 | Missing source; DR-1 | Render a page using an absent source and assert an error. |
| TC-004 | Unsafe target; DR-3, [SEC] | Render a page using `../` as a target and assert an error. |
| TC-005 | Target collision; DR-3 | Render a page using one of two assets registered for the same target and assert an error. |
| TC-006 | Repeated asset; DR-5, [IDMP] | Render two occurrences and assert two image nodes sharing one target file. |
| TC-007 | Regression | Run the complete `40ants-doc-test` ASDF suite. |
| TC-008 | Static analysis | Run `40ants-linter` for core, full, and test systems with imports checking. |
| TC-009 | DR-6 | Disable implicit uppercase-code formatting and assert a bare asset name is emitted as an image. |
| TC-010 | DR-7 | Use an asset symbol directly in DEFSECTION and assert Markdown image output. |
| TC-011 | DR-9 | Declare only `:WIDTH` and assert HTML contains no height attribute. |
| TC-012 | DR-9 | Declare only `:HEIGHT` and assert HTML contains no width attribute. |
| TC-013 | DR-10 | Declare both dimensions and assert Markdown emits a sized HTML `img`. |

## § QA Review

- All acceptance criteria are covered by TC-001 through TC-013.
- The negative scenarios cover missing input, unsafe traversal, and a duplicate
  output target.
- IDMP is implemented through a target-keyed copy set; TC-006 verifies the
  repeated-use case, while TC-001 and TC-002 exercise both output formats.
