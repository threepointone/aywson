---
"aywson": patch
---

feature parity with comment-json

### New Features

- **`sort(json, path?, options?)`** — Sort object keys alphabetically, preserving comments
- **`parse<T>(json)`** — Parse JSONC to JavaScript object
- **`getComment(json, path)`** — Get comment above a field
- **`set()` with optional comment** — `set(json, path, value, comment?)`
- **Trailing comma preservation** — In all operations including `sort()`

### New CLI Commands

- `aywson parse <file>` — Parse JSONC to JSON
- `aywson sort <file> [path]` — Sort keys (with `--no-deep` option)
- `aywson comment <file> <path>` — Get comment (without text arg)

### New Documentation

- Object iteration & transformation patterns
- Building JSONC from scratch
