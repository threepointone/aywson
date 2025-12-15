# aywson

## 0.0.13

### Patch Changes

- [`ca0f382`](https://github.com/threepointone/aywson/commit/ca0f3821145a5a8afe8d52453bdf59fa1b48eec4) Thanks [@threepointone](https://github.com/threepointone)! - Update feature comparison table in README

## 0.0.12

### Patch Changes

- [`6ceab82`](https://github.com/threepointone/aywson/commit/6ceab82c567b99d79249941ef98f8cbd78365dff) Thanks [@threepointone](https://github.com/threepointone)! - Add support for trailing comments in JSON operations

  Implements getTrailingComment, setTrailingComment, and removeTrailingComment functions to handle trailing comments after fields. Updates all relevant operations (get, set, remove, modify, merge, sort, etc.) to preserve or manipulate trailing comments as appropriate. Enhances CLI and documentation to support and describe trailing comment features, and adds comprehensive tests for trailing comment behavior.

## 0.0.11

### Patch Changes

- [`3b018a6`](https://github.com/threepointone/aywson/commit/3b018a6943c6326f9942468df65923214f0fabcc) Thanks [@threepointone](https://github.com/threepointone)! - Add comparison section with comment-json to README

  Added a detailed comparison between aywson and the comment-json package, including differences in architecture, feature set, and usage scenarios. Also included example code snippets to illustrate typical usage for both libraries.

## 0.0.10

### Patch Changes

- [`15d9777`](https://github.com/threepointone/aywson/commit/15d9777867cfeba7a4e0fa4d2d73f07c37efc936) Thanks [@threepointone](https://github.com/threepointone)! - Add advanced usage examples to README

## 0.0.9

### Patch Changes

- [#11](https://github.com/threepointone/aywson/pull/11) [`9601074`](https://github.com/threepointone/aywson/commit/96010741c9105fc58afdb1ea725527a6cdfc8888) Thanks [@threepointone](https://github.com/threepointone)! - feature parity with comment-json

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

## 0.0.8

### Patch Changes

- [`3030115`](https://github.com/threepointone/aywson/commit/3030115ab9a4b62e9bb30dcbaaf28797a3c3fbeb) Thanks [@threepointone](https://github.com/threepointone)! - small readme movement

## 0.0.7

### Patch Changes

- [#8](https://github.com/threepointone/aywson/pull/8) [`219f778`](https://github.com/threepointone/aywson/commit/219f77857f7483a98f283d1d95972a5e485fafaa) Thanks [@threepointone](https://github.com/threepointone)! - Add CLI support for aywson with commands and tests

  Introduces a CLI for aywson with commands such as get, set, remove, modify, merge, rename, move, comment, and uncomment, as documented in the updated README. Adds src/cli-lib.ts for CLI logic, src/cli.ts as the entry point, and src/cli.test.ts for unit tests. Updates package.json to include CLI entry, dependencies (chalk, @types/node), and build script. tsconfig.json is updated to include Node.js types.

## 0.0.6

### Patch Changes

- [#6](https://github.com/threepointone/aywson/pull/6) [`f984737`](https://github.com/threepointone/aywson/commit/f984737eac6b3c6db4415c092f9d9ab6683bb64e) Thanks [@threepointone](https://github.com/threepointone)! - Change comment preservation syntax from "fieldName:" to "\*\*"

  Comments above deleted fields are now removed by default. To preserve a
  comment when its field is deleted, start the comment with "\*\*".

## 0.0.5

### Patch Changes

- [#4](https://github.com/threepointone/aywson/pull/4) [`175fe37`](https://github.com/threepointone/aywson/commit/175fe378b44fc4969c12c39ecb5e7ecadf7831ba) Thanks [@threepointone](https://github.com/threepointone)! - fix tagline

## 0.0.4

### Patch Changes

- [`2f528b3`](https://github.com/threepointone/aywson/commit/2f528b37ee296b579b97a92247bf84d29f192947) Thanks [@threepointone](https://github.com/threepointone)! - another

## 0.0.3

### Patch Changes

- [`04c1ae4`](https://github.com/threepointone/aywson/commit/04c1ae4cb5eaee5df18056fc71d58ee557165311) Thanks [@threepointone](https://github.com/threepointone)! - trigger a release

## 0.0.2

### Patch Changes

- [`dd3175f`](https://github.com/threepointone/aywson/commit/dd3175fb724d5def7d2ce1968d64e232cd48d5c0) Thanks [@threepointone](https://github.com/threepointone)! - trigger a release
