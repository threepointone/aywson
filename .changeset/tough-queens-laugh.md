---
"aywson": patch
---

Add CLI support for aywson with commands and tests

Introduces a CLI for aywson with commands such as get, set, remove, modify, merge, rename, move, comment, and uncomment, as documented in the updated README. Adds src/cli-lib.ts for CLI logic, src/cli.ts as the entry point, and src/cli.test.ts for unit tests. Updates package.json to include CLI entry, dependencies (chalk, @types/node), and build script. tsconfig.json is updated to include Node.js types.
