# aywson

𝖆𝖗𝖊 𝖞𝖆 𝖜𝖎𝖓𝖓𝖎𝖓𝖌, 𝖘𝖔𝖓?

![Are ya winning, son?](aywson.png)

Modify JSONC while preserving comments and formatting.

```sh
npm install aywson
```

## Usage

```ts
import {
  parse, // parse JSONC to object
  modify, // replace fields, delete unlisted
  get, // read value at path
  set, // write value at path (with optional comment)
  remove, // delete field at path
  merge, // update fields, keep unlisted
  replace, // alias for modify
  patch, // alias for merge
  rename, // rename a key
  move, // move field to new path
  getComment, // read comment (above or trailing)
  setComment, // add comment above field
  removeComment, // remove comment above field
  getTrailingComment, // read trailing comment
  setTrailingComment, // add trailing comment
  removeTrailingComment, // remove trailing comment
  sort // sort object keys
} from "aywson";
```

## `modify`

Replace fields, delete unlisted. Comments above deleted fields are also deleted, unless they start with `**`.

```ts
import { modify } from "aywson";

modify('{ /* keep this */ "a": 1, "b": 2 }', { a: 10 });
// → '{ /* keep this */ "a": 10 }' — comment preserved, b deleted
```

`modify` uses **replace semantics** — fields not in `changes` are deleted. Comments (both above and trailing) on deleted fields are also deleted, unless they start with `**`.

## `parse`

Parse a JSONC string into a JavaScript value. Unlike `JSON.parse()`, this handles comments and trailing commas.

```ts
import { parse } from "aywson";

parse(`{
  // database config
  "host": "localhost",
  "port": 5432,
}`);
// → { host: "localhost", port: 5432 }

// With TypeScript generics
interface Config {
  host: string;
  port: number;
}
const config = parse<Config>(jsonString);
```

## Path-based Operations

### `get(json, path)`

Get a value at a path.

```ts
get('{ "config": { "enabled": true } }', ["config", "enabled"]);
// → true
```

### `has(json, path)`

Check if a path exists.

```ts
has('{ "foo": "bar" }', ["foo"]); // → true
has('{ "foo": "bar" }', ["baz"]); // → false
```

### `set(json, path, value, comment?)`

Set a value at a path, optionally with a comment.

```ts
set('{ "foo": "bar" }', ["foo"], "baz");
// → '{ "foo": "baz" }'

// With a comment
set('{ "foo": "bar" }', ["foo"], "baz", "this is foo");
// → adds "// this is foo" above the field
```

### `remove(json, path)`

Remove a field. Comments (both above and trailing) are also removed, unless they start with `**`.

```ts
remove(
  `{
  // this is foo
  "foo": "bar",
  "baz": 123
}`,
  ["foo"]
);
// → '{ "baz": 123 }' — comment removed too

remove(
  `{
  "foo": "bar", // trailing comment
  "baz": 123
}`,
  ["foo"]
);
// → '{ "baz": 123 }' — trailing comment removed too
```

## Merge Strategies

### `merge(json, changes)`

Update/add fields, never delete (unless explicit `undefined`).

```ts
merge('{ "a": 1, "b": 2 }', { a: 10 });
// → '{ "a": 10, "b": 2 }' — b preserved
```

### `replace(json, changes)`

Delete fields not in changes (same as `modify`).

```ts
replace('{ "a": 1, "b": 2 }', { a: 10 });
// → '{ "a": 10 }' — b deleted
```

### `patch(json, changes)`

Alias for `merge`. Use `undefined` to delete.

```ts
patch('{ "a": 1, "b": 2 }', { a: undefined });
// → '{ "b": 2 }' — a explicitly deleted
```

## Key Operations

### `rename(json, path, newKey)`

Rename a key while preserving its value.

```ts
rename('{ "oldName": 123 }', ["oldName"], "newName");
// → '{ "newName": 123 }'
```

### `move(json, fromPath, toPath)`

Move a field to a different location.

```ts
move(
  '{ "source": { "value": 123 }, "target": {} }',
  ["source", "value"],
  ["target", "value"]
);
// → '{ "source": {}, "target": { "value": 123 } }'
```

## Sort Operations

### `sort(json, path?, options?)`

Sort object keys alphabetically while preserving comments (both above and trailing) with their respective keys.

```ts
sort(`{
  // z comment
  "z": 1,
  // a comment
  "a": 2
}`);
// → '{ "a": 2, "z": 1 }' with comments preserved

// Trailing comments are also preserved
sort(`{
  "z": 1, // z trailing
  "a": 2 // a trailing
}`);
// → '{ "a": 2 // a trailing, "z": 1 // z trailing }'
```

**Path:** Specify a path to sort only a nested object (defaults to `[]` for root).

```ts
sort(json, ["config", "database"]); // Sort only the database object
```

**Options:**

- `comparator?: (a: string, b: string) => number` — Custom sort function. Defaults to alphabetical.
- `deep?: boolean` — Sort nested objects recursively. Defaults to `true`.

```ts
// Custom sort order (reverse alphabetical)
sort(json, [], { comparator: (a, b) => b.localeCompare(a) });

// Only sort top-level keys (not nested objects)
sort(json, [], { deep: false });

// Sort only a specific nested object, non-recursively
sort(json, ["config"], { deep: false });
```

## Comment Operations

### `setComment(json, path, comment)`

Add or update a comment above a field.

```ts
setComment(
  `{
  "enabled": true
}`,
  ["enabled"],
  "controls the feature"
);
// → adds "// controls the feature" above the field
```

### `removeComment(json, path)`

Remove the comment above a field.

```ts
removeComment(
  `{
  // this will be removed
  "foo": "bar"
}`,
  ["foo"]
);
// → '{ "foo": "bar" }'
```

### `getComment(json, path)`

Get the comment associated with a field. First checks for a comment above, then falls back to a trailing comment.

```ts
getComment(
  `{
  // this is foo
  "foo": "bar"
}`,
  ["foo"]
);
// → "this is foo"

getComment(
  `{
  "foo": "bar" // trailing comment
}`,
  ["foo"]
);
// → "trailing comment"

getComment('{ "foo": "bar" }', ["foo"]);
// → null (no comment)
```

## Trailing Comments

Trailing comments are comments on the same line after a field value:

```jsonc
{
  "foo": "bar", // this is a trailing comment
  "baz": 123 // another trailing comment
}
```

### `getTrailingComment(json, path)`

Get the trailing comment after a field (explicitly, ignoring comments above).

```ts
getTrailingComment(
  `{
  "foo": "bar", // trailing comment
  "baz": 123
}`,
  ["foo"]
);
// → "trailing comment"
```

### `setTrailingComment(json, path, comment)`

Add or update a trailing comment after a field.

```ts
setTrailingComment(
  `{
  "foo": "bar",
  "baz": 123
}`,
  ["foo"],
  "this is foo"
);
// → '{ "foo": "bar" // this is foo, "baz": 123 }'

// Update existing trailing comment
setTrailingComment(
  `{
  "foo": "bar", // old comment
  "baz": 123
}`,
  ["foo"],
  "new comment"
);
// → replaces "old comment" with "new comment"
```

### `removeTrailingComment(json, path)`

Remove the trailing comment after a field.

```ts
removeTrailingComment(
  `{
  "foo": "bar", // this will be removed
  "baz": 123
}`,
  ["foo"]
);
// → '{ "foo": "bar", "baz": 123 }'
```

### Comments Above vs Trailing

You can have both a comment above and a trailing comment:

```ts
const json = `{
  // comment above
  "foo": "bar", // trailing comment
  "baz": 123
}`;

getComment(json, ["foo"]); // → "comment above" (prefers above)
getTrailingComment(json, ["foo"]); // → "trailing comment"

// Set comment above (preserves trailing)
setComment(json, ["foo"], "new above");
// → both comments preserved, above is updated

// Remove comment above (preserves trailing)
removeComment(json, ["foo"]);
// → trailing comment still there
```

## Preserving Comments

When deleting fields, comments are deleted by default. Start a comment with `**` to preserve it:

```ts
remove(
  `{
  // this comment will be deleted
  "config": {}
}`,
  ["config"]
);
// → '{}' — comment deleted with field

remove(
  `{
  // ** this comment will be preserved
  "config": {}
}`,
  ["config"]
);
// → '{ // ** this comment will be preserved }' — comment kept
```

## Object Iteration & Transformation

Even though aywson works on strings, you can still do full object manipulation:

```ts
import { parse, set, remove, merge } from "aywson";

let json = `{
  // Database settings
  "database": {
    "host": "localhost",
    "port": 5432
  },
  // Feature flags
  "features": {
    "darkMode": false,
    "beta": true
  }
}`;

// Parse to iterate/transform
const config = parse<Record<string, unknown>>(json);

// Example: Update all feature flags to false
for (const [key, value] of Object.entries(config.features as object)) {
  if (typeof value === "boolean") {
    json = set(json, ["features", key], false);
  }
}

// Example: Remove fields based on condition
for (const key of Object.keys(config)) {
  if (key.startsWith("_")) {
    json = remove(json, [key]);
  }
}

// Example: Bulk update from transformed object
const updates = Object.fromEntries(
  Object.entries(config.database as object).map(([k, v]) => [
    k,
    typeof v === "string" ? v.toUpperCase() : v
  ])
);
json = merge(json, { database: updates });
```

The key insight: use `parse()` to read and decide _what_ to change, then use `set()`/`remove()`/`merge()` to apply changes while preserving formatting and comments.

## Building JSONC from Scratch

You can build a JSONC file from scratch using `set()` with comments:

```ts
import { set } from "aywson";

let json = "{}";

// Build up the structure with comments
json = set(json, ["database"], {}, "Database configuration");
json = set(json, ["database", "host"], "localhost", "Primary database host");
json = set(json, ["database", "port"], 5432);
json = set(json, ["database", "ssl"], true, "Enable SSL in production");

json = set(json, ["features"], {}, "Feature flags");
json = set(json, ["features", "darkMode"], false);
json = set(
  json,
  ["features", "beta"],
  true,
  "Beta features - use with caution"
);

console.log(json);
```

Output:

```jsonc
{
  // Database configuration
  "database": {
    // Primary database host
    "host": "localhost",
    "port": 5432,
    // Enable SSL in production
    "ssl": true
  },
  // Feature flags
  "features": {
    "darkMode": false,
    // Beta features - use with caution
    "beta": true
  }
}
```

For more complex construction, you can also use `merge()`:

```ts
import { merge, setComment } from "aywson";

let json = "{}";

// Add multiple fields at once
json = merge(json, {
  name: "my-app",
  version: "1.0.0",
  scripts: {
    build: "tsc",
    test: "vitest"
  }
});

// Add comments where needed
json = setComment(json, ["scripts"], "Available npm scripts");
```

## CLI

```bash
# Parse JSONC to JSON (strips comments, handles trailing commas)
aywson parse config.jsonc

# Read a value
aywson get config.json database.host

# Set a value (shows diff and writes to file)
aywson set config.json database.port 5433

# Preview without writing
aywson set --dry-run config.json database.port 5433

# Modify with replace semantics
aywson modify config.json '{"database": {"host": "prod.db.com"}}'

# Merge without deleting existing fields
aywson merge config.json '{"newField": true}'

# Remove a field
aywson remove config.json database.debug

# Sort object keys alphabetically
aywson sort config.json

# Sort only a specific nested object
aywson sort config.json dependencies

# Sort without recursing into nested objects
aywson sort config.json --no-deep

# Get a comment (above, or trailing as fallback)
aywson comment config.json database.host

# Set a comment above a field
aywson comment config.json database.host "production database"

# Remove a comment above a field
aywson uncomment config.json database.host

# Get a trailing comment explicitly
aywson comment --trailing config.json database.port

# Set a trailing comment
aywson comment --trailing config.json database.port "default: 5432"

# Remove a trailing comment
aywson uncomment --trailing config.json database.port
```

Mutating commands always show a colored diff. Use `--dry-run` (`-n`) to preview without writing.

Path syntax uses dot-notation: `config.database.host` or bracket notation for indices: `items[0].name`

## Comparison with `comment-json`

[`comment-json`](https://www.npmjs.com/package/comment-json) is another popular package for working with JSON files that contain comments. Here's how the two packages differ:

### Architecture

| Aspect              | aywson                                | comment-json                           |
| ------------------- | ------------------------------------- | -------------------------------------- |
| **Approach**        | String-in, string-out                 | Parse to object, modify, stringify     |
| **Formatting**      | Preserves original formatting exactly | Re-stringifies (may change formatting) |
| **Mutations**       | Immutable (returns new string)        | Mutable (modifies object in place)     |
| **Comment storage** | Stays in the string                   | Symbol properties on object            |

### Feature Set

| Category              | aywson                                                       | comment-json                         |
| --------------------- | ------------------------------------------------------------ | ------------------------------------ |
| **Core**              | `parse()`                                                    | `parse()`, `stringify()`, `assign()` |
| **Path operations**   | `get()`, `has()`, `set()`, `remove()`                        | Object/array access                  |
| **Bulk updates**      | `merge()`, `modify()`                                        | `assign()`                           |
| **Key manipulation**  | `rename()`, `move()`, `sort()`                               | ❌                                   |
| **Comment API**       | `getComment()`, `setComment()`, `getTrailingComment()`, etc. | Symbol-based access                  |
| **Comment positions** | Above field and trailing (same line)                         | Many (before, after, inline, etc.)   |
| **Extras**            | CLI, `**` prefix to preserve comments                        | `CommentArray` for array operations  |

### When to use aywson

- You need **exact formatting preservation** (whitespace, indentation, trailing commas)
- You want **surgical edits** without re-serializing the entire file
- You prefer **immutable operations** that return new strings
- You need **high-level operations** like rename, move, or sort
- You want **explicit comment manipulation** with a simple API

### When to use comment-json

- You want to work with a **JavaScript object** and make many modifications before writing back
- You're comfortable with **Symbol-based comment access**
- Re-stringifying the entire file is acceptable for your use case

### Example comparison

**comment-json:**

```js
const { parse, stringify, assign } = require("comment-json");

const obj = parse(jsonString);
obj.database.port = 5433;
assign(obj.database, { ssl: true });
const result = stringify(obj, null, 2);
```

**aywson:**

```js
import { set, merge } from "aywson";

let result = set(jsonString, ["database", "port"], 5433);
result = merge(result, { database: { ssl: true } });
// Original formatting preserved exactly
```
