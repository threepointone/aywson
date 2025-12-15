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
  getComment, // read comment above field
  setComment, // add comment above field
  removeComment, // remove comment above field
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

`modify` uses **replace semantics** — fields not in `changes` are deleted. Comments above deleted fields are also deleted, unless they start with `**`.

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

Remove a field. Comments above the field are also removed, unless they start with `**`.

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

Sort object keys alphabetically while preserving comments with their respective keys.

```ts
sort(`{
  // z comment
  "z": 1,
  // a comment
  "a": 2
}`);
// → '{ "a": 2, "z": 1 }' with comments preserved
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

Get the comment above a field.

```ts
getComment(
  `{
  // this is foo
  "foo": "bar"
}`,
  ["foo"]
);
// → "this is foo"

getComment('{ "foo": "bar" }', ["foo"]);
// → null (no comment)
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

# Get a comment above a field
aywson comment config.json database.host

# Set a comment above a field
aywson comment config.json database.host "production database"

# Remove a comment
aywson uncomment config.json database.host
```

Mutating commands always show a colored diff. Use `--dry-run` (`-n`) to preview without writing.

Path syntax uses dot-notation: `config.database.host` or bracket notation for indices: `items[0].name`
