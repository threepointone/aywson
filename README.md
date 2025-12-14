# aywson

𝖆𝖗𝖊 𝖞𝖆 𝖜𝖎𝖓𝖓𝖎𝖓𝖌, 𝖘𝖔𝖓?

![Are ya winning, son?](aywson.png)

Modify JSONC while preserving comments and formatting.

```sh
npm install aywson
```

## Usage

```ts
import { modify } from "aywson";

modify('{ /* keep this */ "a": 1, "b": 2 }', { a: 10 });
// → '{ /* keep this */ "a": 10 }' — comment preserved, b deleted
```

`modify` uses **replace semantics** — fields not in `changes` are deleted. Comments above deleted fields are also deleted, unless they start with `**`.

## All Exports

```ts
import {
  modify,
  get,
  set,
  remove,
  merge,
  replace,
  patch,
  rename,
  move,
  setComment,
  removeComment
} from "aywson";
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

### `set(json, path, value)`

Set a value at a path.

```ts
set('{ "foo": "bar" }', ["foo"], "baz");
// → '{ "foo": "baz" }'
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
