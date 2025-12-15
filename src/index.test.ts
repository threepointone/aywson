import { describe, expect, it } from "vitest";
import {
  get,
  getComment,
  getTrailingComment,
  has,
  merge,
  modify,
  move,
  parse,
  patch,
  remove,
  removeComment,
  removeTrailingComment,
  rename,
  replace,
  set,
  setComment,
  setTrailingComment,
  sort
} from "./index";

describe("modify", () => {
  it("should modify a value while preserving comments", () => {
    const json = '{/* comment */ "foo": "bar" }';
    const result = modify(json, { foo: "baz" });
    expect(result).toBe('{/* comment */ "foo": "baz" }');
  });

  it("should modify multiple keys", () => {
    const json = '{ "foo": "bar", "baz": 123 }';
    const result = modify(json, { foo: "updated", baz: 456 });
    expect(result).toBe('{ "foo": "updated", "baz": 456 }');
  });

  it("should add new keys and keep existing ones when specified", () => {
    const json = '{ "foo": "bar" }';
    const result = modify(json, { foo: "bar", baz: "new" });
    expect(result).toBe('{ "foo": "bar","baz": "new" }');
  });

  it("should delete fields not in changes (replace semantics)", () => {
    const json = '{ "foo": "bar", "baz": 123 }';
    const result = modify(json, { baz: 456 });
    expect(result).toBe('{ "baz": 456 }');
  });

  it("should preserve inline comments", () => {
    const json = `{
  // This is a comment
  "name": "old"
}`;
    const result = modify(json, { name: "new" });
    expect(result).toBe(`{
  // This is a comment
  "name": "new"
}`);
  });

  it("should preserve block comments", () => {
    const json = `{
  /* block comment */
  "value": 100
}`;
    const result = modify(json, { value: 200 });
    expect(result).toBe(`{
  /* block comment */
  "value": 200
}`);
  });

  it("should preserve top-level comments", () => {
    const json = `// top level comment
{
  "a": 123,
  "b": 456
}`;
    const result = modify(json, { a: 999 });
    expect(result).toBe(`// top level comment
{
  "a": 999
}`);
  });

  it("should preserve top-level block comments", () => {
    const json = `/* top level block comment */
{
  "a": 123
}`;
    const result = modify(json, { a: 999 });
    expect(result).toBe(`/* top level block comment */
{
  "a": 999
}`);
  });

  it("should preserve multiple top-level comments", () => {
    const json = `// comment 1
// comment 2
{
  "a": 123,
  "b": 456
}`;
    const result = modify(json, { b: 456 });
    expect(result).toBe(`// comment 1
// comment 2
{
  "b": 456
}`);
  });

  it("should handle nested objects by updating individual values", () => {
    const json = '{ "nested": { "a": 1 } }';
    const result = modify(json, { nested: { a: 2, b: 3 } });
    expect(result).toBe('{ "nested": { "a": 2,"b": 3 } }');
  });

  it("should preserve comments in nested objects", () => {
    const json = `{
  "config": {
    // Important setting
    "enabled": true,
    "count": 5
  }
}`;
    const result = modify(json, { config: { enabled: false, count: 10 } });
    expect(result).toBe(`{
  "config": {
    // Important setting
    "enabled": false,
    "count": 10
  }
}`);
  });

  it("should preserve comments in deeply nested objects", () => {
    const json = `{
  "database": {
    // Database configuration
    "connection": {
      /* Primary host settings */
      "host": "localhost",
      "port": 5432,
      "options": {
        // Enable SSL for security
        "ssl": false,
        "timeout": 30
      }
    },
    // Pool settings
    "pool": {
      "min": 2,
      "max": 10
    }
  }
}`;
    const result = modify(json, {
      database: {
        connection: {
          host: "production.db.example.com",
          port: 5433,
          options: {
            ssl: true,
            timeout: 60
          }
        },
        pool: {
          min: 5,
          max: 50
        }
      }
    });
    expect(result).toBe(`{
  "database": {
    // Database configuration
    "connection": {
      /* Primary host settings */
      "host": "production.db.example.com",
      "port": 5433,
      "options": {
        // Enable SSL for security
        "ssl": true,
        "timeout": 60
      }
    },
    // Pool settings
    "pool": {
      "min": 5,
      "max": 50
    }
  }
}`);
  });

  describe("deletions", () => {
    it("should delete fields not mentioned in changes", () => {
      const json = `{
  "foo": "bar",
  "baz": 123
}`;
      const result = modify(json, { baz: 123 });
      expect(result).toBe(`{
  "baz": 123
}`);
    });

    it("should delete a field and its comment (line comment)", () => {
      const json = `{
  // this is the abc field
  "abc": 123,
  "def": 456
}`;
      const result = modify(json, { def: 456 });
      expect(result).toBe(`{
  "def": 456
}`);
    });

    it("should delete a field and its comment (block comment)", () => {
      const json = `{
  /* this is the abc field */
  "abc": 123,
  "def": 456
}`;
      const result = modify(json, { def: 456 });
      expect(result).toBe(`{
  "def": 456
}`);
    });

    it("should preserve comment starting with ** when deleting field", () => {
      const json = `{
  // ** important note about abc
  "abc": 123,
  "def": 456
}`;
      const result = modify(json, { def: 456 });
      expect(result).toBe(`{
  // ** important note about abc
  "def": 456
}`);
    });

    it("should preserve block comment starting with ** when deleting field", () => {
      const json = `{
  /* ** important note about abc */
  "abc": 123,
  "def": 456
}`;
      const result = modify(json, { def: 456 });
      expect(result).toBe(`{
  /* ** important note about abc */
  "def": 456
}`);
    });

    it("should delete nested field and its comment", () => {
      const json = `{
  "config": {
    // controls whether feature is on
    "enabled": true,
    "count": 5
  }
}`;
      const result = modify(json, { config: { count: 5 } });
      expect(result).toBe(`{
  "config": {
    "count": 5
  }
}`);
    });

    it("should delete entire nested object when not mentioned", () => {
      const json = `{
  "keep": true,
  "remove": {
    "a": 1,
    "b": 2
  }
}`;
      const result = modify(json, { keep: true });
      expect(result).toBe(`{
  "keep": true
}`);
    });
  });
});

describe("unicode escape sequences", () => {
  // These tests ensure we don't have the issue from:
  // https://github.com/kaelzhang/node-comment-json/issues/29
  // where unicode escape sequences are converted to literal characters
  // when parse+stringify is used. aywson preserves them because it
  // works directly on the string and only modifies what's necessary.

  it("should preserve unicode escape sequences when modifying other fields", () => {
    const json = `{
  "settings": {
    "additionalUnicodeChars": [
      "\\u0008",
      "\\u3000",
      "\\u00A0"
    ],
    "enabled": true
  }
}`;
    // Only change "enabled", leave the array untouched
    const result = set(json, ["settings", "enabled"], false);
    // The unicode escape sequences should be preserved exactly
    expect(result).toContain('"\\u0008"');
    expect(result).toContain('"\\u3000"');
    expect(result).toContain('"\\u00A0"');
    expect(result).toContain('"enabled": false');
  });

  it("should preserve unicode escapes when using set on different path", () => {
    const json = '{ "unicode": "\\u3000", "other": "value" }';
    const result = set(json, ["other"], "changed");
    expect(result).toBe('{ "unicode": "\\u3000", "other": "changed" }');
  });

  it("should preserve unicode escapes when using merge", () => {
    const json = '{ "unicode": "\\u3000", "count": 1 }';
    const result = merge(json, { count: 2 });
    expect(result).toBe('{ "unicode": "\\u3000", "count": 2 }');
  });

  it("should preserve unicode escapes when removing other fields", () => {
    const json = `{
  "unicode": "\\u3000",
  "toRemove": "value"
}`;
    const result = remove(json, ["toRemove"]);
    expect(result).toContain('"\\u3000"');
  });

  it("should preserve unicode escapes in arrays when modifying sibling fields", () => {
    // Simulating a VS Code workspace config like in the original issue
    const json = `{
  "folders": [{ "path": "." }],
  "settings": {
    "highlight-bad-chars.additionalUnicodeChars": [
      "\\u0008",
      "\\u3000"
    ],
    "editor.renderWhitespace": "all"
  }
}`;
    const result = set(json, ["settings", "editor.renderWhitespace"], "none");
    expect(result).toContain('"\\u0008"');
    expect(result).toContain('"\\u3000"');
    expect(result).toContain('"editor.renderWhitespace": "none"');
  });

  it("should properly escape control characters when setting values", () => {
    // This test ensures we don't have the issue from:
    // https://github.com/kaelzhang/node-comment-json/issues/36
    // Control characters (0x00-0x1F) must be escaped as \uXXXX per JSON spec
    const json = '{ "test": "" }';

    // Vertical tab (0x0B)
    const result1 = set(json, ["test"], "\x0B");
    expect(result1).toBe('{ "test": "\\u000b" }');

    // Multiple control characters
    const result2 = set(json, ["test"], "\x00\x01\x1F");
    expect(result2).toBe('{ "test": "\\u0000\\u0001\\u001f" }');

    // Backspace (0x08) - also a control character
    const result3 = set(json, ["test"], "\x08");
    expect(result3).toBe('{ "test": "\\b" }');

    // Tab and newline have standard escapes
    const result4 = set(json, ["test"], "\t\n");
    expect(result4).toBe('{ "test": "\\t\\n" }');
  });
});

describe("parse", () => {
  it("should parse simple JSON", () => {
    const result = parse('{ "foo": "bar" }');
    expect(result).toEqual({ foo: "bar" });
  });

  it("should parse JSON with line comments", () => {
    const result = parse(`{
      // this is a comment
      "foo": "bar"
    }`);
    expect(result).toEqual({ foo: "bar" });
  });

  it("should parse JSON with block comments", () => {
    const result = parse(`{
      /* block comment */
      "foo": "bar"
    }`);
    expect(result).toEqual({ foo: "bar" });
  });

  it("should parse JSON with trailing commas", () => {
    const result = parse(`{
      "foo": "bar",
      "baz": 123,
    }`);
    expect(result).toEqual({ foo: "bar", baz: 123 });
  });

  it("should parse arrays", () => {
    const result = parse("[1, 2, 3]");
    expect(result).toEqual([1, 2, 3]);
  });

  it("should parse arrays with trailing commas", () => {
    const result = parse("[1, 2, 3,]");
    expect(result).toEqual([1, 2, 3]);
  });

  it("should parse nested objects", () => {
    const result = parse('{ "config": { "enabled": true } }');
    expect(result).toEqual({ config: { enabled: true } });
  });

  it("should support generic type parameter", () => {
    interface Config {
      name: string;
      count: number;
    }
    const result = parse<Config>('{ "name": "test", "count": 42 }');
    expect(result.name).toBe("test");
    expect(result.count).toBe(42);
  });

  it("should parse primitive values", () => {
    expect(parse("123")).toBe(123);
    expect(parse('"hello"')).toBe("hello");
    expect(parse("true")).toBe(true);
    expect(parse("null")).toBe(null);
  });
});

describe("get", () => {
  it("should get a value at a path", () => {
    const json = '{ "foo": "bar", "baz": 123 }';
    expect(get(json, ["foo"])).toBe("bar");
    expect(get(json, ["baz"])).toBe(123);
  });

  it("should get nested values", () => {
    const json = '{ "config": { "enabled": true, "count": 5 } }';
    expect(get(json, ["config", "enabled"])).toBe(true);
    expect(get(json, ["config", "count"])).toBe(5);
    expect(get(json, ["config"])).toEqual({ enabled: true, count: 5 });
  });

  it("should return undefined for non-existent paths", () => {
    const json = '{ "foo": "bar" }';
    expect(get(json, ["nonexistent"])).toBeUndefined();
    expect(get(json, ["foo", "bar"])).toBeUndefined();
  });

  it("should get array values", () => {
    const json = '{ "items": [1, 2, 3] }';
    expect(get(json, ["items"])).toEqual([1, 2, 3]);
    expect(get(json, ["items", 0])).toBe(1);
    expect(get(json, ["items", 2])).toBe(3);
  });
});

describe("has", () => {
  it("should return true for existing paths", () => {
    const json = '{ "foo": "bar", "baz": null }';
    expect(has(json, ["foo"])).toBe(true);
    expect(has(json, ["baz"])).toBe(true);
  });

  it("should return false for non-existent paths", () => {
    const json = '{ "foo": "bar" }';
    expect(has(json, ["nonexistent"])).toBe(false);
    expect(has(json, ["foo", "bar"])).toBe(false);
  });

  it("should work with nested paths", () => {
    const json = '{ "config": { "enabled": true } }';
    expect(has(json, ["config"])).toBe(true);
    expect(has(json, ["config", "enabled"])).toBe(true);
    expect(has(json, ["config", "disabled"])).toBe(false);
  });
});

describe("set", () => {
  it("should set a value at a path", () => {
    const json = '{ "foo": "bar" }';
    const result = set(json, ["foo"], "baz");
    expect(result).toBe('{ "foo": "baz" }');
  });

  it("should add a new key", () => {
    const json = '{ "foo": "bar" }';
    const result = set(json, ["baz"], 123);
    expect(result).toBe('{ "foo": "bar","baz": 123 }');
  });

  it("should set nested values", () => {
    const json = '{ "config": { "enabled": true } }';
    const result = set(json, ["config", "enabled"], false);
    expect(result).toBe('{ "config": { "enabled": false } }');
  });

  it("should create nested paths", () => {
    const json = '{ "foo": "bar" }';
    const result = set(json, ["config", "enabled"], true);
    expect(result).toBe('{ "foo": "bar","config": {"enabled":true} }');
  });

  it("should set a value with a comment", () => {
    const json = `{
  "foo": "bar"
}`;
    const result = set(json, ["foo"], "baz", "this is foo");
    expect(result).toContain("// this is foo");
    expect(result).toContain('"foo": "baz"');
  });

  it("should replace existing comment when setting with comment", () => {
    const json = `{
  // old comment
  "foo": "bar"
}`;
    const result = set(json, ["foo"], "baz", "new comment");
    expect(result).toContain("// new comment");
    expect(result).not.toContain("old comment");
    expect(result).toContain('"foo": "baz"');
  });

  it("should set value without comment when comment is undefined", () => {
    const json = `{
  // existing comment
  "foo": "bar"
}`;
    const result = set(json, ["foo"], "baz");
    expect(result).toContain("// existing comment");
    expect(result).toContain('"foo": "baz"');
  });
});

describe("getComment", () => {
  it("should get a line comment above a field", () => {
    const json = `{
  // this is foo
  "foo": "bar"
}`;
    expect(getComment(json, ["foo"])).toBe("this is foo");
  });

  it("should get a block comment above a field", () => {
    const json = `{
  /* block comment */
  "foo": "bar"
}`;
    expect(getComment(json, ["foo"])).toBe("block comment");
  });

  it("should return null if no comment exists", () => {
    const json = `{
  "foo": "bar"
}`;
    expect(getComment(json, ["foo"])).toBeNull();
  });

  it("should return null for non-existent path", () => {
    const json = '{ "foo": "bar" }';
    expect(getComment(json, ["nonexistent"])).toBeNull();
  });

  it("should get comment from nested field", () => {
    const json = `{
  "config": {
    // nested comment
    "enabled": true
  }
}`;
    expect(getComment(json, ["config", "enabled"])).toBe("nested comment");
  });

  it("should preserve ** prefix in comment content", () => {
    const json = `{
  // ** important note
  "foo": "bar"
}`;
    expect(getComment(json, ["foo"])).toBe("** important note");
  });
});

describe("remove", () => {
  it("should remove a field", () => {
    const json = `{
  "foo": "bar",
  "baz": 123
}`;
    const result = remove(json, ["foo"]);
    expect(result).toBe(`{
  "baz": 123
}`);
  });

  it("should remove a field with its comment", () => {
    const json = `{
  // this is foo
  "foo": "bar",
  "baz": 123
}`;
    const result = remove(json, ["foo"]);
    expect(result).toBe(`{
  "baz": 123
}`);
  });

  it("should preserve comments starting with **", () => {
    const json = `{
  // ** important note
  "foo": "bar",
  "baz": 123
}`;
    const result = remove(json, ["foo"]);
    expect(result).toBe(`{
  // ** important note
  "baz": 123
}`);
  });

  it("should remove nested fields", () => {
    const json = `{
  "config": {
    "enabled": true,
    "count": 5
  }
}`;
    const result = remove(json, ["config", "enabled"]);
    expect(result).toBe(`{
  "config": {
    "count": 5
  }
}`);
  });
});

describe("merge", () => {
  it("should update values without deleting", () => {
    const json = '{ "foo": "bar", "baz": 123 }';
    const result = merge(json, { foo: "updated" });
    expect(result).toBe('{ "foo": "updated", "baz": 123 }');
  });

  it("should add new keys", () => {
    const json = '{ "foo": "bar" }';
    const result = merge(json, { baz: 123 });
    expect(result).toBe('{ "foo": "bar","baz": 123 }');
  });

  it("should handle nested merges", () => {
    const json = '{ "config": { "a": 1, "b": 2 } }';
    const result = merge(json, { config: { a: 10 } });
    expect(result).toBe('{ "config": { "a": 10, "b": 2 } }');
  });

  it("should delete with explicit undefined", () => {
    const json = '{ "foo": "bar", "baz": 123 }';
    const result = merge(json, { foo: undefined });
    expect(result).toBe('{ "baz": 123 }');
  });
});

describe("replace", () => {
  it("should delete fields not in changes", () => {
    const json = '{ "foo": "bar", "baz": 123 }';
    const result = replace(json, { baz: 456 });
    expect(result).toBe('{ "baz": 456 }');
  });

  it("should work like modify", () => {
    const json = '{ "a": 1, "b": 2, "c": 3 }';
    expect(replace(json, { a: 10, b: 20 })).toBe(
      modify(json, { a: 10, b: 20 })
    );
  });
});

describe("patch", () => {
  it("should update without deleting (like merge)", () => {
    const json = '{ "foo": "bar", "baz": 123 }';
    const result = patch(json, { foo: "updated" });
    expect(result).toBe('{ "foo": "updated", "baz": 123 }');
  });

  it("should delete with explicit undefined", () => {
    const json = '{ "foo": "bar", "baz": 123 }';
    const result = patch(json, { foo: undefined });
    expect(result).toBe('{ "baz": 123 }');
  });
});

describe("rename", () => {
  it("should rename a key", () => {
    const json = '{ "oldName": "value" }';
    const result = rename(json, ["oldName"], "newName");
    expect(get(result, ["newName"])).toBe("value");
    expect(has(result, ["oldName"])).toBe(false);
  });

  it("should rename nested keys", () => {
    const json = '{ "config": { "oldKey": 123 } }';
    const result = rename(json, ["config", "oldKey"], "newKey");
    expect(get(result, ["config", "newKey"])).toBe(123);
    expect(has(result, ["config", "oldKey"])).toBe(false);
  });

  it("should rename and preserve value", () => {
    const json = `{
  "oldName": "value",
  "other": 123
}`;
    const result = rename(json, ["oldName"], "newName");
    expect(get(result, ["newName"])).toBe("value");
    expect(has(result, ["oldName"])).toBe(false);
    expect(get(result, ["other"])).toBe(123);
  });
});

describe("move", () => {
  it("should move a field to a new location", () => {
    const json = '{ "source": { "value": 123 }, "target": {} }';
    const result = move(json, ["source", "value"], ["target", "value"]);
    expect(get(result, ["target", "value"])).toBe(123);
    expect(has(result, ["source", "value"])).toBe(false);
  });

  it("should move to a new top-level key", () => {
    const json = '{ "nested": { "value": 123 } }';
    const result = move(json, ["nested", "value"], ["topLevel"]);
    expect(get(result, ["topLevel"])).toBe(123);
  });
});

describe("setComment", () => {
  it("should add a comment above a field", () => {
    const json = `{
  "foo": "bar"
}`;
    const result = setComment(json, ["foo"], "foo: this is foo");
    expect(result).toContain("// foo: this is foo");
    expect(result).toContain('"foo": "bar"');
  });

  it("should update an existing comment", () => {
    const json = `{
  // old comment
  "foo": "bar"
}`;
    const result = setComment(json, ["foo"], "new comment");
    expect(result).toContain("// new comment");
    expect(result).not.toContain("old comment");
  });

  it("should work with nested fields", () => {
    const json = `{
  "config": {
    "enabled": true
  }
}`;
    const result = setComment(
      json,
      ["config", "enabled"],
      "enabled: controls the feature"
    );
    expect(result).toContain("// enabled: controls the feature");
  });
});

describe("removeComment", () => {
  it("should remove a comment above a field", () => {
    const json = `{
  // this is a comment
  "foo": "bar"
}`;
    const result = removeComment(json, ["foo"]);
    expect(result).not.toContain("// this is a comment");
    expect(result).toContain('"foo": "bar"');
  });

  it("should handle block comments", () => {
    const json = `{
  /* block comment */
  "foo": "bar"
}`;
    const result = removeComment(json, ["foo"]);
    expect(result).not.toContain("block comment");
    expect(result).toContain('"foo": "bar"');
  });

  it("should do nothing if no comment exists", () => {
    const json = `{
  "foo": "bar"
}`;
    const result = removeComment(json, ["foo"]);
    expect(result).toBe(json);
  });
});

describe("sort", () => {
  it("should sort keys alphabetically", () => {
    const json = `{
  "z": 1,
  "a": 2,
  "m": 3
}`;
    const result = sort(json);
    expect(result).toBe(`{
  "a": 2,
  "m": 3,
  "z": 1
}`);
  });

  it("should preserve comments with their keys", () => {
    const json = `{
  // z comment
  "z": 1,
  // a comment
  "a": 2
}`;
    const result = sort(json);
    expect(result).toContain("// a comment");
    expect(result).toContain("// z comment");
    // a should come before z
    const aPos = result.indexOf('"a"');
    const zPos = result.indexOf('"z"');
    expect(aPos).toBeLessThan(zPos);
  });

  it("should preserve block comments with their keys", () => {
    const json = `{
  /* z block */
  "z": 1,
  /* a block */
  "a": 2
}`;
    const result = sort(json);
    expect(result).toContain("/* a block */");
    expect(result).toContain("/* z block */");
  });

  it("should handle single-line objects", () => {
    const json = '{ "z": 1, "a": 2, "m": 3 }';
    const result = sort(json);
    expect(result).toBe('{ "a": 2, "m": 3, "z": 1 }');
  });

  it("should sort nested objects when deep=true (default)", () => {
    const json = `{
  "outer": {
    "z": 1,
    "a": 2
  }
}`;
    const result = sort(json);
    const aPos = result.indexOf('"a"');
    const zPos = result.indexOf('"z"');
    expect(aPos).toBeLessThan(zPos);
  });

  it("should not sort nested objects when deep=false", () => {
    const json = `{
  "z": 1,
  "a": {
    "z": 1,
    "a": 2
  }
}`;
    const result = sort(json, [], { deep: false });
    // Outer should be sorted (a before z)
    const outerAPos = result.indexOf('"a"');
    const outerZPos = result.indexOf('"z": 1');
    expect(outerAPos).toBeLessThan(outerZPos);
    // Inner should still have z before a
    const innerContent = result.slice(result.indexOf("{", 1));
    expect(innerContent.indexOf('"z"')).toBeLessThan(
      innerContent.indexOf('"a": 2')
    );
  });

  it("should accept custom comparator", () => {
    const json = `{
  "a": 1,
  "z": 2
}`;
    // Reverse alphabetical
    const result = sort(json, [], { comparator: (a, b) => b.localeCompare(a) });
    const aPos = result.indexOf('"a"');
    const zPos = result.indexOf('"z"');
    expect(zPos).toBeLessThan(aPos);
  });

  it("should handle already sorted objects", () => {
    const json = `{
  "a": 1,
  "b": 2,
  "c": 3
}`;
    const result = sort(json);
    expect(result).toBe(json);
  });

  it("should handle objects with various value types", () => {
    const json = `{
  "string": "hello",
  "number": 42,
  "array": [1, 2, 3],
  "null": null,
  "bool": true
}`;
    const result = sort(json);
    // Should be sorted: array, bool, null, number, string
    const arrayPos = result.indexOf('"array"');
    const boolPos = result.indexOf('"bool"');
    const nullPos = result.indexOf('"null"');
    const numberPos = result.indexOf('"number"');
    const stringPos = result.indexOf('"string"');
    expect(arrayPos).toBeLessThan(boolPos);
    expect(boolPos).toBeLessThan(nullPos);
    expect(nullPos).toBeLessThan(numberPos);
    expect(numberPos).toBeLessThan(stringPos);
  });

  it("should handle empty objects", () => {
    const json = "{}";
    const result = sort(json);
    expect(result).toBe("{}");
  });

  it("should handle single property objects", () => {
    const json = '{ "only": 1 }';
    const result = sort(json);
    expect(result).toBe('{ "only": 1 }');
  });

  it("should preserve values correctly", () => {
    const json = `{
  "z": { "nested": true },
  "a": [1, 2, 3]
}`;
    const result = sort(json);
    expect(get(result, ["a"])).toEqual([1, 2, 3]);
    expect(get(result, ["z", "nested"])).toBe(true);
  });

  it("should preserve trailing commas in multi-line objects", () => {
    const json = `{
  "z": 1,
  "a": 2,
}`;
    const result = sort(json);
    expect(result).toBe(`{
  "a": 2,
  "z": 1,
}`);
  });

  it("should not add trailing commas when original has none", () => {
    const json = `{
  "z": 1,
  "a": 2
}`;
    const result = sort(json);
    expect(result).toBe(`{
  "a": 2,
  "z": 1
}`);
  });

  it("should preserve trailing commas in single-line objects", () => {
    const json = '{ "z": 1, "a": 2, }';
    const result = sort(json);
    expect(result).toBe('{ "a": 2, "z": 1, }');
  });

  it("should sort at a specific path", () => {
    const json = `{
  "keep": "unsorted",
  "nested": {
    "z": 1,
    "a": 2
  }
}`;
    const result = sort(json, ["nested"]);
    // Root should be unchanged
    expect(result.indexOf('"keep"')).toBeLessThan(result.indexOf('"nested"'));
    // Nested should be sorted
    const nestedStart = result.indexOf('"nested"');
    const nestedContent = result.slice(nestedStart);
    expect(nestedContent.indexOf('"a"')).toBeLessThan(
      nestedContent.indexOf('"z"')
    );
  });

  it("should sort deeply within a path", () => {
    const json = `{
  "config": {
    "z": {
      "z": 1,
      "a": 2
    },
    "a": 3
  }
}`;
    const result = sort(json, ["config"]);
    // config should have a before z
    const configStart = result.indexOf('"config"');
    const configContent = result.slice(configStart);
    expect(configContent.indexOf('"a": 3')).toBeLessThan(
      configContent.indexOf('"z":')
    );
    // nested object inside z should also be sorted (deep=true by default)
    expect(get(result, ["config", "z", "a"])).toBe(2);
  });

  it("should only sort at path with deep=false", () => {
    const json = `{
  "outer": {
    "z": {
      "z": 1,
      "a": 2
    },
    "a": 3
  }
}`;
    const result = sort(json, ["outer"], { deep: false });
    // outer should be sorted (a before z)
    const outerContent = result.slice(result.indexOf('"outer"'));
    expect(outerContent.indexOf('"a": 3')).toBeLessThan(
      outerContent.indexOf('"z":')
    );
    // Inner z object should NOT be sorted (z should still be before a)
    const innerZ = result.slice(result.indexOf('"z": {'));
    expect(innerZ.indexOf('"z": 1')).toBeLessThan(innerZ.indexOf('"a": 2'));
  });

  it("should return unchanged if path does not exist", () => {
    const json = '{ "a": 1 }';
    const result = sort(json, ["nonexistent"]);
    expect(result).toBe(json);
  });

  it("should return unchanged if path is not an object", () => {
    const json = '{ "a": [3, 1, 2] }';
    const result = sort(json, ["a"]);
    expect(result).toBe(json);
  });
});

describe("trailing comments", () => {
  describe("getTrailingComment", () => {
    it("should get a line comment after a field", () => {
      const json = `{
  "foo": "bar", // this is foo
  "baz": 123
}`;
      expect(getTrailingComment(json, ["foo"])).toBe("this is foo");
    });

    it("should get a block comment after a field", () => {
      const json = `{
  "foo": "bar", /* block comment */
  "baz": 123
}`;
      expect(getTrailingComment(json, ["foo"])).toBe("block comment");
    });

    it("should return null if no trailing comment exists", () => {
      const json = `{
  "foo": "bar",
  "baz": 123
}`;
      expect(getTrailingComment(json, ["foo"])).toBeNull();
    });

    it("should return null for non-existent path", () => {
      const json = '{ "foo": "bar" }';
      expect(getTrailingComment(json, ["nonexistent"])).toBeNull();
    });

    it("should get trailing comment from nested field", () => {
      const json = `{
  "config": {
    "enabled": true // is it enabled?
  }
}`;
      expect(getTrailingComment(json, ["config", "enabled"])).toBe(
        "is it enabled?"
      );
    });

    it("should get trailing comment on last property (no comma)", () => {
      const json = `{
  "foo": "bar" // trailing on last
}`;
      expect(getTrailingComment(json, ["foo"])).toBe("trailing on last");
    });
  });

  describe("getComment with trailing comments", () => {
    it("should fall back to trailing comment if no comment above", () => {
      const json = `{
  "foo": "bar", // trailing comment
  "baz": 123
}`;
      expect(getComment(json, ["foo"])).toBe("trailing comment");
    });

    it("should prefer comment above over trailing comment", () => {
      const json = `{
  // comment above
  "foo": "bar", // trailing comment
  "baz": 123
}`;
      expect(getComment(json, ["foo"])).toBe("comment above");
    });
  });

  describe("setTrailingComment", () => {
    it("should add a trailing comment after a field", () => {
      const json = `{
  "foo": "bar",
  "baz": 123
}`;
      const result = setTrailingComment(json, ["foo"], "this is foo");
      expect(result).toContain('"foo": "bar" // this is foo,');
    });

    it("should update an existing trailing comment", () => {
      const json = `{
  "foo": "bar", // old comment
  "baz": 123
}`;
      const result = setTrailingComment(json, ["foo"], "new comment");
      expect(result).toContain("// new comment");
      expect(result).not.toContain("old comment");
    });

    it("should work with nested fields", () => {
      const json = `{
  "config": {
    "enabled": true,
    "count": 5
  }
}`;
      const result = setTrailingComment(
        json,
        ["config", "enabled"],
        "is it on?"
      );
      expect(result).toContain('"enabled": true // is it on?');
    });

    it("should add trailing comment on last property (no comma)", () => {
      const json = `{
  "foo": "bar"
}`;
      const result = setTrailingComment(json, ["foo"], "the only one");
      expect(result).toContain('"foo": "bar" // the only one');
    });
  });

  describe("removeTrailingComment", () => {
    it("should remove a trailing comment after a field", () => {
      const json = `{
  "foo": "bar", // this is a comment
  "baz": 123
}`;
      const result = removeTrailingComment(json, ["foo"]);
      expect(result).not.toContain("// this is a comment");
      expect(result).toContain('"foo": "bar",');
    });

    it("should handle block comments", () => {
      const json = `{
  "foo": "bar", /* block comment */
  "baz": 123
}`;
      const result = removeTrailingComment(json, ["foo"]);
      expect(result).not.toContain("block comment");
      expect(result).toContain('"foo": "bar",');
    });

    it("should do nothing if no trailing comment exists", () => {
      const json = `{
  "foo": "bar",
  "baz": 123
}`;
      const result = removeTrailingComment(json, ["foo"]);
      expect(result).toBe(json);
    });
  });

  describe("remove with trailing comments", () => {
    it("should remove a field with its trailing comment", () => {
      const json = `{
  "foo": "bar", // this is foo
  "baz": 123
}`;
      const result = remove(json, ["foo"]);
      expect(result).toBe(`{
  "baz": 123
}`);
    });

    it("should preserve trailing comment starting with **", () => {
      const json = `{
  "foo": "bar", // ** important note
  "baz": 123
}`;
      const result = remove(json, ["foo"]);
      expect(result).toContain("// ** important note");
    });

    it("should remove field with both above and trailing comment", () => {
      const json = `{
  // comment above
  "foo": "bar", // trailing comment
  "baz": 123
}`;
      const result = remove(json, ["foo"]);
      expect(result).toBe(`{
  "baz": 123
}`);
    });
  });

  describe("sort with trailing comments", () => {
    it("should preserve trailing comments when sorting", () => {
      const json = `{
  "z": 1, // z comment
  "a": 2 // a comment
}`;
      const result = sort(json);
      expect(result).toContain('"a": 2 // a comment');
      expect(result).toContain('"z": 1 // z comment');
      // a should come before z
      const aPos = result.indexOf('"a"');
      const zPos = result.indexOf('"z"');
      expect(aPos).toBeLessThan(zPos);
    });

    it("should preserve both above and trailing comments when sorting", () => {
      const json = `{
  // z above
  "z": 1, // z trailing
  // a above
  "a": 2 // a trailing
}`;
      const result = sort(json);
      expect(result).toContain("// a above");
      expect(result).toContain("// a trailing");
      expect(result).toContain("// z above");
      expect(result).toContain("// z trailing");
    });

    it("should handle single-line objects with trailing comments", () => {
      const json = '{ "z": 1 /* z */, "a": 2 /* a */ }';
      const result = sort(json);
      expect(result).toContain('"a": 2 // a');
      expect(result).toContain('"z": 1 // z');
    });
  });

  describe("modify with trailing comments", () => {
    it("should preserve trailing comments when modifying values", () => {
      const json = `{
  "foo": "bar", // important field
  "baz": 123
}`;
      const result = modify(json, { foo: "updated", baz: 123 });
      expect(result).toContain('"foo": "updated"');
      expect(result).toContain("// important field");
    });

    it("should delete trailing comment with field when using modify", () => {
      const json = `{
  "foo": "bar", // will be deleted
  "baz": 123
}`;
      const result = modify(json, { baz: 123 });
      expect(result).not.toContain("will be deleted");
      expect(result).toBe(`{
  "baz": 123
}`);
    });
  });

  describe("set with trailing comments", () => {
    it("should preserve trailing comments when setting a value", () => {
      const json = `{
  "foo": "bar", // trailing comment
  "baz": 123
}`;
      const result = set(json, ["foo"], "updated");
      expect(result).toContain('"foo": "updated"');
      expect(result).toContain("// trailing comment");
    });

    it("should preserve trailing comment on other fields when setting", () => {
      const json = `{
  "foo": "bar",
  "baz": 123 // baz comment
}`;
      const result = set(json, ["foo"], "updated");
      expect(result).toContain('"foo": "updated"');
      expect(result).toContain("// baz comment");
    });

    it("should preserve trailing comments when adding a new field", () => {
      const json = `{
  "foo": "bar" // trailing comment
}`;
      const result = set(json, ["baz"], 123);
      expect(result).toContain("// trailing comment");
      expect(result).toContain('"baz": 123');
    });

    it("should preserve trailing comments in nested objects", () => {
      const json = `{
  "config": {
    "enabled": true, // is enabled
    "count": 5
  }
}`;
      const result = set(json, ["config", "count"], 10);
      expect(result).toContain('"count": 10');
      expect(result).toContain("// is enabled");
    });
  });

  describe("merge with trailing comments", () => {
    it("should preserve trailing comments when merging", () => {
      const json = `{
  "foo": "bar", // foo comment
  "baz": 123 // baz comment
}`;
      const result = merge(json, { foo: "updated" });
      expect(result).toContain('"foo": "updated"');
      expect(result).toContain("// foo comment");
      expect(result).toContain("// baz comment");
    });

    it("should preserve trailing comments when adding via merge", () => {
      const json = `{
  "foo": "bar" // existing comment
}`;
      const result = merge(json, { baz: 123 });
      expect(result).toContain("// existing comment");
      expect(result).toContain('"baz": 123');
    });

    it("should delete trailing comment when field is deleted via undefined", () => {
      const json = `{
  "foo": "bar", // will be deleted
  "baz": 123
}`;
      const result = merge(json, { foo: undefined });
      expect(result).not.toContain("will be deleted");
      expect(result).toContain('"baz": 123');
    });
  });

  describe("replace with trailing comments", () => {
    it("should preserve trailing comments on kept fields", () => {
      const json = `{
  "foo": "bar", // foo comment
  "baz": 123 // baz comment
}`;
      const result = replace(json, { baz: 456 });
      expect(result).not.toContain("foo comment");
      expect(result).toContain("// baz comment");
      expect(result).toContain('"baz": 456');
    });

    it("should delete trailing comments on removed fields", () => {
      const json = `{
  "foo": "bar", // will be gone
  "baz": 123
}`;
      const result = replace(json, { baz: 123 });
      expect(result).not.toContain("will be gone");
    });
  });

  describe("patch with trailing comments", () => {
    it("should preserve trailing comments when patching", () => {
      const json = `{
  "foo": "bar", // foo comment
  "baz": 123 // baz comment
}`;
      const result = patch(json, { foo: "patched" });
      expect(result).toContain('"foo": "patched"');
      expect(result).toContain("// foo comment");
      expect(result).toContain("// baz comment");
    });

    it("should delete trailing comment when field removed via patch", () => {
      const json = `{
  "foo": "bar", // foo comment
  "baz": 123
}`;
      const result = patch(json, { foo: undefined });
      expect(result).not.toContain("foo comment");
    });
  });

  describe("rename with trailing comments", () => {
    it("should preserve trailing comments on other fields when renaming", () => {
      const json = `{
  "foo": "bar",
  "baz": 123 // baz comment
}`;
      const result = rename(json, ["foo"], "renamed");
      expect(result).toContain('"renamed"');
      expect(result).toContain("// baz comment");
    });
  });

  describe("move with trailing comments", () => {
    it("should preserve trailing comments on other fields when moving", () => {
      const json = `{
  "source": {
    "value": 123
  },
  "target": {} // target comment
}`;
      const result = move(json, ["source", "value"], ["target", "value"]);
      expect(result).toContain("// target comment");
    });

    it("should remove trailing comment with moved field", () => {
      const json = `{
  "source": {
    "value": 123 // value comment
  },
  "target": {}
}`;
      const result = move(json, ["source", "value"], ["target", "moved"]);
      expect(result).not.toContain("value comment");
    });
  });

  describe("setComment with trailing comments", () => {
    it("should add comment above while preserving trailing comment", () => {
      const json = `{
  "foo": "bar", // trailing
  "baz": 123
}`;
      const result = setComment(json, ["foo"], "comment above");
      expect(result).toContain("// comment above");
      expect(result).toContain("// trailing");
    });

    it("should update comment above while preserving trailing comment", () => {
      const json = `{
  // old above
  "foo": "bar", // trailing
  "baz": 123
}`;
      const result = setComment(json, ["foo"], "new above");
      expect(result).toContain("// new above");
      expect(result).not.toContain("old above");
      expect(result).toContain("// trailing");
    });
  });

  describe("removeComment with trailing comments", () => {
    it("should remove comment above while preserving trailing comment", () => {
      const json = `{
  // comment above
  "foo": "bar", // trailing
  "baz": 123
}`;
      const result = removeComment(json, ["foo"]);
      expect(result).not.toContain("comment above");
      expect(result).toContain("// trailing");
    });
  });
});
