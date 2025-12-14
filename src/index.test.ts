import { describe, expect, it } from "vitest";
import {
  get,
  has,
  merge,
  modify,
  move,
  patch,
  remove,
  removeComment,
  rename,
  replace,
  set,
  setComment
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
