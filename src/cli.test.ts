import { describe, expect, it } from "vitest";
import { diff, parsePath } from "./cli-lib";

describe("parsePath", () => {
  it("should parse simple dot-notation paths", () => {
    expect(parsePath("foo")).toEqual(["foo"]);
    expect(parsePath("foo.bar")).toEqual(["foo", "bar"]);
    expect(parsePath("foo.bar.baz")).toEqual(["foo", "bar", "baz"]);
  });

  it("should parse numeric indices as numbers", () => {
    expect(parsePath("items.0")).toEqual(["items", 0]);
    expect(parsePath("items.1.name")).toEqual(["items", 1, "name"]);
  });

  it("should parse bracket notation for indices", () => {
    expect(parsePath("items[0]")).toEqual(["items", 0]);
    expect(parsePath("items[0].name")).toEqual(["items", 0, "name"]);
    expect(parsePath("data[0][1]")).toEqual(["data", 0, 1]);
  });

  it("should handle mixed notation", () => {
    expect(parsePath("config.items[0].enabled")).toEqual([
      "config",
      "items",
      0,
      "enabled"
    ]);
  });

  it("should return empty array for empty string", () => {
    expect(parsePath("")).toEqual([]);
  });

  it("should handle negative numbers as string keys", () => {
    expect(parsePath("foo.-1")).toEqual(["foo", "-1"]);
  });

  it("should parse consecutive numbers as separate indices", () => {
    expect(parsePath("foo.1.5")).toEqual(["foo", 1, 5]);
  });
});

describe("diff", () => {
  it("should show no changes for identical strings", () => {
    const result = diff("hello", "hello");
    expect(result).toContain("hello");
    expect(result).not.toContain("-");
    expect(result).not.toContain("+");
  });

  it("should show additions", () => {
    const result = diff("line1", "line1\nline2");
    expect(result).toContain("line1");
    expect(result).toContain("+ line2");
  });

  it("should show removals", () => {
    const result = diff("line1\nline2", "line1");
    expect(result).toContain("line1");
    expect(result).toContain("- line2");
  });

  it("should show changes", () => {
    const result = diff("old", "new");
    expect(result).toContain("- old");
    expect(result).toContain("+ new");
  });

  it("should handle multi-line JSON diffs", () => {
    const old = `{
  "name": "test",
  "value": 123
}`;
    const updated = `{
  "name": "test",
  "value": 456
}`;
    const result = diff(old, updated);
    expect(result).toContain("- ");
    expect(result).toContain("+ ");
    expect(result).toContain("123");
    expect(result).toContain("456");
  });
});
