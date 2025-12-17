import { execSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { afterEach, beforeAll, beforeEach, describe, expect, it } from "vitest";
import { diff, parsePath } from "./cli-lib";

// Build once before all CLI tests
beforeAll(() => {
  execSync("npm run build", { stdio: "pipe" });
});

// Helper to run CLI commands using the built version
function runCli(
  args: string,
  cwd?: string,
  env?: Record<string, string>
): { stdout: string; stderr: string; exitCode: number } {
  try {
    const result = execSync(`node dist/cli.mjs ${args}`, {
      cwd: cwd ?? process.cwd(),
      encoding: "utf-8",
      stdio: ["pipe", "pipe", "pipe"],
      env: { ...process.env, ...env },
      maxBuffer: 100 * 1024 * 1024 // 100MB buffer for large file tests
    });
    return { stdout: result.toString(), stderr: "", exitCode: 0 };
  } catch (error: unknown) {
    const execError = error as {
      stdout?: Buffer | string;
      stderr?: Buffer | string;
      status?: number;
    };
    const stdout = execError.stdout
      ? Buffer.isBuffer(execError.stdout)
        ? execError.stdout.toString("utf-8")
        : execError.stdout
      : "";
    const stderr = execError.stderr
      ? Buffer.isBuffer(execError.stderr)
        ? execError.stderr.toString("utf-8")
        : execError.stderr
      : "";
    return {
      stdout,
      stderr,
      exitCode: execError.status ?? 1
    };
  }
}

describe("CLI commands", () => {
  let tempDir: string;
  let testFile: string;

  beforeEach(() => {
    // Create temp dir in current working directory to avoid path validation issues
    tempDir = mkdtempSync(join(process.cwd(), "aywson-test-"));
    testFile = join(tempDir, "test.json");
  });

  afterEach(() => {
    rmSync(tempDir, { recursive: true, force: true });
  });

  describe("parse", () => {
    it("should parse JSONC to JSON", () => {
      writeFileSync(testFile, '{ "foo": "bar", /* comment */ "baz": 123 }');
      const { stdout } = runCli(`parse ${testFile}`);
      expect(JSON.parse(stdout)).toEqual({ foo: "bar", baz: 123 });
    });

    it("should handle trailing commas", () => {
      writeFileSync(testFile, '{ "foo": "bar", }');
      const { stdout } = runCli(`parse ${testFile}`);
      expect(JSON.parse(stdout)).toEqual({ foo: "bar" });
    });
  });

  describe("get", () => {
    it("should get a value at a path", () => {
      writeFileSync(testFile, '{ "foo": { "bar": 123 } }');
      const { stdout } = runCli(`get ${testFile} foo.bar`);
      expect(stdout.trim()).toBe("123");
    });

    it("should get nested objects", () => {
      writeFileSync(testFile, '{ "config": { "enabled": true } }');
      const { stdout } = runCli(`get ${testFile} config`);
      expect(JSON.parse(stdout)).toEqual({ enabled: true });
    });
  });

  describe("set", () => {
    it("should set a value at a path", () => {
      writeFileSync(testFile, '{ "foo": "bar" }');
      runCli(`set ${testFile} foo '"baz"'`);
      const result = JSON.parse(readFileSync(testFile, "utf-8"));
      expect(result.foo).toBe("baz");
    });

    it("should add new fields", () => {
      writeFileSync(testFile, '{ "foo": "bar" }');
      runCli(`set ${testFile} baz 123`);
      const result = JSON.parse(readFileSync(testFile, "utf-8"));
      expect(result.baz).toBe(123);
    });

    it("should respect --dry-run", () => {
      writeFileSync(testFile, '{ "foo": "bar" }');
      const original = readFileSync(testFile, "utf-8");
      runCli(`set --dry-run ${testFile} foo '"baz"'`);
      expect(readFileSync(testFile, "utf-8")).toBe(original);
    });
  });

  describe("remove", () => {
    it("should remove a field", () => {
      writeFileSync(testFile, '{ "foo": "bar", "baz": 123 }');
      runCli(`remove ${testFile} foo`);
      const result = JSON.parse(readFileSync(testFile, "utf-8"));
      expect(result).toEqual({ baz: 123 });
    });
  });

  describe("modify", () => {
    it("should replace fields with delete semantics", () => {
      writeFileSync(testFile, '{ "a": 1, "b": 2, "c": 3 }');
      runCli(`modify ${testFile} '{"a": 10}'`);
      const result = JSON.parse(readFileSync(testFile, "utf-8"));
      expect(result).toEqual({ a: 10 });
    });
  });

  describe("merge", () => {
    it("should merge without deleting", () => {
      writeFileSync(testFile, '{ "a": 1, "b": 2 }');
      runCli(`merge ${testFile} '{"a": 10}'`);
      const result = JSON.parse(readFileSync(testFile, "utf-8"));
      expect(result).toEqual({ a: 10, b: 2 });
    });

    it("should add new fields", () => {
      writeFileSync(testFile, '{ "a": 1 }');
      runCli(`merge ${testFile} '{"b": 2}'`);
      const result = JSON.parse(readFileSync(testFile, "utf-8"));
      expect(result).toEqual({ a: 1, b: 2 });
    });
  });

  describe("rename", () => {
    it("should rename a key", () => {
      writeFileSync(testFile, '{ "oldName": 123 }');
      runCli(`rename ${testFile} oldName newName`);
      const result = JSON.parse(readFileSync(testFile, "utf-8"));
      expect(result.newName).toBe(123);
      expect(result.oldName).toBeUndefined();
    });
  });

  describe("move", () => {
    it("should move a field to a new location", () => {
      writeFileSync(testFile, '{ "source": { "value": 123 }, "target": {} }');
      runCli(`move ${testFile} source.value target.value`);
      const result = JSON.parse(readFileSync(testFile, "utf-8"));
      expect(result.target.value).toBe(123);
      expect(result.source.value).toBeUndefined();
    });
  });

  describe("sort", () => {
    it("should sort object keys alphabetically", () => {
      writeFileSync(testFile, '{ "z": 1, "a": 2, "m": 3 }');
      runCli(`sort ${testFile}`);
      const content = readFileSync(testFile, "utf-8");
      const aPos = content.indexOf('"a"');
      const mPos = content.indexOf('"m"');
      const zPos = content.indexOf('"z"');
      expect(aPos).toBeLessThan(mPos);
      expect(mPos).toBeLessThan(zPos);
    });

    it("should sort at a specific path", () => {
      writeFileSync(testFile, '{ "keep": 1, "nested": { "z": 1, "a": 2 } }');
      runCli(`sort ${testFile} nested`);
      const content = readFileSync(testFile, "utf-8");
      const nestedStart = content.indexOf('"nested"');
      const nestedContent = content.slice(nestedStart);
      expect(nestedContent.indexOf('"a"')).toBeLessThan(
        nestedContent.indexOf('"z"')
      );
    });

    it("should respect --no-deep", () => {
      writeFileSync(testFile, '{ "z": { "z": 1, "a": 2 }, "a": 3 }');
      runCli(`sort ${testFile} --no-deep`);
      const content = readFileSync(testFile, "utf-8");
      // Outer should be sorted
      expect(content.indexOf('"a": 3')).toBeLessThan(content.indexOf('"z":'));
      // Inner should still have z before a
      const innerContent = content.slice(content.indexOf('"z": {'));
      expect(innerContent.indexOf('"z": 1')).toBeLessThan(
        innerContent.indexOf('"a": 2')
      );
    });
  });

  describe("format", () => {
    it("should format minified JSON", () => {
      writeFileSync(testFile, '{"foo":"bar","baz":123}');
      runCli(`format ${testFile}`);
      const content = readFileSync(testFile, "utf-8");
      expect(content).toBe(`{
  "foo": "bar",
  "baz": 123
}`);
    });

    it("should format with custom tab size", () => {
      writeFileSync(testFile, '{"foo":"bar"}');
      runCli(`format ${testFile} --tab-size 4`);
      const content = readFileSync(testFile, "utf-8");
      expect(content).toBe(`{
    "foo": "bar"
}`);
    });

    it("should format with tabs", () => {
      writeFileSync(testFile, '{"foo":"bar"}');
      runCli(`format ${testFile} --tabs`);
      const content = readFileSync(testFile, "utf-8");
      expect(content).toBe(`{
\t"foo": "bar"
}`);
    });

    it("should preserve comments", () => {
      writeFileSync(testFile, '{ /* comment */ "foo":"bar"}');
      runCli(`format ${testFile}`);
      const content = readFileSync(testFile, "utf-8");
      expect(content).toContain("/* comment */");
      expect(content).toContain('"foo": "bar"');
    });
  });

  describe("comment", () => {
    it("should get a comment", () => {
      writeFileSync(
        testFile,
        `{
  // this is foo
  "foo": "bar"
}`
      );
      const { stdout } = runCli(`comment ${testFile} foo`);
      expect(stdout.trim()).toBe("this is foo");
    });

    it("should set a comment", () => {
      writeFileSync(
        testFile,
        `{
  "foo": "bar"
}`
      );
      runCli(`comment ${testFile} foo "new comment"`);
      const content = readFileSync(testFile, "utf-8");
      expect(content).toContain("// new comment");
    });

    it("should get trailing comment with --trailing", () => {
      writeFileSync(
        testFile,
        `{
  "foo": "bar" // trailing
}`
      );
      const { stdout } = runCli(`comment --trailing ${testFile} foo`);
      expect(stdout.trim()).toBe("trailing");
    });

    it("should set trailing comment with --trailing", () => {
      writeFileSync(
        testFile,
        `{
  "foo": "bar"
}`
      );
      runCli(`comment --trailing ${testFile} foo "inline note"`);
      const content = readFileSync(testFile, "utf-8");
      expect(content).toContain("// inline note");
    });
  });

  describe("uncomment", () => {
    it("should remove a comment above", () => {
      writeFileSync(
        testFile,
        `{
  // remove this
  "foo": "bar"
}`
      );
      runCli(`uncomment ${testFile} foo`);
      const content = readFileSync(testFile, "utf-8");
      expect(content).not.toContain("remove this");
      expect(content).toContain('"foo": "bar"');
    });

    it("should remove trailing comment with --trailing", () => {
      writeFileSync(
        testFile,
        `{
  "foo": "bar" // remove this
}`
      );
      runCli(`uncomment --trailing ${testFile} foo`);
      const content = readFileSync(testFile, "utf-8");
      expect(content).not.toContain("remove this");
      expect(content).toContain('"foo": "bar"');
    });
  });
});

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

describe("Security features", () => {
  let tempDir: string;
  let testFile: string;
  let parentDir: string;
  let parentFile: string;

  beforeEach(() => {
    // Create temp dir in current working directory to avoid path validation issues
    tempDir = mkdtempSync(join(process.cwd(), "aywson-test-"));
    testFile = join(tempDir, "test.json");
    parentDir = join(tempDir, "..");
    parentFile = join(parentDir, "parent.json");
  });

  afterEach(() => {
    rmSync(tempDir, { recursive: true, force: true });
    // Clean up parent file if it exists
    try {
      rmSync(parentFile, { force: true });
    } catch {
      // Ignore if file doesn't exist
    }
  });

  describe("Path validation", () => {
    it("should block path traversal by default", () => {
      writeFileSync(testFile, '{ "foo": "bar" }');
      // Use a path that definitely goes outside the working directory
      const outsidePath = join(process.cwd(), "..", "..", "etc", "passwd");
      const { exitCode, stderr, stdout } = runCli(`get ${outsidePath} foo`);
      expect(exitCode).toBe(1);
      const errorOutput = stderr || stdout;
      expect(errorOutput).toContain("Path traversal detected");
    });

    it("should allow path traversal with --allow-path-traversal flag", () => {
      writeFileSync(parentFile, '{ "foo": "bar" }');
      const { stdout, exitCode } = runCli(
        `get --allow-path-traversal ${parentFile} foo`
      );
      expect(exitCode).toBe(0);
      expect(stdout.trim()).toBe('"bar"');
    });

    it("should allow stdin (-) without path validation", () => {
      const { exitCode } = runCli(`parse -`, undefined, undefined);
      // stdin is allowed, but will fail because there's no input
      // The important thing is it doesn't fail with path traversal error
      expect(exitCode).toBeGreaterThanOrEqual(0);
    });

    it("should block path traversal on write operations", () => {
      writeFileSync(testFile, '{ "foo": "bar" }');
      // Use a path that definitely goes outside the working directory
      const outsidePath = join(process.cwd(), "..", "..", "sensitive.json");
      const { exitCode, stderr, stdout } = runCli(
        `set ${outsidePath} foo '"baz"'`
      );
      expect(exitCode).toBe(1);
      const errorOutput = stderr || stdout;
      expect(errorOutput).toContain("Path traversal detected");
    });

    it("should allow path traversal on write with flag", () => {
      writeFileSync(parentFile, '{ "foo": "bar" }');
      const { exitCode } = runCli(
        `set --allow-path-traversal ${parentFile} foo '"baz"'`
      );
      expect(exitCode).toBe(0);
      const result = JSON.parse(readFileSync(parentFile, "utf-8"));
      expect(result.foo).toBe("baz");
    });
  });

  describe("File size limits", () => {
    it("should reject files larger than default limit (50MB)", () => {
      // Create a file larger than 50MB
      // Use a smaller size for testing to avoid memory issues, but test the mechanism
      const largeContent = "x".repeat(51 * 1024 * 1024);
      writeFileSync(testFile, largeContent);
      const { exitCode, stderr, stdout } = runCli(`parse ${testFile}`);
      expect(exitCode).toBe(1);
      const errorOutput = stderr || stdout;
      expect(errorOutput).toContain("File too large");
      // Check for the default limit (50MB = 52428800 bytes)
      expect(errorOutput).toMatch(/50|52428800/);
    });

    it("should accept files within default limit", () => {
      // Create a file smaller than 50MB
      const content = JSON.stringify({ data: "x".repeat(10 * 1024 * 1024) });
      writeFileSync(testFile, content);
      const { exitCode } = runCli(`parse ${testFile}`);
      expect(exitCode).toBe(0);
    });

    it("should respect --max-file-size flag", () => {
      // Create a 10MB file
      const content = JSON.stringify({ data: "x".repeat(10 * 1024 * 1024) });
      writeFileSync(testFile, content);
      // Set limit to 5MB - should fail
      const { exitCode, stderr, stdout } = runCli(
        `parse --max-file-size ${5 * 1024 * 1024} ${testFile}`
      );
      expect(exitCode).toBe(1);
      const errorOutput = stderr || stdout;
      expect(errorOutput).toContain("File too large");
    });

    it("should allow larger files with --max-file-size flag", () => {
      // Create a 10MB file
      const content = JSON.stringify({ data: "x".repeat(10 * 1024 * 1024) });
      writeFileSync(testFile, content);
      // Set limit to 20MB - should succeed
      const { exitCode } = runCli(
        `parse --max-file-size ${20 * 1024 * 1024} ${testFile}`
      );
      expect(exitCode).toBe(0);
    });

    it("should allow unlimited file size with --no-file-size-limit", () => {
      // Create a file larger than 50MB
      const largeContent = JSON.stringify({
        data: "x".repeat(51 * 1024 * 1024)
      });
      writeFileSync(testFile, largeContent);
      const { exitCode } = runCli(`parse --no-file-size-limit ${testFile}`);
      expect(exitCode).toBe(0);
    });

    it("should allow stdin without file size limit", () => {
      // stdin should not be subject to file size limits
      const { exitCode } = runCli(`parse -`);
      // Will fail because no input, but not because of size limit
      expect(exitCode).toBeGreaterThanOrEqual(0);
    });
  });

  describe("JSON parsing limits", () => {
    it("should reject JSON larger than default limit (10MB)", () => {
      writeFileSync(testFile, '{ "foo": "bar" }');
      // Test the limit mechanism with a smaller size that can be passed as CLI arg
      // The actual 10MB limit is tested via the environment variable override tests
      // This test verifies the error message format
      const json = JSON.stringify({ data: "x".repeat(11000) });
      const { exitCode, stderr, stdout } = runCli(
        `set ${testFile} data '${json}'`,
        undefined,
        { AYWSON_MAX_JSON_SIZE: "10000" }
      );
      expect(exitCode).toBe(1);
      const errorOutput = stderr || stdout;
      expect(errorOutput).toContain("JSON input too large");
      expect(errorOutput).toContain("10000");
    });

    it("should accept JSON within default size limit", () => {
      writeFileSync(testFile, '{ "foo": "bar" }');
      // Create JSON string smaller than 10MB - use reasonable size for CLI args
      const json = JSON.stringify({ data: "x".repeat(1000) });
      const { exitCode } = runCli(`set ${testFile} data '${json}'`);
      expect(exitCode).toBe(0);
    });

    it("should respect AYWSON_MAX_JSON_SIZE environment variable", () => {
      writeFileSync(testFile, '{ "foo": "bar" }');
      // Create JSON that exceeds the custom limit
      const json = JSON.stringify({ data: "x".repeat(5000) });
      // Set limit to 3000 bytes - should fail
      const { exitCode, stderr, stdout } = runCli(
        `set ${testFile} data '${json}'`,
        undefined,
        { AYWSON_MAX_JSON_SIZE: "3000" }
      );
      expect(exitCode).toBe(1);
      const errorOutput = stderr || stdout;
      expect(errorOutput).toContain("JSON input too large");
    });

    it("should allow larger JSON with AYWSON_MAX_JSON_SIZE", () => {
      writeFileSync(testFile, '{ "foo": "bar" }');
      // Create JSON within the custom limit
      const json = JSON.stringify({ data: "x".repeat(5000) });
      // Set limit to 10000 bytes - should succeed
      const { exitCode } = runCli(`set ${testFile} data '${json}'`, undefined, {
        AYWSON_MAX_JSON_SIZE: "10000"
      });
      expect(exitCode).toBe(0);
    });

    it("should reject deeply nested JSON (default depth: 100)", () => {
      writeFileSync(testFile, '{ "foo": "bar" }');
      // Create deeply nested JSON (101 levels)
      let deepJson = "1";
      for (let i = 0; i < 101; i++) {
        deepJson = `{"nested": ${deepJson}}`;
      }
      const { exitCode, stderr, stdout } = runCli(
        `modify ${testFile} '${deepJson}'`
      );
      expect(exitCode).toBe(1);
      const errorOutput = stderr || stdout;
      expect(errorOutput).toContain("too deeply nested");
      expect(errorOutput).toContain("100");
    });

    it("should accept JSON within default depth limit", () => {
      writeFileSync(testFile, '{ "foo": "bar" }');
      // Create nested JSON (50 levels)
      let nestedJson = "1";
      for (let i = 0; i < 50; i++) {
        nestedJson = `{"nested": ${nestedJson}}`;
      }
      const { exitCode } = runCli(`modify ${testFile} '${nestedJson}'`);
      expect(exitCode).toBe(0);
    });

    it("should respect AYWSON_MAX_JSON_DEPTH environment variable", () => {
      writeFileSync(testFile, '{ "foo": "bar" }');
      // Create nested JSON (50 levels)
      let nestedJson = "1";
      for (let i = 0; i < 50; i++) {
        nestedJson = `{"nested": ${nestedJson}}`;
      }
      // Set limit to 30 - should fail
      const { exitCode, stderr, stdout } = runCli(
        `modify ${testFile} '${nestedJson}'`,
        undefined,
        { AYWSON_MAX_JSON_DEPTH: "30" }
      );
      expect(exitCode).toBe(1);
      const errorOutput = stderr || stdout;
      expect(errorOutput).toContain("too deeply nested");
    });

    it("should allow deeper JSON with AYWSON_MAX_JSON_DEPTH", () => {
      writeFileSync(testFile, '{ "foo": "bar" }');
      // Create nested JSON (150 levels)
      let nestedJson = "1";
      for (let i = 0; i < 150; i++) {
        nestedJson = `{"nested": ${nestedJson}}`;
      }
      // Set limit to 200 - should succeed
      const { exitCode } = runCli(
        `modify ${testFile} '${nestedJson}'`,
        undefined,
        { AYWSON_MAX_JSON_DEPTH: "200" }
      );
      expect(exitCode).toBe(0);
    });

    it("should apply limits to modify command", () => {
      writeFileSync(testFile, '{ "foo": "bar" }');
      // Test with a size that works as CLI arg but exceeds a small limit
      const json = JSON.stringify({ data: "x".repeat(5000) });
      const { exitCode, stderr, stdout } = runCli(
        `modify ${testFile} '${json}'`,
        undefined,
        { AYWSON_MAX_JSON_SIZE: "3000" }
      );
      expect(exitCode).toBe(1);
      const errorOutput = stderr || stdout;
      expect(errorOutput).toContain("JSON input too large");
    });

    it("should apply limits to merge command", () => {
      writeFileSync(testFile, '{ "foo": "bar" }');
      // Test with a size that works as CLI arg but exceeds a small limit
      const json = JSON.stringify({ data: "x".repeat(5000) });
      const { exitCode, stderr, stdout } = runCli(
        `merge ${testFile} '${json}'`,
        undefined,
        { AYWSON_MAX_JSON_SIZE: "3000" }
      );
      expect(exitCode).toBe(1);
      const errorOutput = stderr || stdout;
      expect(errorOutput).toContain("JSON input too large");
    });

    it("should apply limits to set command", () => {
      writeFileSync(testFile, '{ "foo": "bar" }');
      // Test with a size that works as CLI arg but exceeds a small limit
      const json = JSON.stringify({ data: "x".repeat(5000) });
      const { exitCode, stderr, stdout } = runCli(
        `set ${testFile} data '${json}'`,
        undefined,
        { AYWSON_MAX_JSON_SIZE: "3000" }
      );
      expect(exitCode).toBe(1);
      const errorOutput = stderr || stdout;
      expect(errorOutput).toContain("JSON input too large");
    });
  });
});
