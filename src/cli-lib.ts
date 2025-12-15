import { readFileSync, writeFileSync } from "node:fs";
import chalk from "chalk";
import {
  get,
  getComment,
  merge,
  modify,
  move,
  parse,
  remove,
  removeComment,
  rename,
  set,
  setComment,
  sort
} from "./index.js";

// =============================================================================
// Path Parsing
// =============================================================================

/**
 * Parse a dot-notation path string into a JSONPath array
 * Examples:
 *   "config.database.host" -> ["config", "database", "host"]
 *   "items.0" -> ["items", 0]
 *   "items[0]" -> ["items", 0]
 */
export function parsePath(pathStr: string): (string | number)[] {
  if (!pathStr) return [];

  const result: (string | number)[] = [];
  let current = "";
  let i = 0;

  while (i < pathStr.length) {
    const char = pathStr[i];

    if (char === ".") {
      if (current) {
        result.push(parseSegment(current));
        current = "";
      }
      i++;
    } else if (char === "[") {
      if (current) {
        result.push(parseSegment(current));
        current = "";
      }
      // Find closing bracket
      const closeBracket = pathStr.indexOf("]", i);
      if (closeBracket === -1) {
        throw new Error(`Unclosed bracket in path: ${pathStr}`);
      }
      const indexStr = pathStr.slice(i + 1, closeBracket);
      result.push(parseSegment(indexStr));
      i = closeBracket + 1;
    } else {
      current += char;
      i++;
    }
  }

  if (current) {
    result.push(parseSegment(current));
  }

  return result;
}

function parseSegment(segment: string): string | number {
  const num = Number(segment);
  if (!Number.isNaN(num) && Number.isInteger(num) && num >= 0) {
    return num;
  }
  return segment;
}

// =============================================================================
// Diff Display
// =============================================================================

/**
 * Generate a simple line-by-line diff between two strings
 */
export function diff(oldStr: string, newStr: string): string {
  const oldLines = oldStr.split("\n");
  const newLines = newStr.split("\n");

  // Simple LCS-based diff
  const result: string[] = [];
  let oldIdx = 0;
  let newIdx = 0;

  while (oldIdx < oldLines.length || newIdx < newLines.length) {
    const oldLine = oldLines[oldIdx];
    const newLine = newLines[newIdx];

    if (oldLine === undefined) {
      // Only new lines left
      result.push(chalk.green(`+ ${newLine}`));
      newIdx++;
    } else if (newLine === undefined) {
      // Only old lines left
      result.push(chalk.red(`- ${oldLine}`));
      oldIdx++;
    } else if (oldLine === newLine) {
      // Lines match
      result.push(chalk.gray(`  ${oldLine}`));
      oldIdx++;
      newIdx++;
    } else {
      // Lines differ - look ahead to find matches
      const oldInNew = newLines.indexOf(oldLine, newIdx);
      const newInOld = oldLines.indexOf(newLine, oldIdx);

      if (oldInNew === -1 && newInOld === -1) {
        // Neither line appears later, show as change
        result.push(chalk.red(`- ${oldLine}`));
        result.push(chalk.green(`+ ${newLine}`));
        oldIdx++;
        newIdx++;
      } else if (oldInNew === -1 || (newInOld !== -1 && newInOld < oldInNew)) {
        // Old line appears in new, so new line was added
        result.push(chalk.red(`- ${oldLine}`));
        oldIdx++;
      } else {
        // New line appears in old, so old line was removed
        result.push(chalk.green(`+ ${newLine}`));
        newIdx++;
      }
    }
  }

  return result.join("\n");
}

// =============================================================================
// CLI
// =============================================================================

function printHelp(): void {
  console.log(`
aywson - Modify JSONC while preserving comments and formatting

Usage:
  aywson <command> [options] <file> [args...]

Commands:
  parse <file>                        Parse JSONC and output as JSON
  get <file> <path>                   Get value at path
  set <file> <path> <value>           Set value at path
  remove <file> <path>                Remove field at path
  modify <file> <json>                Replace semantics (delete unmentioned fields)
  merge <file> <json>                 Merge without deleting
  rename <file> <path> <newKey>       Rename a key
  move <file> <fromPath> <toPath>     Move field to new location
  sort <file> [path]                  Sort object keys alphabetically
  comment <file> <path> [text]        Get or set comment above a field
  uncomment <file> <path>             Remove comment above a field

Options:
  --dry-run, -n    Show diff but don't write to file
  --no-deep        For sort: only sort specified object, not nested
  --help, -h       Show this help
  --version, -v    Show version

Path Syntax:
  Use dot-notation: config.database.host
  Array indices: items.0 or items[0]

Examples:
  aywson parse config.jsonc
  aywson get config.json database.host
  aywson set config.json database.port 5433
  aywson set --dry-run config.json database.port 5433
  aywson modify config.json '{"database": {"host": "prod.db.com"}}'
  aywson sort config.json
  aywson sort config.json dependencies --no-deep
`);
}

function printVersion(): void {
  // Read version from package.json
  try {
    const pkg = JSON.parse(
      readFileSync(new URL("../package.json", import.meta.url), "utf-8")
    );
    console.log(pkg.version);
  } catch {
    console.log("unknown");
  }
}

interface ParsedArgs {
  command: string;
  file: string;
  args: string[];
  dryRun: boolean;
  noDeep: boolean;
}

function parseArgs(argv: string[]): ParsedArgs | null {
  const args = argv.slice(2); // Skip node and script path

  // Check for help/version flags
  if (args.includes("--help") || args.includes("-h")) {
    printHelp();
    return null;
  }

  if (args.includes("--version") || args.includes("-v")) {
    printVersion();
    return null;
  }

  // Filter out flags
  const dryRun = args.includes("--dry-run") || args.includes("-n");
  const noDeep = args.includes("--no-deep");
  const positional = args.filter((a) => !a.startsWith("-") || a === "-");

  const command = positional[0];
  const file = positional[1];

  if (!command || !file) {
    console.error("Error: Missing command or file argument");
    console.error("Run 'aywson --help' for usage");
    process.exit(1);
  }

  return { command, file, args: positional.slice(2), dryRun, noDeep };
}

function readInput(file: string): string {
  if (file === "-") {
    // Read from stdin
    return readFileSync(0, "utf-8");
  }
  return readFileSync(file, "utf-8");
}

function writeOutput(file: string, content: string): void {
  if (file === "-") {
    process.stdout.write(content);
  } else {
    writeFileSync(file, content);
  }
}

export function run(): void {
  const parsed = parseArgs(process.argv);
  if (!parsed) return;

  const { command, file, args, dryRun, noDeep } = parsed;

  try {
    const json = readInput(file);

    switch (command) {
      case "parse": {
        const value = parse(json);
        console.log(JSON.stringify(value, null, 2));
        break;
      }

      case "get": {
        const pathArg = args[0];
        if (!pathArg) {
          console.error("Error: get requires a path argument");
          process.exit(1);
        }
        const path = parsePath(pathArg);
        const value = get(json, path);
        console.log(JSON.stringify(value, null, 2));
        break;
      }

      case "set": {
        const pathArg = args[0];
        const valueArg = args[1];
        if (!pathArg || !valueArg) {
          console.error("Error: set requires path and value arguments");
          process.exit(1);
        }
        const path = parsePath(pathArg);
        const value = JSON.parse(valueArg);
        const result = set(json, path, value);
        handleMutation(file, json, result, dryRun);
        break;
      }

      case "remove": {
        const pathArg = args[0];
        if (!pathArg) {
          console.error("Error: remove requires a path argument");
          process.exit(1);
        }
        const path = parsePath(pathArg);
        const result = remove(json, path);
        handleMutation(file, json, result, dryRun);
        break;
      }

      case "modify": {
        const changesArg = args[0];
        if (!changesArg) {
          console.error("Error: modify requires a JSON argument");
          process.exit(1);
        }
        const changes = JSON.parse(changesArg);
        const result = modify(json, changes);
        handleMutation(file, json, result, dryRun);
        break;
      }

      case "merge": {
        const changesArg = args[0];
        if (!changesArg) {
          console.error("Error: merge requires a JSON argument");
          process.exit(1);
        }
        const changes = JSON.parse(changesArg);
        const result = merge(json, changes);
        handleMutation(file, json, result, dryRun);
        break;
      }

      case "rename": {
        const pathArg = args[0];
        const newKeyArg = args[1];
        if (!pathArg || !newKeyArg) {
          console.error("Error: rename requires path and newKey arguments");
          process.exit(1);
        }
        const path = parsePath(pathArg);
        const result = rename(json, path, newKeyArg);
        handleMutation(file, json, result, dryRun);
        break;
      }

      case "move": {
        const fromArg = args[0];
        const toArg = args[1];
        if (!fromArg || !toArg) {
          console.error("Error: move requires fromPath and toPath arguments");
          process.exit(1);
        }
        const fromPath = parsePath(fromArg);
        const toPath = parsePath(toArg);
        const result = move(json, fromPath, toPath);
        handleMutation(file, json, result, dryRun);
        break;
      }

      case "sort": {
        const pathArg = args[0];
        const path = pathArg ? parsePath(pathArg) : [];
        const result = sort(json, path, { deep: !noDeep });
        handleMutation(file, json, result, dryRun);
        break;
      }

      case "comment": {
        const pathArg = args[0];
        const textArg = args[1];
        if (!pathArg) {
          console.error("Error: comment requires a path argument");
          process.exit(1);
        }
        const path = parsePath(pathArg);
        if (textArg === undefined) {
          // Get comment
          const comment = getComment(json, path);
          if (comment === null) {
            console.log("(no comment)");
          } else {
            console.log(comment);
          }
        } else {
          // Set comment
          const result = setComment(json, path, textArg);
          handleMutation(file, json, result, dryRun);
        }
        break;
      }

      case "uncomment": {
        const pathArg = args[0];
        if (!pathArg) {
          console.error("Error: uncomment requires a path argument");
          process.exit(1);
        }
        const path = parsePath(pathArg);
        const result = removeComment(json, path);
        handleMutation(file, json, result, dryRun);
        break;
      }

      default:
        console.error(`Error: Unknown command '${command}'`);
        console.error("Run 'aywson --help' for usage");
        process.exit(1);
    }
  } catch (error) {
    if (error instanceof Error) {
      console.error(`Error: ${error.message}`);
    } else {
      console.error("An unknown error occurred");
    }
    process.exit(1);
  }
}

function handleMutation(
  file: string,
  original: string,
  result: string,
  dryRun: boolean
): void {
  // Always show the diff
  if (original !== result) {
    console.log(diff(original, result));
  } else {
    console.log("No changes");
  }

  // Write unless dry-run
  if (!dryRun && file !== "-") {
    writeOutput(file, result);
  } else if (dryRun) {
    console.log("\n(dry-run: file not modified)");
  }
}
