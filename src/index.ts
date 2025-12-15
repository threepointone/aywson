import {
  applyEdits,
  findNodeAtLocation,
  getNodeValue,
  modify as jsoncModify,
  type Node,
  parseTree
} from "jsonc-parser";

// =============================================================================
// Types
// =============================================================================

export type JSONPath = (string | number)[];

// =============================================================================
// Shared Helpers
// =============================================================================

/**
 * Get all property keys from an object node in the JSON tree
 */
function getObjectKeys(node: Node | undefined): string[] {
  if (!node || node.type !== "object" || !node.children) {
    return [];
  }
  const keys: string[] = [];
  for (const child of node.children) {
    if (child.type === "property" && child.children && child.children[0]) {
      keys.push(child.children[0].value);
    }
  }
  return keys;
}

/**
 * Find the property node at a given path
 */
function findProperty(
  json: string,
  path: JSONPath
): { node: Node; propertyNode: Node; tree: Node } | null {
  const tree = parseTree(json);
  if (!tree) return null;

  const node = findNodeAtLocation(tree, path);
  if (!node || !node.parent) return null;

  return { node, propertyNode: node.parent, tree };
}

/**
 * Get the range of a property including its line context
 */
function getPropertyRange(
  json: string,
  propertyNode: Node
): { lineStart: number; isSingleLine: boolean } {
  let lineStart = propertyNode.offset;
  while (lineStart > 0 && json[lineStart - 1] !== "\n") {
    lineStart--;
  }

  const textBeforeOnLine = json.slice(lineStart, propertyNode.offset);
  const isSingleLine = textBeforeOnLine.includes("{");

  return { lineStart, isSingleLine };
}

/**
 * Look backwards from propertyOffset for a comment.
 * Returns the start position of the comment line if it should be deleted with the field.
 * Comments starting with "**" are preserved (returns null).
 */
function findAssociatedCommentStart(
  json: string,
  propertyOffset: number
): number | null {
  let pos = propertyOffset - 1;

  // Skip whitespace (spaces and tabs)
  while (pos >= 0 && (json[pos] === " " || json[pos] === "\t")) {
    pos--;
  }

  // Must have a newline before the property for there to be a comment above
  if (pos >= 0 && json[pos] === "\n") {
    pos--;
    if (pos >= 0 && json[pos] === "\r") {
      pos--;
    }
  } else {
    return null;
  }

  // Skip trailing whitespace on previous line
  while (pos >= 0 && (json[pos] === " " || json[pos] === "\t")) {
    pos--;
  }

  if (pos < 0) return null;

  // Check for block comment end */
  if (pos >= 1 && json[pos] === "/" && json[pos - 1] === "*") {
    let i = pos - 2;
    while (i >= 1) {
      if (json[i - 1] === "/" && json[i] === "*") {
        const commentContent = json.slice(i + 1, pos - 1).trim();
        // Comments starting with ** are preserved
        if (commentContent.startsWith("**")) {
          return null;
        }
        let commentLineStart = i - 1;
        while (commentLineStart > 0 && json[commentLineStart - 1] !== "\n") {
          commentLineStart--;
        }
        return commentLineStart;
      }
      i--;
    }
    return null;
  }

  // Check for line comment
  const lineEnd = pos;
  while (pos >= 0 && json[pos] !== "\n") {
    pos--;
  }
  const lineStart = pos + 1;

  const line = json.slice(lineStart, lineEnd + 1).trim();

  if (line.startsWith("//")) {
    const commentContent = line.slice(2).trim();
    // Comments starting with ** are preserved
    if (commentContent.startsWith("**")) {
      return null;
    }
    return lineStart;
  }

  return null;
}

/**
 * Find any comment above a property (not just associated ones)
 */
function findCommentAbove(
  json: string,
  propertyOffset: number
): { start: number; end: number; content: string } | null {
  let pos = propertyOffset - 1;

  // Skip whitespace
  while (pos >= 0 && (json[pos] === " " || json[pos] === "\t")) {
    pos--;
  }

  // Must have a newline
  if (pos >= 0 && json[pos] === "\n") {
    pos--;
    if (pos >= 0 && json[pos] === "\r") {
      pos--;
    }
  } else {
    return null;
  }

  // Skip trailing whitespace on previous line
  while (pos >= 0 && (json[pos] === " " || json[pos] === "\t")) {
    pos--;
  }

  if (pos < 0) return null;

  // Check for block comment end */
  if (pos >= 1 && json[pos] === "/" && json[pos - 1] === "*") {
    let i = pos - 2;
    while (i >= 1) {
      if (json[i - 1] === "/" && json[i] === "*") {
        const commentStart = i - 1;
        const commentEnd = pos + 1;
        const content = json.slice(commentStart + 2, commentEnd - 2).trim();
        return { start: commentStart, end: commentEnd, content };
      }
      i--;
    }
    return null;
  }

  // Check for line comment
  const lineEnd = pos;
  while (pos >= 0 && json[pos] !== "\n") {
    pos--;
  }
  const lineStart = pos + 1;

  const line = json.slice(lineStart, lineEnd + 1).trim();

  if (line.startsWith("//")) {
    const content = line.slice(2).trim();
    return { start: lineStart, end: lineEnd + 1, content };
  }

  return null;
}

/**
 * Flatten a nested changes object into individual path-value pairs
 */
function flattenChanges(
  obj: Record<string, unknown>,
  prefix: JSONPath = []
): Array<{ path: JSONPath; value: unknown }> {
  const result: Array<{ path: JSONPath; value: unknown }> = [];

  for (const [key, value] of Object.entries(obj)) {
    const currentPath = [...prefix, key];

    if (value === undefined) {
      result.push({ path: currentPath, value: undefined });
    } else if (
      value !== null &&
      typeof value === "object" &&
      !Array.isArray(value)
    ) {
      result.push(
        ...flattenChanges(value as Record<string, unknown>, currentPath)
      );
    } else {
      result.push({ path: currentPath, value });
    }
  }

  return result;
}

/**
 * Compute deletions for fields that exist in currentJson but not in changes (replace semantics)
 */
function computeDeletions(
  currentJson: string,
  changes: Record<string, unknown>,
  prefix: JSONPath = []
): Array<{ path: JSONPath; value: undefined }> {
  const result: Array<{ path: JSONPath; value: undefined }> = [];

  const tree = parseTree(currentJson);
  if (!tree) return result;

  const currentNode =
    prefix.length === 0 ? tree : findNodeAtLocation(tree, prefix);
  const currentKeys = getObjectKeys(currentNode);

  for (const key of currentKeys) {
    if (!(key in changes)) {
      result.push({ path: [...prefix, key], value: undefined });
    } else {
      const changeValue = changes[key];
      if (
        changeValue !== null &&
        changeValue !== undefined &&
        typeof changeValue === "object" &&
        !Array.isArray(changeValue)
      ) {
        result.push(
          ...computeDeletions(
            currentJson,
            changeValue as Record<string, unknown>,
            [...prefix, key]
          )
        );
      }
    }
  }

  return result;
}

// =============================================================================
// Parse Operations
// =============================================================================

/**
 * Parse a JSONC string into a JavaScript value.
 * Unlike JSON.parse(), this handles comments and trailing commas.
 */
export function parse<T = unknown>(json: string): T {
  const tree = parseTree(json);
  if (!tree) {
    throw new SyntaxError("Failed to parse JSON");
  }
  return getNodeValue(tree) as T;
}

// =============================================================================
// Path-based Operations
// =============================================================================

/**
 * Get the value at a path in the JSON
 */
export function get(json: string, path: JSONPath): unknown {
  const tree = parseTree(json);
  if (!tree) return undefined;

  const node = findNodeAtLocation(tree, path);
  if (!node) return undefined;

  return getNodeValue(node);
}

/**
 * Check if a path exists in the JSON
 */
export function has(json: string, path: JSONPath): boolean {
  const tree = parseTree(json);
  if (!tree) return false;

  const node = findNodeAtLocation(tree, path);
  return node !== undefined;
}

/**
 * Get the comment above a field at a path
 */
export function getComment(json: string, path: JSONPath): string | null {
  const found = findProperty(json, path);
  if (!found) return null;

  const { propertyNode } = found;
  const comment = findCommentAbove(json, propertyNode.offset);

  return comment ? comment.content : null;
}

/**
 * Set a value at a path in the JSON, optionally with a comment
 */
export function set(
  json: string,
  path: JSONPath,
  value: unknown,
  comment?: string
): string {
  const edits = jsoncModify(json, path, value, {});
  let result = applyEdits(json, edits);

  // Add comment if provided
  if (comment !== undefined) {
    result = setComment(result, path, comment);
  }

  return result;
}

/**
 * Remove a field at a path (with associated comment handling)
 */
export function remove(json: string, path: JSONPath): string {
  const found = findProperty(json, path);
  if (!found) return json;

  const { propertyNode } = found;
  const { lineStart, isSingleLine } = getPropertyRange(json, propertyNode);

  // Check for associated comment (comments starting with ** are preserved)
  const commentStart = !isSingleLine
    ? findAssociatedCommentStart(json, propertyNode.offset)
    : null;

  // Determine delete start
  let deleteStart: number;
  if (commentStart !== null) {
    deleteStart = commentStart;
  } else if (isSingleLine) {
    deleteStart = propertyNode.offset;
  } else {
    deleteStart = lineStart;
  }

  // Find the end of the property
  let deleteEnd = propertyNode.offset + propertyNode.length;

  // Skip trailing comma
  if (deleteEnd < json.length && json[deleteEnd] === ",") {
    deleteEnd++;
  }

  // Skip trailing whitespace
  while (
    deleteEnd < json.length &&
    (json[deleteEnd] === " " || json[deleteEnd] === "\t")
  ) {
    deleteEnd++;
  }

  // For multi-line, include the trailing newline
  if (!isSingleLine) {
    if (deleteEnd < json.length && json[deleteEnd] === "\n") {
      deleteEnd++;
    } else if (deleteEnd < json.length && json[deleteEnd] === "\r") {
      deleteEnd++;
      if (deleteEnd < json.length && json[deleteEnd] === "\n") {
        deleteEnd++;
      }
    }
  }

  const beforeDelete = json.slice(0, deleteStart);
  const afterDelete = json.slice(deleteEnd);

  let result = beforeDelete + afterDelete;

  // Handle trailing comma issues
  const trimmedBefore = beforeDelete.trimEnd();
  const afterTrimmed = afterDelete.trimStart();

  if (trimmedBefore.endsWith(",") && afterTrimmed.startsWith("}")) {
    const commaPos = beforeDelete.lastIndexOf(",");
    result =
      beforeDelete.slice(0, commaPos) +
      beforeDelete.slice(commaPos + 1) +
      afterDelete;
  }

  return result;
}

// =============================================================================
// Merge Strategies
// =============================================================================

/**
 * Merge changes into JSON (update/add only, never delete)
 */
export function merge(json: string, changes: Record<string, unknown>): string {
  let result = json;
  const flatChanges = flattenChanges(changes);

  for (const { path, value } of flatChanges) {
    if (value === undefined) {
      result = remove(result, path);
    } else {
      const edits = jsoncModify(result, path, value, {});
      result = applyEdits(result, edits);
    }
  }

  return result;
}

/**
 * Replace JSON with changes (delete fields not in changes)
 */
export function replace(
  json: string,
  changes: Record<string, unknown>
): string {
  let result = json;

  // Compute deletions for fields not in changes
  const deletions = computeDeletions(json, changes);
  const flatChanges = flattenChanges(changes);

  // Process deletions first (from deepest to shallowest)
  const sortedDeletions = deletions.sort(
    (a, b) => b.path.length - a.path.length
  );

  for (const { path } of sortedDeletions) {
    result = remove(result, path);
  }

  // Then apply updates
  for (const { path, value } of flatChanges) {
    if (value === undefined) {
      result = remove(result, path);
    } else {
      const edits = jsoncModify(result, path, value, {});
      result = applyEdits(result, edits);
    }
  }

  return result;
}

/**
 * Patch JSON with explicit deletes via undefined
 */
export function patch(json: string, changes: Record<string, unknown>): string {
  return merge(json, changes);
}

// =============================================================================
// Key Operations
// =============================================================================

/**
 * Rename a key while preserving its value and associated comment
 */
export function rename(json: string, path: JSONPath, newKey: string): string {
  const found = findProperty(json, path);
  if (!found) return json;

  const { node, propertyNode } = found;

  // Get the value
  const value = getNodeValue(node);

  // Check for associated comment
  const { isSingleLine } = getPropertyRange(json, propertyNode);
  const commentInfo = !isSingleLine
    ? findCommentAbove(json, propertyNode.offset)
    : null;

  // Build new path
  const newPath = [...path.slice(0, -1), newKey];

  // Remove old key
  let result = remove(json, path);

  // Add new key with the value
  result = set(result, newPath, value);

  // If there was a comment, preserve it
  if (commentInfo) {
    result = setComment(result, newPath, commentInfo.content);
  }

  return result;
}

/**
 * Move a field to a different location
 */
export function move(
  json: string,
  fromPath: JSONPath,
  toPath: JSONPath
): string {
  const found = findProperty(json, fromPath);
  if (!found) return json;

  const { node } = found;
  const value = getNodeValue(node);

  // Remove from old location
  let result = remove(json, fromPath);

  // Add to new location
  result = set(result, toPath, value);

  return result;
}

// =============================================================================
// Comment Operations
// =============================================================================

/**
 * Set or update a comment above a field
 */
export function setComment(
  json: string,
  path: JSONPath,
  comment: string
): string {
  const found = findProperty(json, path);
  if (!found) return json;

  const { propertyNode } = found;
  const { lineStart, isSingleLine } = getPropertyRange(json, propertyNode);

  if (isSingleLine) {
    // Can't add comment to single-line JSON
    return json;
  }

  // Check for existing comment
  const existingComment = findCommentAbove(json, propertyNode.offset);

  // Determine indentation
  const indentation = json.slice(lineStart, propertyNode.offset);
  const commentLine = `${indentation}// ${comment}\n`;

  if (existingComment) {
    // Find the line start of the existing comment
    let commentLineStart = existingComment.start;
    while (commentLineStart > 0 && json[commentLineStart - 1] !== "\n") {
      commentLineStart--;
    }
    // Find the end of the comment line (including newline)
    let commentLineEnd = existingComment.end;
    while (commentLineEnd < json.length && json[commentLineEnd] !== "\n") {
      commentLineEnd++;
    }
    if (commentLineEnd < json.length) {
      commentLineEnd++; // Include the newline
    }

    // Replace the existing comment
    return (
      json.slice(0, commentLineStart) + commentLine + json.slice(commentLineEnd)
    );
  } else {
    // Insert new comment before the property
    return json.slice(0, lineStart) + commentLine + json.slice(lineStart);
  }
}

/**
 * Remove the comment above a field
 */
export function removeComment(json: string, path: JSONPath): string {
  const found = findProperty(json, path);
  if (!found) return json;

  const { propertyNode } = found;
  const existingComment = findCommentAbove(json, propertyNode.offset);

  if (!existingComment) return json;

  // Find the full line range of the comment
  let commentLineStart = existingComment.start;
  while (commentLineStart > 0 && json[commentLineStart - 1] !== "\n") {
    commentLineStart--;
  }

  let commentLineEnd = existingComment.end;
  while (commentLineEnd < json.length && json[commentLineEnd] !== "\n") {
    commentLineEnd++;
  }
  if (commentLineEnd < json.length) {
    commentLineEnd++; // Include the newline
  }

  return json.slice(0, commentLineStart) + json.slice(commentLineEnd);
}

// =============================================================================
// Sort Operations
// =============================================================================

export interface SortOptions {
  /**
   * Custom comparator for sorting keys. Defaults to alphabetical.
   */
  comparator?: (a: string, b: string) => number;
  /**
   * Whether to recursively sort nested objects. Defaults to true.
   */
  deep?: boolean;
}

interface PropertyInfo {
  key: string;
  keyNode: Node;
  propertyNode: Node;
  /** Start of the property including any associated comment */
  fullStart: number;
  /** End of the property (before comma/whitespace) */
  propertyEnd: number;
  /** The indentation used for this property */
  indentation: string;
}

/**
 * Extract information about all properties in an object node
 */
function getPropertyInfos(json: string, objectNode: Node): PropertyInfo[] {
  if (!objectNode.children) return [];

  const infos: PropertyInfo[] = [];

  for (const child of objectNode.children) {
    if (child.type !== "property" || !child.children || !child.children[0]) {
      continue;
    }

    const keyNode = child.children[0];
    const key = keyNode.value as string;

    // Find line start for indentation
    let lineStart = child.offset;
    while (lineStart > 0 && json[lineStart - 1] !== "\n") {
      lineStart--;
    }
    const indentation = json.slice(lineStart, child.offset);

    // Check for associated comment above
    const commentStart = findAssociatedCommentStart(json, child.offset);
    const fullStart = commentStart !== null ? commentStart : lineStart;

    infos.push({
      key,
      keyNode,
      propertyNode: child,
      fullStart,
      propertyEnd: child.offset + child.length,
      indentation
    });
  }

  return infos;
}

/**
 * Sort object keys while preserving comments
 */
export function sort(
  json: string,
  path: JSONPath = [],
  options: SortOptions = {}
): string {
  const { comparator = (a, b) => a.localeCompare(b), deep = true } = options;

  const tree = parseTree(json);
  if (!tree) return json;

  // Find the target node at the given path
  const targetNode = path.length === 0 ? tree : findNodeAtLocation(tree, path);
  if (!targetNode || targetNode.type !== "object") return json;

  // Collect all objects that need sorting within the target (process deepest first)
  const objectsToSort: Array<{ node: Node; depth: number }> = [];

  function collectObjects(node: Node, depth: number): void {
    if (node.type === "object" && node.children && node.children.length > 0) {
      objectsToSort.push({ node, depth });
    }
    if (node.children) {
      for (const child of node.children) {
        collectObjects(child, depth + 1);
      }
    }
  }

  collectObjects(targetNode, 0);

  // Sort deepest objects first so offsets remain valid
  objectsToSort.sort((a, b) => b.depth - a.depth);

  // If not deep, only process the target object itself
  const objectsToProcess = deep
    ? objectsToSort
    : objectsToSort.filter((o) => o.depth === 0);

  let result = json;

  for (const { node: objectNode } of objectsToProcess) {
    result = sortSingleObject(result, objectNode, comparator);
    // Re-parse after each modification to get updated offsets
    if (objectsToProcess.length > 1) {
      const newTree = parseTree(result);
      if (!newTree) break;
    }
  }

  // If we processed multiple objects, we need to re-process with fresh offsets
  // since sorting one object invalidates offsets for others
  if (deep && objectsToSort.length > 1) {
    // Re-parse and process again from scratch with updated positions
    result = sortDeepAtPath(result, path, comparator);
  }

  return result;
}

/**
 * Recursively sort all objects in the JSON starting at a path
 */
function sortDeepAtPath(
  json: string,
  path: JSONPath,
  comparator: (a: string, b: string) => number
): string {
  const tree = parseTree(json);
  if (!tree) return json;

  const targetNode = path.length === 0 ? tree : findNodeAtLocation(tree, path);
  if (!targetNode || targetNode.type !== "object") return json;

  // Find the deepest object and sort it, repeat until no changes
  let result = json;
  let changed = true;

  while (changed) {
    changed = false;
    const currentTree = parseTree(result);
    if (!currentTree) break;

    const currentTarget =
      path.length === 0 ? currentTree : findNodeAtLocation(currentTree, path);
    if (!currentTarget) break;

    // Find deepest object within target
    let deepestObject: Node | null = null;
    let maxDepth = -1;

    function findDeepest(node: Node, depth: number): void {
      if (node.type === "object" && node.children && node.children.length > 1) {
        // Check if this object needs sorting
        const infos = getPropertyInfos(result, node);
        const keys = infos.map((i) => i.key);
        const sortedKeys = [...keys].sort(comparator);
        const needsSort = keys.some((k, i) => k !== sortedKeys[i]);

        if (needsSort && depth > maxDepth) {
          deepestObject = node;
          maxDepth = depth;
        }
      }
      if (node.children) {
        for (const child of node.children) {
          findDeepest(child, depth + 1);
        }
      }
    }

    findDeepest(currentTarget, 0);

    if (deepestObject) {
      result = sortSingleObject(result, deepestObject, comparator);
      changed = true;
    }
  }

  return result;
}

/**
 * Sort a single object's keys
 */
function sortSingleObject(
  json: string,
  objectNode: Node,
  comparator: (a: string, b: string) => number
): string {
  const infos = getPropertyInfos(json, objectNode);

  if (infos.length <= 1) return json;

  // Check if already sorted
  const keys = infos.map((i) => i.key);
  const sortedKeys = [...keys].sort(comparator);
  if (keys.every((k, i) => k === sortedKeys[i])) {
    return json;
  }

  // Sort the property infos
  const sortedInfos = [...infos].sort((a, b) => comparator(a.key, b.key));

  // Find the object's content boundaries (inside the braces)
  const objectStart = objectNode.offset;
  const objectEnd = objectNode.offset + objectNode.length;

  // Find where content starts (after opening brace and any whitespace)
  let contentStart = objectStart + 1;
  while (
    contentStart < objectEnd &&
    (json[contentStart] === " " ||
      json[contentStart] === "\t" ||
      json[contentStart] === "\n" ||
      json[contentStart] === "\r")
  ) {
    contentStart++;
  }

  // Find where content ends (before closing brace)
  let contentEnd = objectEnd - 1;
  while (
    contentEnd > contentStart &&
    (json[contentEnd - 1] === " " ||
      json[contentEnd - 1] === "\t" ||
      json[contentEnd - 1] === "\n" ||
      json[contentEnd - 1] === "\r")
  ) {
    contentEnd--;
  }

  // Check if it's a single-line object
  const objectContent = json.slice(objectStart, objectEnd);
  const isSingleLine = !objectContent.includes("\n");

  // Detect if original has trailing comma (check after last property)
  const lastInfo = infos[infos.length - 1];
  let hasTrailingComma = false;
  if (lastInfo) {
    let pos = lastInfo.propertyEnd;
    while (pos < objectEnd && (json[pos] === " " || json[pos] === "\t")) {
      pos++;
    }
    hasTrailingComma = pos < objectEnd && json[pos] === ",";
  }

  if (isSingleLine) {
    // For single-line objects, rebuild simply
    const sortedPairs = sortedInfos.map((info) => {
      const valueNode = info.propertyNode.children?.[1];
      const valueStr = valueNode
        ? json.slice(valueNode.offset, valueNode.offset + valueNode.length)
        : "null";
      return `"${info.key}": ${valueStr}`;
    });
    const trailingComma = hasTrailingComma ? "," : "";
    return (
      json.slice(0, objectStart) +
      "{ " +
      sortedPairs.join(", ") +
      trailingComma +
      " }" +
      json.slice(objectEnd)
    );
  }

  // For multi-line objects, preserve formatting
  // Extract each property with its full content (including comment)
  const propertyTexts: string[] = [];

  for (const sortedInfo of sortedInfos) {
    // Find the original info to get proper text boundaries
    const originalInfo = infos.find((x) => x.key === sortedInfo.key);
    if (!originalInfo) continue;

    // Determine the end boundary for this property's text
    let textEnd = originalInfo.propertyEnd;

    // Find where the text for this property ends (including trailing content but not next property)
    // Look for comma and skip it
    let pos = textEnd;
    while (pos < json.length && (json[pos] === " " || json[pos] === "\t")) {
      pos++;
    }
    if (pos < json.length && json[pos] === ",") {
      pos++; // skip comma
    }
    // Skip trailing whitespace on the same line
    while (pos < json.length && (json[pos] === " " || json[pos] === "\t")) {
      pos++;
    }
    // Include newline if present
    if (pos < json.length && json[pos] === "\n") {
      textEnd = pos + 1;
    } else if (pos < json.length && json[pos] === "\r") {
      textEnd = pos + 1;
      if (textEnd < json.length && json[textEnd] === "\n") {
        textEnd++;
      }
    } else {
      textEnd = pos;
    }

    // Get the full text including comment
    let propText = json.slice(originalInfo.fullStart, textEnd);

    // If this property had a comment, we need to preserve it
    if (originalInfo.fullStart < originalInfo.propertyNode.offset) {
      // There's a comment before this property
      const commentText = json.slice(
        originalInfo.fullStart,
        originalInfo.propertyNode.offset
      );

      // Get just the property part
      const propOnlyText = json.slice(
        originalInfo.propertyNode.offset,
        originalInfo.propertyEnd
      );

      // Reconstruct with consistent indentation
      propText = commentText + propOnlyText;
    } else {
      // No comment, just the property
      propText =
        sortedInfo.indentation +
        json.slice(originalInfo.propertyNode.offset, originalInfo.propertyEnd);
    }

    // Remove any trailing comma from the property text
    propText = propText.replace(/,\s*$/, "");

    propertyTexts.push(propText);
  }

  // Find closing brace indentation
  const closingBracePos = objectEnd - 1;
  let closingIndentStart = closingBracePos;
  while (closingIndentStart > 0 && json[closingIndentStart - 1] !== "\n") {
    closingIndentStart--;
  }
  const closingIndent = json.slice(closingIndentStart, closingBracePos);

  // Build the new object content
  const newContent = propertyTexts
    .map((text, i) => {
      // Add comma except for last item (unless trailing comma is preserved)
      const isLast = i === propertyTexts.length - 1;
      if (isLast) {
        return hasTrailingComma ? `${text},` : text;
      }
      return `${text},`;
    })
    .join("\n");

  // Reconstruct the object
  const beforeObject = json.slice(0, objectStart);
  const afterObject = json.slice(objectEnd);

  return `${beforeObject}{\n${newContent}\n${closingIndent}}${afterObject}`;
}

// =============================================================================
// High-level API
// =============================================================================

/**
 * Modify JSON with replace semantics — fields not in changes are deleted
 */
export function modify(json: string, changes: Record<string, unknown>): string {
  return replace(json, changes);
}
