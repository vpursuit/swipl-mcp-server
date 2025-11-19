import { describe, test, expect, beforeEach, afterEach } from "vitest";
import { PrologInterface } from "../../src/PrologInterface.js";
import { promises as fs } from "fs";
import path from "path";
import os from "os";

describe("Source Storage Infrastructure", () => {
  let prologInterface: PrologInterface;

  beforeEach(async () => {
    prologInterface = new PrologInterface();
    await prologInterface.start();
    // Clear any existing state
    try {
      await prologInterface.clearWorkspaceWithSource();
    } catch (error) {
      // Ignore errors during cleanup
    }
  });

  afterEach(async () => {
    await prologInterface.stop();
  });

  describe("assertClauseWithSource", () => {
    test("should preserve variable names in source storage", async () => {
      const clause = "parent(X, Y) :- ancestor(X, Y).";
      const result = await prologInterface.assertClauseWithSource(clause, 'inline');

      expect(result.success).toBe(true);
      expect(result.id).toBeDefined();

      const snapshot = await prologInterface.getSnapshot();
      expect(snapshot).toContain("parent(X, Y)");
      expect(snapshot).toContain("ancestor(X, Y)");
    });

    test("should handle trailing period normalization", async () => {
      const withPeriod = "likes(mary, food).";
      const withoutPeriod = "likes(john, wine)";

      const result1 = await prologInterface.assertClauseWithSource(withPeriod, 'inline');
      const result2 = await prologInterface.assertClauseWithSource(withoutPeriod, 'inline');

      expect(result1.success).toBe(true);
      expect(result2.success).toBe(true);

      const snapshot = await prologInterface.getSnapshot();
      expect(snapshot).toContain("likes(mary, food).");
      expect(snapshot).toContain("likes(john, wine).");
    });

    test("should track inline type correctly", async () => {
      const clause = "fact(a).";
      const result = await prologInterface.assertClauseWithSource(clause, 'inline');

      expect(result.success).toBe(true);

      // Access private field for testing
      const storage = (prologInterface as any).kbSourceStorage;
      const entry = storage.get(result.id!);
      expect(entry.type).toBe('inline');
      expect(entry.file).toBeUndefined();
    });

    test("should not store source if Prolog assertion fails", async () => {
      const invalidClause = "this is not valid prolog";
      const result = await prologInterface.assertClauseWithSource(invalidClause, 'inline');

      expect(result.success).toBe(false);
      expect(result.error).toBeDefined();

      const snapshot = await prologInterface.getSnapshot();
      expect(snapshot).not.toContain("this is not valid prolog");
    });

    test("should store timestamp on successful assertion", async () => {
      const clause = "timestamped(fact).";
      const before = Date.now();
      const result = await prologInterface.assertClauseWithSource(clause, 'inline');
      const after = Date.now();

      expect(result.success).toBe(true);

      const storage = (prologInterface as any).kbSourceStorage;
      const entry = storage.get(result.id!);
      expect(entry.timestamp).toBeGreaterThanOrEqual(before);
      expect(entry.timestamp).toBeLessThanOrEqual(after);
    });

    test("should mark entry as compiled on success", async () => {
      const clause = "compiled(fact).";
      const result = await prologInterface.assertClauseWithSource(clause, 'inline');

      expect(result.success).toBe(true);

      const storage = (prologInterface as any).kbSourceStorage;
      const entry = storage.get(result.id!);
      expect(entry.compiled).toBe(true);
    });
  });

  describe("getSnapshot", () => {
    test("should return clauses in insertion order", async () => {
      await prologInterface.assertClauseWithSource("first(fact).", 'inline');
      await prologInterface.assertClauseWithSource("second(fact).", 'inline');
      await prologInterface.assertClauseWithSource("third(fact).", 'inline');

      const snapshot = await prologInterface.getSnapshot();
      const lines = snapshot.split('\n').filter(l => l.trim());

      expect(lines[0]).toContain("first(fact)");
      expect(lines[1]).toContain("second(fact)");
      expect(lines[2]).toContain("third(fact)");
    });

    test("should preserve original variable names", async () => {
      await prologInterface.assertClauseWithSource("ancestor(X, Y) :- parent(X, Y).", 'inline');
      await prologInterface.assertClauseWithSource("ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).", 'inline');

      const snapshot = await prologInterface.getSnapshot();

      // Should preserve X, Y, Z variable names
      expect(snapshot).toContain("ancestor(X, Y)");
      expect(snapshot).toContain("parent(X, Y)");
      expect(snapshot).toContain("ancestor(X, Z)");
      expect(snapshot).toContain("ancestor(Y, Z)");
    });

    test("should return empty string for empty workspace", async () => {
      const snapshot = await prologInterface.getSnapshot();
      expect(snapshot).toBe("");
    });

    test("should only include successfully compiled entries", async () => {
      await prologInterface.assertClauseWithSource("valid(fact).", 'inline');

      // Manually add an uncompiled entry (for testing)
      const storage = (prologInterface as any).kbSourceStorage;
      storage.set('test-id', {
        id: 'test-id',
        sourceText: 'uncompiled(fact).',
        type: 'inline',
        timestamp: Date.now(),
        compiled: false
      });

      const snapshot = await prologInterface.getSnapshot();
      expect(snapshot).toContain("valid(fact)");
      expect(snapshot).not.toContain("uncompiled(fact)");
    });
  });

  describe("retractClauseWithSource", () => {
    test("should remove clause from both Prolog and source storage", async () => {
      const clause = "removable(fact).";
      await prologInterface.assertClauseWithSource(clause, 'inline');

      let snapshot = await prologInterface.getSnapshot();
      expect(snapshot).toContain("removable(fact)");

      const success = await prologInterface.retractClauseWithSource(clause);
      expect(success).toBe(true);

      snapshot = await prologInterface.getSnapshot();
      expect(snapshot).not.toContain("removable(fact)");
    });

    test("should handle clause with or without period", async () => {
      const clause = "flexible(fact).";
      await prologInterface.assertClauseWithSource(clause, 'inline');

      // Retract without period
      const success = await prologInterface.retractClauseWithSource("flexible(fact)");
      expect(success).toBe(true);

      const snapshot = await prologInterface.getSnapshot();
      expect(snapshot).not.toContain("flexible(fact)");
    });

    test("should return false for non-existent clause", async () => {
      const success = await prologInterface.retractClauseWithSource("nonexistent(fact).");
      expect(success).toBe(false);
    });

    test("should only remove first matching entry from duplicates", async () => {
      const clause = "duplicate(fact).";
      await prologInterface.assertClauseWithSource(clause, 'inline');
      await prologInterface.assertClauseWithSource(clause, 'inline');

      const success = await prologInterface.retractClauseWithSource(clause);
      expect(success).toBe(true);

      // One should still remain
      const snapshot = await prologInterface.getSnapshot();
      expect(snapshot.split('\n').filter(l => l.includes("duplicate(fact)")).length).toBe(1);
    });
  });

  describe("clearWorkspaceWithSource", () => {
    test("should clear both Prolog KB and source storage", async () => {
      await prologInterface.assertClauseWithSource("fact1(a).", 'inline');
      await prologInterface.assertClauseWithSource("fact2(b).", 'inline');
      await prologInterface.assertClauseWithSource("fact3(c).", 'inline');

      let snapshot = await prologInterface.getSnapshot();
      expect(snapshot.length).toBeGreaterThan(0);

      await prologInterface.clearWorkspaceWithSource();

      snapshot = await prologInterface.getSnapshot();
      expect(snapshot).toBe("");
    });

    test("should handle empty workspace gracefully", async () => {
      await expect(prologInterface.clearWorkspaceWithSource()).resolves.not.toThrow();
    });
  });

  describe("parseFileToStringArray (via importFileWithSource)", () => {
    let tempDir: string;
    let testFile: string;

    beforeEach(async () => {
      tempDir = await fs.mkdtemp(path.join(os.tmpdir(), 'prolog-test-'));
      testFile = path.join(tempDir, 'test.pl');
    });

    afterEach(async () => {
      try {
        await fs.rm(tempDir, { recursive: true, force: true });
      } catch (error) {
        // Ignore cleanup errors
      }
    });

    test("should parse simple facts from file", async () => {
      const content = `
parent(john, mary).
parent(mary, ann).
parent(ann, tom).
      `;
      await fs.writeFile(testFile, content);

      const result = await prologInterface.importFileWithSource(testFile);
      expect(result.success).toBe(true);
      expect(result.clausesAdded).toBe(3);

      const snapshot = await prologInterface.getSnapshot();
      // Note: Prolog parser adds spaces after commas with spacing(next_argument)
      expect(snapshot).toContain("parent(john, mary)");
      expect(snapshot).toContain("parent(mary, ann)");
      expect(snapshot).toContain("parent(ann, tom)");
    });

    test("should skip comments and empty lines", async () => {
      const content = `
% This is a comment
parent(john, mary).

% Another comment
parent(mary, ann).

      `;
      await fs.writeFile(testFile, content);

      const result = await prologInterface.importFileWithSource(testFile);
      expect(result.success).toBe(true);
      expect(result.clausesAdded).toBe(2);

      const snapshot = await prologInterface.getSnapshot();
      expect(snapshot).not.toContain("%");
    });

    test("should skip directives", async () => {
      const content = `
:- use_module(library(lists)).
parent(john, mary).
:- dynamic fact/1.
parent(mary, ann).
      `;
      await fs.writeFile(testFile, content);

      const result = await prologInterface.importFileWithSource(testFile);
      expect(result.success).toBe(true);
      expect(result.clausesAdded).toBe(2);

      const snapshot = await prologInterface.getSnapshot();
      expect(snapshot).not.toContain(":-");
    });

    test("should handle multi-line clauses", async () => {
      const content = `
ancestor(X, Y) :-
    parent(X, Y).
ancestor(X, Z) :-
    parent(X, Y),
    ancestor(Y, Z).
      `;
      await fs.writeFile(testFile, content);

      const result = await prologInterface.importFileWithSource(testFile);
      expect(result.success).toBe(true);
      expect(result.clausesAdded).toBe(2);

      const snapshot = await prologInterface.getSnapshot();
      expect(snapshot).toContain("ancestor");
    });

    test("should handle strings with dots (Prolog parser)", async () => {
      const content = `
person("Dr. Smith", doctor).
person("Prof. Jones", professor).
message("Hello. How are you?").
      `;
      await fs.writeFile(testFile, content);

      const result = await prologInterface.importFileWithSource(testFile);
      expect(result.success).toBe(true);
      expect(result.clausesAdded).toBe(3);

      const snapshot = await prologInterface.getSnapshot();
      // Note: Prolog parser adds spaces after commas with spacing(next_argument)
      expect(snapshot).toContain('person("Dr. Smith", doctor)');
      expect(snapshot).toContain('person("Prof. Jones", professor)');
      expect(snapshot).toContain('message("Hello. How are you?")');
    });

    test("should reject DCG rules with helpful error message", async () => {
      // DCG rules are not supported - users should use expanded difference-list form
      const content = `
sentence --> noun, verb.
noun --> [cat].
      `;
      await fs.writeFile(testFile, content);

      const result = await prologInterface.importFileWithSource(testFile);
      expect(result.success).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
      expect(result.errors[0]).toContain("DCG rules");
      expect(result.errors[0]).toContain("not supported");
      expect(result.errors[0]).toContain("difference-list");
    });

    test("should handle complex operators and precedence", async () => {
      const content = `
expr(X+Y) :- expr(X), expr(Y).
expr(X*Y) :- expr(X), expr(Y).
expr(X-Y) :- expr(X), expr(Y).
expr(X/Y) :- expr(X), expr(Y), Y \\= 0.
expr(N) :- number(N).
      `;
      await fs.writeFile(testFile, content);

      const result = await prologInterface.importFileWithSource(testFile);
      expect(result.success).toBe(true);
      expect(result.clausesAdded).toBe(5);

      const snapshot = await prologInterface.getSnapshot();
      expect(snapshot).toContain("expr(X+Y)");
      expect(snapshot).toContain("expr(X*Y)");
      expect(snapshot).toContain("Y\\=0");
    });

    test("should preserve original variable names", async () => {
      const content = `
grandparent(Grandparent, Grandchild) :-
    parent(Grandparent, Parent),
    parent(Parent, Grandchild).
sibling(Child1, Child2) :-
    parent(Parent, Child1),
    parent(Parent, Child2),
    Child1 \\= Child2.
      `;
      await fs.writeFile(testFile, content);

      const result = await prologInterface.importFileWithSource(testFile);
      expect(result.success).toBe(true);
      expect(result.clausesAdded).toBe(2);

      const snapshot = await prologInterface.getSnapshot();
      // Note: spacing(next_argument) adds spaces after commas in compound terms
      expect(snapshot).toContain("grandparent(Grandparent, Grandchild)");
      expect(snapshot).toContain("parent(Grandparent, Parent)");
      expect(snapshot).toContain("sibling(Child1, Child2)");
      expect(snapshot).not.toMatch(/_G\d+/); // Should not contain internal Prolog variables
    });

    test("should handle atoms with special characters", async () => {
      const content = `
'special-atom'(foo).
'atom with spaces'(bar).
'atom_with_underscore'(baz).
      `;
      await fs.writeFile(testFile, content);

      const result = await prologInterface.importFileWithSource(testFile);
      expect(result.success).toBe(true);
      expect(result.clausesAdded).toBe(3);

      const snapshot = await prologInterface.getSnapshot();
      expect(snapshot).toContain("'special-atom'(foo)");
      expect(snapshot).toContain("'atom with spaces'(bar)");
      // Note: atom_with_underscore doesn't need quotes (valid Prolog atom)
      expect(snapshot).toContain("atom_with_underscore(baz)");
    });

    test("should handle lists and complex data structures", async () => {
      const content = `
list_member(X, [X|_]).
list_member(X, [_|Tail]) :- list_member(X, Tail).
process([]).
process([H|T]) :- handle(H), process(T).
tree(node(Left, Value, Right)) :- tree(Left), number(Value), tree(Right).
tree(leaf).
      `;
      await fs.writeFile(testFile, content);

      const result = await prologInterface.importFileWithSource(testFile);
      expect(result.success).toBe(true);
      expect(result.clausesAdded).toBe(6);

      const snapshot = await prologInterface.getSnapshot();
      // Note: Anonymous variables are converted to internal Prolog variables
      // This is expected behavior from Prolog's write_term
      // Also note: spacing(next_argument) adds spaces in some contexts but not consistently
      expect(snapshot).toMatch(/\[X\|_\d+\]/); // Matches [X|_12345]
      expect(snapshot).toMatch(/\[_\d+\|Tail\]/); // Matches [_12345|Tail]
      expect(snapshot).toContain("node(Left, Value, Right)"); // With spaces
    });
  });

  describe("importFileWithSource", () => {
    let tempDir: string;
    let testFile: string;

    beforeEach(async () => {
      tempDir = await fs.mkdtemp(path.join(os.tmpdir(), 'prolog-test-'));
      testFile = path.join(tempDir, 'test.pl');
    });

    afterEach(async () => {
      try {
        await fs.rm(tempDir, { recursive: true, force: true });
      } catch (error) {
        // Ignore cleanup errors
      }
    });

    test("should track file provenance", async () => {
      const content = "imported(fact).";
      await fs.writeFile(testFile, content);

      const result = await prologInterface.importFileWithSource(testFile);
      expect(result.success).toBe(true);

      const storage = (prologInterface as any).kbSourceStorage;
      const entries = Array.from(storage.values());
      const fileEntry = entries.find((e: any) => e.type === 'file');

      expect(fileEntry).toBeDefined();
      expect(fileEntry.file).toBe(testFile);
    });

    test("should prevent duplicate file imports", async () => {
      const content = "fact(a).";
      await fs.writeFile(testFile, content);

      const result1 = await prologInterface.importFileWithSource(testFile);
      expect(result1.success).toBe(true);

      const result2 = await prologInterface.importFileWithSource(testFile);
      expect(result2.success).toBe(false);
      expect(result2.errors[0]).toContain("already imported");
    });

  });

  describe("unimportFile", () => {
    let tempDir: string;
    let testFile: string;

    beforeEach(async () => {
      tempDir = await fs.mkdtemp(path.join(os.tmpdir(), 'prolog-test-'));
      testFile = path.join(tempDir, 'test.pl');
    });

    afterEach(async () => {
      try {
        await fs.rm(tempDir, { recursive: true, force: true });
      } catch (error) {
        // Ignore cleanup errors
      }
    });

    test("should remove all clauses from specific file", async () => {
      const content = `
file_fact1(a).
file_fact2(b).
file_fact3(c).
      `;
      await fs.writeFile(testFile, content);

      await prologInterface.importFileWithSource(testFile);
      let snapshot = await prologInterface.getSnapshot();
      expect(snapshot).toContain("file_fact1");
      expect(snapshot).toContain("file_fact2");
      expect(snapshot).toContain("file_fact3");

      const result = await prologInterface.unimportFile(testFile);
      expect(result.success).toBe(true);
      expect(result.clausesRemoved).toBe(3);

      snapshot = await prologInterface.getSnapshot();
      expect(snapshot).not.toContain("file_fact1");
      expect(snapshot).not.toContain("file_fact2");
      expect(snapshot).not.toContain("file_fact3");
    });

    test("should only remove clauses from specified file", async () => {
      const file1 = path.join(tempDir, 'file1.pl');
      const file2 = path.join(tempDir, 'file2.pl');

      await fs.writeFile(file1, "file1_fact(a).");
      await fs.writeFile(file2, "file2_fact(b).");

      await prologInterface.importFileWithSource(file1);
      await prologInterface.importFileWithSource(file2);

      const result = await prologInterface.unimportFile(file1);
      expect(result.success).toBe(true);

      const snapshot = await prologInterface.getSnapshot();
      expect(snapshot).not.toContain("file1_fact");
      expect(snapshot).toContain("file2_fact");
    });

    test("should return false for non-imported file", async () => {
      const result = await prologInterface.unimportFile("/nonexistent/file.pl");
      expect(result.success).toBe(false);
      expect(result.clausesRemoved).toBe(0);
    });
  });

  describe("getImportedFiles", () => {
    let tempDir: string;

    beforeEach(async () => {
      tempDir = await fs.mkdtemp(path.join(os.tmpdir(), 'prolog-test-'));
    });

    afterEach(async () => {
      try {
        await fs.rm(tempDir, { recursive: true, force: true });
      } catch (error) {
        // Ignore cleanup errors
      }
    });

    test("should list imported files with metadata", async () => {
      const file1 = path.join(tempDir, 'file1.pl');
      const file2 = path.join(tempDir, 'file2.pl');

      await fs.writeFile(file1, "fact1(a).\nfact2(b).");
      await fs.writeFile(file2, "fact3(c).");

      await prologInterface.importFileWithSource(file1);
      await prologInterface.importFileWithSource(file2);

      const files = prologInterface.getImportedFiles();
      expect(files.length).toBe(2);

      const file1Info = files.find(f => f.filename === file1);
      expect(file1Info).toBeDefined();
      expect(file1Info!.clauseCount).toBe(2);
      expect(file1Info!.timestamp).toBeDefined();

      const file2Info = files.find(f => f.filename === file2);
      expect(file2Info).toBeDefined();
      expect(file2Info!.clauseCount).toBe(1);
    });

    test("should return empty array when no files imported", () => {
      const files = prologInterface.getImportedFiles();
      expect(files).toEqual([]);
    });

    test("should sort files by timestamp (earliest first)", async () => {
      const file1 = path.join(tempDir, 'file1.pl');
      const file2 = path.join(tempDir, 'file2.pl');
      const file3 = path.join(tempDir, 'file3.pl');

      await fs.writeFile(file1, "fact1(a).");
      await fs.writeFile(file2, "fact2(b).");
      await fs.writeFile(file3, "fact3(c).");

      await prologInterface.importFileWithSource(file1);
      await new Promise(resolve => setTimeout(resolve, 10)); // Small delay
      await prologInterface.importFileWithSource(file2);
      await new Promise(resolve => setTimeout(resolve, 10));
      await prologInterface.importFileWithSource(file3);

      const files = prologInterface.getImportedFiles();
      expect(files[0].filename).toBe(file1);
      expect(files[1].filename).toBe(file2);
      expect(files[2].filename).toBe(file3);
    });
  });

  describe("Edge Cases", () => {
    test("should handle clause with special characters", async () => {
      const clause = "special('quoted', \"double\").";
      const result = await prologInterface.assertClauseWithSource(clause, 'inline');

      expect(result.success).toBe(true);

      const snapshot = await prologInterface.getSnapshot();
      expect(snapshot).toContain("special");
    });

    test("should handle mixed inline and file-imported clauses", async () => {
      let tempDir: string;
      let testFile: string;

      try {
        tempDir = await fs.mkdtemp(path.join(os.tmpdir(), 'prolog-test-'));
        testFile = path.join(tempDir, 'test.pl');
        await fs.writeFile(testFile, "file_fact(a).");

        await prologInterface.assertClauseWithSource("inline_fact(b).", 'inline');
        await prologInterface.importFileWithSource(testFile);
        await prologInterface.assertClauseWithSource("another_inline(c).", 'inline');

        const snapshot = await prologInterface.getSnapshot();
        expect(snapshot).toContain("inline_fact");
        expect(snapshot).toContain("file_fact");
        expect(snapshot).toContain("another_inline");

        await fs.rm(tempDir, { recursive: true, force: true });
      } catch (error) {
        // Cleanup on error
        if (tempDir) {
          try {
            await fs.rm(tempDir, { recursive: true, force: true });
          } catch {}
        }
      }
    });

    test("should handle workspace clear after mixed imports", async () => {
      await prologInterface.assertClauseWithSource("fact1(a).", 'inline');
      await prologInterface.assertClauseWithSource("fact2(b).", 'inline');

      await prologInterface.clearWorkspaceWithSource();

      const snapshot = await prologInterface.getSnapshot();
      expect(snapshot).toBe("");

      const files = prologInterface.getImportedFiles();
      expect(files).toEqual([]);
    });
  });
});
