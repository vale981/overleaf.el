# Elisp Development: Lessons Learned

## 1. Syntax Correctness & Parenthesis Debugging

Parenthesis mismatches are a common source of frustration in Elisp. Here is the recommended workflow for resolving them:

- **Check Parens**: Run `(check-parens)` in the buffer or via batch mode:
  ```bash
  emacs --batch <file.el> --eval '(check-parens)'
  ```
  This will pinpoint the exact location of the first unmatched bracket or quote.
- **Native/Byte Compilation**: Use the byte-compiler to find the exact line where the parser gives up:
  ```bash
  emacs --batch -f batch-byte-compile <file.el>
  ```
  Errors like `End of file during parsing` or `Invalid read syntax: ")"` indicate that the structural nesting is broken.
- **Structural Editing**: Use tools like `paredit` or `smartparens` during development to prevent mismatches from occurring in the first place.

## 2. Preferring Smaller Functions (Decomposition)

Breaking giant functions into smaller pieces is not just about style; it's a debugging strategy:

- **Locating Errors**: When a 100-line function has a parenthesis error, finding it is difficult. If that logic is split into five 20-line functions, the error is isolated to a much smaller scope.
- **Reusability**: Smaller helpers (like `overleaf--ediff-two-way`) can be reused across different paths (e.g., as a direct resolution path OR a fallback path).
- **Testability**: Small, pure (or semi-pure) functions are significantly easier to unit test than monolithic handlers that manage state, UI, and network calls simultaneously.
- **Compiler Clarity**: Byte-compiler warnings (like unused variables or free variables) become much more specific when the scope is narrow.

## 3. Practical Workflow for Large Edits

- **Incremental Validation**: Run the byte-compiler after every small change. Do not wait until a 50-line block is written.
- **Top-Down or Bottom-Up**: 
    - **Bottom-Up**: Define small helper functions first, verify them, then use them in the main logic.
    - **Top-Down**: Stub out the logic in the main function first, then extract the stubs into real functions.
- **Variable Visibility**: Moving code into separate functions often reveals hidden dependencies or variables that should have been passed as arguments rather than relied upon via lexical/dynamic scope.
