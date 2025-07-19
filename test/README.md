# Yi Editor Test Suite

Comprehensive test suite for Yi editor enhancements including new keymaps and async syntax highlighting.

## Test Structure

```
test/
├── Spec.hs                    # Main test runner
├── KeymapTests/              # Unit tests for each keymap
│   ├── AcmeSpec.hs          # Acme keymap tests
│   ├── CuaSpec.hs           # CUA keymap tests
│   ├── JoeSpec.hs           # Joe keymap tests
│   ├── MgSpec.hs            # Mg keymap tests
│   └── EeSpec.hs            # Ee keymap tests
├── Properties/               # Property-based tests
│   ├── KeymapProperties.hs  # General keymap properties
│   ├── SwitchingProps.hs    # Runtime switching properties
│   └── EditingProps.hs      # Text editing properties
└── Integration/              # Integration tests
    ├── RuntimeSpec.hs       # Runtime control tests
    └── HighlightSpec.hs     # Async highlighting tests
```

## Running Tests

```bash
# Run all tests
stack test

# Run with coverage
stack test --coverage

# Run specific test suite
stack test yi-tests:yi-test-suite

# Run with detailed output
stack test --test-arguments="--format=detailed"

# Run only specific tests
stack test --test-arguments="--match=Acme"
```

## Test Coverage

The test suite covers:

- **Keymap Bindings**: All key bindings for each keymap (Acme, CUA, Joe, Mg, Ee)
- **Runtime Switching**: Dynamic keymap switching and state preservation
- **Memory Safety**: No leaks during keymap switches
- **Concurrent Operations**: Thread-safe keymap operations
- **Async Highlighting**: Non-blocking syntax highlighting
- **External Parsers**: Integration with haskell-src-exts and tree-sitter
- **Rainbow Parentheses**: Depth-based coloring
- **Performance**: Sub-millisecond keymap switches, linear memory scaling

## Property Tests

QuickCheck properties verify:

- Keymap invariants (no duplicate bindings)
- Composition laws (associativity, identity)
- State preservation across switches
- Memory bounds and performance guarantees
- Text editing consistency (undo/redo, insertions, deletions)
- Bracket balancing in rainbow highlighting

## Integration Tests

Full system tests covering:

- Frontend communication (Vty, Qt, Web)
- Runtime configuration changes
- Auto-detection of large files
- Memory efficiency with huge files
- Cancellation of long-running operations

## Benchmarks

Performance benchmarks are available:

```bash
stack bench
```

Key metrics:
- Keymap switch time: < 1ms
- Highlighting 1MB file: < 1s
- Memory per line: < 1KB