# The Hydra Programming Language

A modern, statically-typed programming language with import/namespace support, built in Rust.

## 🚀 Features

### Language Features
- **Static typing** with type inference
- **Import system** with namespace support (`import stdlib::io`)
- **Method call syntax** with transformations (`obj::method()`)
- **Array initialization** with type-size syntax (`{type, size}`)
- **Variadic functions** with `...` syntax
- **Control flow** statements with enhanced break (`break if condition`)
- **Compound assignment** operators (`+=`, `-=`, `*=`, `/=`, `%=`)
- **Power operator** (`**`)
- **Global constants** and variables

### Type System
- Basic types: `int`, `float`, `string`, `char`, `boolean`, `void`
- Array types: `int[]`, `string[]`, etc.
- Variadic types: `int...`
- Type compatibility with automatic promotions (int → float)

### Control Flow
- `if`/`else` statements
- `while` loops
- `for` loops with C-style syntax
- `forEach` loops for arrays
- `break` and `skip` statements
- Conditional breaks: `break if (condition)`

## 📁 Project Structure

```
src/
├── compiler/
│   ├── hydrac.rs                    # Main compiler command
│   ├── hydrac_parse/               # Frontend (Lexer & Parser)
│   │   ├── lexer/
│   │   │   ├── lexer.rs            # Tokenization
│   │   │   └── token.rs            # Token definitions
│   │   └── parser/
│   │       ├── parser.rs           # Syntax analysis
│   │       └── ast.rs              # Abstract Syntax Tree
│   ├── hydrac_middle/              # Middle-end (Analysis)
│   │   ├── semantic_analysis.rs    # Symbol resolution & imports
│   │   └── type_check.rs           # Type checking
│   └── mod.rs                      # Compiler module exports
├── stdlib/                         # Standard library (WIP)
│   ├── io/                        # I/O functions
│   ├── math/                      # Mathematical functions
│   ├── string/                    # String utilities
│   └── array/                     # Array utilities
├── tests/                         # Test programs
└── main.rs                        # CLI entry point
```

## 🛠️ Installation & Usage

### Prerequisites
- Rust 1.70+ with Cargo

### Building
```bash
git clone <repository-url>
cd hydra-compiler
cargo build --release
```

### Running
```bash
# Compile a Hydra program
cargo run -- tests/normal.hydra

# Enable debug output
HYDRA_DEBUG=1 cargo run -- tests/import.hydra

# Enable verbose AST output
HYDRA_VERBOSE=1 cargo run -- tests/modulo.hydra
```

## 📝 Language Syntax

### Basic Program Structure
```hydra
import stdlib::io;
import stdlib::math::sqrt;

global const int MAX_SIZE = 100;
global string APP_NAME = "My App";

func main() -> void {
    int result = factorial(5);
    stdlib::io::stdout("Result: %d\n", result);
}

func factorial(int n) -> int {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}
```

### Data Types & Variables
```hydra
// Basic types
int number = 42;
float pi = 3.14159;
string message = "Hello, Hydra!";
char initial = 'H';
boolean flag = true;

// Arrays
int[] numbers = {1, 2, 3, 4, 5};
char[] buffer = {char, 256};  // Array initialization syntax

// Constants
const int BUFFER_SIZE = 1024;
```

### Control Flow
```hydra
// If statements
if (x > 0) {
    // positive
} else if (x < 0) {
    // negative
} else {
    // zero
}

// Loops
for (int i = 0; i < 10; i++) {
    break if (i == 5);  // Conditional break
}

while (condition) {
    skip;  // Continue to next iteration
}

forEach (int item in array) {
    // Process each item
}
```

### Import System & Namespaces
```hydra
// Import entire modules
import stdlib::io;
import stdlib::math;

// Import specific functions
import stdlib::string::toCharArray;

// Use imported functions
func demo() -> void {
    float result = math::sqrt(16.0);
    string name = "John Doe";
    char[] chars = name::toCharArray();
}
```

### Method Call Syntax
```hydra
// Method calls are transformed to namespace calls
string text = "Hello World";
int length = text::length();           // → string::length(text)
char[] chars = text::toCharArray();    // → string::toCharArray(text)
string upper = text::toUpperCase();    // → string::toUpperCase(text)

// Array methods
int[] data = {1, 2, 3, 4, 5};
int size = data::length();             // → array::length(data)
string str = data::toString();         // → array::toString(data)
```

### Variadic Functions
```hydra
// Function with variadic parameters
func logger(string level, string format, ...) -> void {
    // Implementation handles variable arguments
}

// Usage
logger("INFO", "Processing %d items", count);
```

## 🔧 Compiler Architecture

### Compilation Phases

1. **Lexical Analysis** - Tokenizes source code
2. **Syntax Analysis** - Builds Abstract Syntax Tree (AST)
3. **Semantic Analysis** - Resolves symbols and imports
4. **Type Checking** - Validates type compatibility
5. **Import Resolution** - Processes namespace dependencies
6. **Namespace Validation** - Checks method availability

### Current Status

| Phase | Status | Description |
|-------|--------|-------------|
| ✅ Lexer | Complete | Tokenization with all language features |
| ✅ Parser | Complete | Full AST generation |
| ✅ Semantic Analysis | Complete | Symbol resolution and scoping |
| ✅ Type Checker | Complete | Type validation and inference |
| ✅ Import System | Complete | Namespace and module resolution |
| ⏳ Code Generation | TODO | Target code emission |
| ⏳ Optimization | TODO | Code optimization passes |
| ⏳ Linking | TODO | Final executable generation |

## 🧪 Testing

### Test Files
- `tests/normal.hydra` - Basic language features
- `tests/import.hydra` - Import system and namespaces  
- `tests/modulo.hydra` - Modulo operations
- `tests/invalid_semantics.hydra` - Semantic error cases
- `tests/invalid_types.hydra` - Type error cases

### Running Tests
```bash
# Test valid programs
cargo run -- tests/normal.hydra
cargo run -- tests/import.hydra
cargo run -- tests/modulo.hydra

# Test error handling
cargo run -- tests/invalid_semantics.hydra
cargo run -- tests/invalid_types.hydra
```

## 🔍 Error Handling

The compiler provides detailed error messages with helpful hints:

```
❌ Semantic/Type checking errors found:
   1. Undefined symbol 'unknownVariable'
   2. Method 'isUpper' is not available for type int[]

💡 Helpful hints:
   📦 Import Error: Make sure the stdlib directory exists
   🔗 Method Error: Check if you're calling the correct method for the type
   📋 Array Init Error: Use integer literals for array size
```

## 🚧 Roadmap

### Near Term
- [ ] Code generation to intermediate representation
- [ ] Basic optimization passes
- [ ] Standard library implementation
- [ ] Error recovery in parser

### Long Term
- [ ] LLVM backend integration
- [ ] Advanced optimizations
- [ ] Package manager
- [ ] Language server protocol support
- [ ] Debugger integration

### Development Setup
```bash
# Clone and build
git clone <repository-url>
cd hydra-compiler
cargo build

# Run tests
cargo test

# Check formatting
cargo fmt --check

# Run lints
cargo clippy
```

## 🙏 Acknowledgments

- Rust community for excellent tooling
- Programming language design inspiration from C, Rust, and Java
- Java for making me so frustrated, I decided to write my own language

---

**Note**: Hydra is currently in active development. While the frontend (lexing, parsing, semantic analysis) is feature-complete, code generation is not yet implemented. The language design is stable, but expect changes as development continues.