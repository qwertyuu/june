# June
June is a compiled language that allows for low level programming.

```
import std.Sys;

main() {
    Sys.println("Hello from june!");
}
```

### requirements (building from source)

* Must have LLVM installed.
* Currently has only been tested on windows with MSVC compiler.
* CMake requires LLVM_DIR variable to point to the built LLVM install.

### Running the june driver

When building the cmake sub-project `driver` it allows you to compile june projects from the terminal: ``june <sources> <options>``
