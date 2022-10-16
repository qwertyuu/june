# June
June is a compiled language that allows for low level programming.

```
import std.Sys;

main() {
    Sys.println("Hello from june!");
}
```

### requirements (building from source)

* Must have **LLVM 14** installed.
* Tested with both **MSVC** on windows and on **arch linux**.
* When on windows and compiling with **MSVC** you may need to set cmake's LLVM_DIR variable. Otherwise, on linux you should just need **LLVM 14** installed.

### Running the june driver

When building the cmake sub-project `driver` it allows you to compile june projects from the terminal: ``june <sources> <options>``
To get help: ``june -help``

The driver will expect the environment variable ``JuneStdLibPath`` to be set. Simply set it to be the location of where the ``stdlib`` folder is at on your system.

