# turing
Collection of [Turing machine](https://en.wikipedia.org/wiki/Turing_machine) interpreters written in various languages.

## Prerequisities
Prerequisites can be validated by running the `Check.ps1` script, possibly with a list of languages to validate. If the parameter `-Languages` is not provided then all supported languages are cheked. In case of failures will provide a set of links to follow to help with the installation.
```powershell
./Scripts/Check.ps1
./Scripts/Check.ps1 -Languages:$("csharp", "python", "cpp")
```

### [C#](https://github.com/sanelli/turing/tree/main/csharp)
- [dotnet](https://dotnet.microsoft.com) 8

### [Python](https://github.com/sanelli/turing/tree/main/python)
- [python](https://www.python.org) 3.11.5

### [C++](https://github.com/sanelli/turing/tree/main/cpp)
- [CMake](https://cmake.org) 3.27
- C++ 20 compiler (the script does not validate this as too many variations exists)

### [Go](https://github.com/sanelli/turing/tree/main/go)
- [Go](https://go.dev) 1.21.3

### [Pascal](https://github.com/sanelli/turing/tree/main/pascal)
- [Free pascal](https://www.freepascal.org) 3.2

### [Ada](https://github.com/sanelli/turing/tree/main/ada)
- [Alire](https://alire.ada.dev) 2.0

### [C](https://github.com/sanelli/turing/tree/main/c)
- [CMake](https://cmake.org) 3.27
- C17 compiler (the Check.ps1 does not validate this)

## Compile and test
```powershell
./Scripts/Build.ps1       # Build all the executable
./Scripts/Substitute.ps1  # Execute the substitute turing machine sample
./Scripts/Test.ps1        # Run all the tests
```

Each script accepts a `-Language` parameter with a list of languages:
```powershell
./Scripts/Build.ps1 -Languages:$("csharp", "python", "cpp")
./Scripts/Substitute.ps1 -Languages:$("csharp", "cpp")
./Scripts/Test.ps1 -Languages:$("cpp")
```

## Screenshots
<img width="682" alt="image" src="https://github.com/sanelli/turing/assets/2866041/045c58a0-ebc1-4e46-85f9-02d858bb99ac">
