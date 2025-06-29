# Kopi Programming Language

![Untitled5_20250629200314.png]("logo")

**Kopi** is a fast, modern, and lightweight scripting language built in Rust. It features a clean, simple syntax and is designed to be easy to learn and embed in other applications.

---

## Features

* **Simple Syntax:** Familiar, intuitive syntax that's easy to pick up.
* **Core Data Types:** Full support for numbers, strings, and booleans.
* **Rich Control Flow:** Includes `if/else`, `while` loops, and logical operators (`and`, `or`, `not`).
* **Powerful Functions:** Supports user-defined functions, parameters, and `return` values.
* **Variable Bindings:** Use `let` to create variables and `=` to reassign them.
* **Lua-Style Comments:** Supports both single-line (`--`) and block comments (`--[[ ... ]]--`).
* **Rich Error Messages:** Compiler-style error reporting makes debugging a breeze.

---

## Installation

Kopi is very easy to install, just clone this repository;
```
git clone https://github.com/kinoite/kopi-lang.git
```
then go into the cloned Kopi directory
```
cd kopi-lang
```
and the last step, compiling it!
```
cargo build --release
```

Alternatively, you can move it to your `$PATH`.
```
sudo mv target/release/kopi /usr/local/bin
```
And you are basically just done!- enjoy hacking in Kopi!

---
