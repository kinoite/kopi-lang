# Kopi Programming Language

![Logo](/Untitled5_20250629200314.png)
![enbyware](https://camo.githubusercontent.com/bee100e0a2439d329fd52512a3acd1b4df3adf924e315507c5433d68ab79ab95/68747470733a2f2f70726964652d6261646765732e706f6e792e776f726b6572732e6465762f7374617469632f76313f6c6162656c3d656e627977617265266c6162656c436f6c6f723d2532333535352673747269706557696474683d3826737472697065436f6c6f72733d464346343334253243464646464646253243394335394431253243324332433243) 
![GitHub License](https://img.shields.io/github/license/kinoite/kopi-lang)
![GitHub commit activity](https://img.shields.io/github/commit-activity/w/kinoite/kopi-lang)
![GitHub language count](https://img.shields.io/github/languages/count/kinoite/kopi-lang%20)


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
