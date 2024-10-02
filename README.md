<p align="center">
  <img src="images/lucy.png" alt="Lucy, Gleam's mascot">
</p>

<p align="center">
  <a href="https://github.com/gleam-lang/gleam/releases"><img src="https://img.shields.io/github/release/gleam-lang/gleam" alt="GitHub release"></a>
  <a href="https://discord.gg/Fm8Pwmy"><img src="https://img.shields.io/discord/768594524158427167?color=blue" alt="Discord chat"></a>
  <a><img src="https://github.com/gleam-lang/gleam/workflows/ci/badge.svg?branch=main"></a>
</p>

<!-- A spacer -->
<div>&nbsp;</div>

Gleam is a friendly language for building type-safe systems that scale! For more
information see [the website](https://gleam.run).

# Gleam\.NET

This is a fork of Gleam with support for compiling to run on the .NET runtime.

**This is a work in progress**. Many features are implemented, and many are not yet. If you try it out and something doesn't work, feel free to raise an issue or submit a PR.

For a working example, look at [./test/project_fsharp/README.md](./test/project_fsharp/README.md)

The motivation for this was initially to enable a means of native compilation for Gleam code, since the official language currently only supports output to Erlang and Javascript.

## What Gleam\.NET is meant to be:
- A way to write Gleam code that runs on the .NET VM or natively via AOT compilation
- Able to reference .NET code from Gleam code
- Able to reference Gleam code from .NET code
- Mostly compatible with Gleam
- Usable to write real applications
- A fun hobby / learning experience
- Open to contributions from others besides myself

## What Gleam\.NET is _not_ meant to be:
- Perfect
- 100% compatible with every line of code in Gleam and .NET

## Sponsors

Gleam is kindly supported by its sponsors. If you would like to support Gleam
please consider sponsoring its development [by sponsoring lpil on GitHub](https://github.com/sponsors/lpil).


