# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog][], and this project adheres to [Semantic Versioning][].

## [Unreleased]

<!-- ## [1.0.8] - 2023-06-20 -->

### Added

- Support for [Parachute][] unit testing framework
- Package exports for all generated struct functions, for `ISAAC-CTX` and `ISAAC64-CTX`
- Fine-grained unit tests for ISAAC-32 and ISAAC-64 algorithms
- TRIVIAL-FEATURES to test system dependencies
- CHANGELOG.md file following [Keep a Changelog][] format

### Fixed

- Inline type declaration errors for SBCL on Windows and Linux when compiler restriction policies set type safety to 2 or 3

### Changed

- CL-ISAAC API unit tests migrated from [Prove][] to [Parachute][]
- Quick Recipes refactored for more idiomatic Common Lisp
- Function reference and quick recipes copied to project documentation and revised
- GitHub Pages config file updated with new template, project logo, and project metadata
- README.md updated to reflect changes for latest version

### Removed

- Support for [Prove][] unit testing framework

## [1.0.7] - 2022-08-14

### Changed

- Project metadata
- Copyright headers in all source-files
- README

## [1.0.6] - 2022-03-06

### Added

- 64-bit support for ECL and CLISP

### Fixed

- Use 8-bit mask for array index in ISAAC-64 algorithms to avoid overflow

## [1.0.4] - 2014-04-10

### Added

- function `INIT-SELF-SEED` to support stronger cryptographic randomization

## [1.0.3] - 2014-03-20

### Changed

- Available in the March 2014 Quicklisp update

[Prove]: https://github.com/fukamachi/prove
[Parachute]: https://github.com/Shinmera/parachute
[Keep a Changelog]: https://keepachangelog.com/en/1.0.0/
[Semantic Versioning]: https://semver.org/spec/v2.0.0.html