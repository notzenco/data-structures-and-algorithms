# Package

version       = "1.0.0"
author        = "DSA Contributors"
description   = "Data Structures and Algorithms in Nim"
license       = "MIT"
srcDir        = "src"

# Dependencies

requires "nim >= 2.0.0"

# Tasks

task test, "Run tests":
  exec "nim c -r tests/test_all.nim"
