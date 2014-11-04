sudoku-scala
============

A simple solver for sudoku game. Solves sudoku as graph coloring problem (using backtracking).

Has two run modes
- one grid solver, that gets a grid from .csv file and outputs the result to another
- test mode. Solver runs on predefined testsuite with several given grids. They defined in application.conf

The easiest way to run - to use sbt (Simple Build Tool, sbt.org). You should run it in the project directory.
After project is loaded, you can type in "run --help" command and after build phase the usage message will be shown.
