sudoku-scala
============

A simple solver for sudoku game. Solves sudoku as graph coloring problem (using backtracking).

It has two run modes:
- one grid solver, that gets a grid from .csv file and outputs the result to another
- test mode; solver runs on a predefined testsuite with several sudoku grids. They defined in application.conf file. See it for details

The easiest way to run is to use <a href="http://sbt.org/" target="_blank">Simple Build Tool</a>. You should run ```sbt``` command in the project root directory. After project is loaded, you can type in ```run --help``` command and after build phase the usage message will be shown.
