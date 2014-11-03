package sudokusolver.sudoku

import sudokusolver.graphcoloring.ColoringProblem
import sudokusolver.utils.Logging

/**
 * Describes one sudoku game - it's initial grid, solution process and the resulting grid
 */
case class SudokuGame(grid: SudokuGrid) extends Logging {
  private var solutionGrid: Option[SudokuGrid] = None

  /**
   * Trying to solve this sudoku grid as graph coloring problem
   */
  def trySolve() {
    val coloringProblem = ColoringProblem(SudokuGraphColoringAdapter.SudokuGridToColoringState(grid))
    coloringProblem.trySolve()

    coloringProblem.getSolution map { coloring =>
      solutionGrid = Some(SudokuGraphColoringAdapter.ColoringResultToSudokuGrid(grid.size, coloring))
    }
  }

  def isSolved = solutionGrid.isDefined
  def getSolution = solutionGrid
}
