package sudokusolver

import sudokusolver.sudoku.{SudokuGame, SudokuGrid}
import sudokusolver.utils.Logging

object Solver extends Logging {

  def main(args: Array[String]) {
    val sudokuGame = SudokuGame(SudokuGrid.readGridFromFile())
    sudokuGame.trySolve()

    sudokuGame.getSolution match {
      case Some(resultingGrid) =>
        logger.info("Sudoku solved!")
        SudokuGrid.writeGridToFile(resultingGrid)

      case None =>
        logger.info("Cannot solve sudoku!")
    }
  }

}

