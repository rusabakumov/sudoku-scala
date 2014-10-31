package sudoku

object Solver {

  def main(args: Array[String]) {
    val grid = SudokuGame.readGridFromFile()

    val initialColoringState = SudokuToColoring.SudokuToColoring(grid)

    ColoringBacktrack.findColoring(initialColoringState) match {
      case Some(coloring) =>
        println("Sudoku solved!")
        val resultingGrid = SudokuToColoring.ColoringResultToSudokuGrid(coloring)
        SudokuGame.writeGridToFile(resultingGrid)

      case None =>
        println("Cannot solve sudoku!")
    }
  }

}

