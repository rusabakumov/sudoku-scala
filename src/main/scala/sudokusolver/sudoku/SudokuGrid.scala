package sudokusolver.sudoku

import java.io.{BufferedWriter, FileWriter, BufferedReader, FileReader}

import sudokusolver.utils.AppConfig

/**
 * Represents grid for sudoku game alongside it's parameters (size, regionSize)
 */
case class SudokuGrid(size: Int, cells: Array[Array[Int]]) {

  val regionSize = math.sqrt(size).toInt

  require(regionSize * regionSize == size, "Sudoku grid size should be the square of a natural number")
}

object SudokuGrid {
  private val defaultGridSize = AppConfig.config.getInt("default-grid-size")
  private val defaultInputFile = AppConfig.config.getString("default-input-file")
  private val defaultOutputFile = AppConfig.config.getString("default-output-file")

  def readGridFromFile(gridSize: Int = defaultGridSize, fileName: String = defaultInputFile): SudokuGrid = {
    val fileReader = new FileReader(fileName)
    val reader = new BufferedReader(fileReader)

    val cells = new Array[Array[Int]](gridSize)

    for (i <- 0 until gridSize) {
      val rowNumbers = reader.readLine().split(",") map (_.toInt)

      rowNumbers foreach { num =>
        if (num < 0 || num > gridSize) {
          throw new IllegalArgumentException(s"Number $num cannot appear in ${gridSize}x$gridSize sudoku grid")
        }
      }

      cells(i) = rowNumbers
    }
    reader.close()
    fileReader.close()

    SudokuGrid(gridSize, cells)
  }

  def writeGridToFile(grid: SudokuGrid, fileName: String = defaultOutputFile) {
    val fileWriter = new FileWriter(fileName)
    val writer = new BufferedWriter(fileWriter)

    for (i <- 0 until grid.size) {
      for (j <- 0 until grid.size) {
        if (j != 0) {
          writer.write(",")
        }
        writer.write(grid.cells(i)(j).toString)
      }
      writer.newLine()
    }

    writer.close()
    fileWriter.close()
  }

}
