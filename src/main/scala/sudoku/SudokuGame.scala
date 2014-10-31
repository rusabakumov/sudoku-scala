package sudoku

import java.io.{BufferedWriter, FileWriter, BufferedReader, FileReader}

case class SudokuGrid(order: Int, cells: Array[Array[Int]])

object SudokuGame {

  val gameOrder: Int = 3

  val N: Int = gameOrder * gameOrder

  def readGridFromFile(fileName: String = "grid.csv"): SudokuGrid = {
    val fileReader = new FileReader(fileName)
    val reader = new BufferedReader(fileReader)

    val cells = new Array[Array[Int]](N)

    for (i <- 0 until N) {
      val rowNumbers = reader.readLine().split(",") map (_.toInt)

      rowNumbers foreach { num =>
        if (num < 0 || num > N) {
          throw new IllegalArgumentException(s"Number $num can not appear in 3x3 sudoku grid")
        }
      }

      cells(i) = rowNumbers
    }
    reader.close()
    fileReader.close()

    SudokuGrid(gameOrder, cells)
  }

  def writeGridToFile(grid: SudokuGrid, fileName: String = "out.txt") {
    val fileWriter = new FileWriter(fileName)
    val writer = new BufferedWriter(fileWriter)

    for (i <- 0 until N) {
      for (j <- 0 until N) {
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
