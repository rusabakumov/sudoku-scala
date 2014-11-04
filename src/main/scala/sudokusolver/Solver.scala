package sudokusolver

import sudokusolver.sudoku.{SudokuGame, SudokuGrid}
import sudokusolver.utils.{AppConfig, Logging}

object Solver extends Logging {
  private val defaultGridSize = AppConfig.config.getInt("default-grid-size")
  private val defaultInFile = AppConfig.config.getString("default-input-file")
  private val defaultOutFile = AppConfig.config.getString("default-output-file")

  case class Config(inputFile: String = defaultInFile,
                    outputFile: String = defaultOutFile,
                    gridSize: Int = defaultGridSize)

  def main(args: Array[String]): Unit = argParser.parse(args, Config()) map { config =>
    val sudokuGame = SudokuGame(SudokuGrid.readGridFromFile(config.gridSize, config.inputFile))
    sudokuGame.trySolve()

    sudokuGame.solution match {
      case Some(resultingGrid) =>
        logger.info("Sudoku solved!")
        SudokuGrid.writeGridToFile(resultingGrid, config.outputFile)

      case None =>
        logger.info("Cannot solve sudoku!")
    }
  }

  private val argParser = new scopt.OptionParser[Config]("sudoku-solver") {
    head("Sudoku solver", "1.0")
    opt[String]('i', "infile") valueName "<input file>" action { (value, conf) =>
      conf.copy(inputFile = value)
    } text s"File where input grid is stored, default is $defaultInFile"
    opt[String]('o', "outfile") valueName "<output file>" action { (value, conf) =>
      conf.copy(outputFile = value)
    } text s"File to write output grid, default is $defaultOutFile"
    opt[Int]('n', "gridSize") valueName "<grid size>" action { (value, conf) =>
      conf.copy(gridSize = value)
    } text s"The size of game grid, $defaultGridSize by default"
  }

}

