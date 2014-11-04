package sudokusolver

import java.io.{File, FilenameFilter}

import sudokusolver.sudoku.{SudokuGame, SudokuGrid}
import sudokusolver.utils.{AppConfig, Logging}

import scala.collection.JavaConversions._

/**
 * Runs all test suites that presented in config
 */
object Tester extends Logging {

  def main(args: Array[String]) {
    val testSetNames = AppConfig.config.getObject("test-sets").keySet()

    logger.info(s"Starting sudoku solver tester. Found ${testSetNames.size()} testsets")
    testSetNames foreach { case testSetName =>
      val testSetConfig = AppConfig.config.getConfig("test-sets").getConfig(testSetName)

      val name = testSetConfig.getString("name")
      val path = testSetConfig.getString("path")
      val gridSize = testSetConfig.getInt("gridSize")
      runTestSet(name, path, gridSize)
    }
    logger.info(s"Testing completed!")
  }

  /**
   * Runs sudoku solver on all .csv files that found in given path
   * All found grids should have the same size
   * @param gridSize - A size for all grids in found .csv files
   */
  def runTestSet(name: String, path: String, gridSize: Int) {
    val gridFiles = new File(path).listFiles(csvFileFilter)

    //Path where solutions for testset will be stored
    val resultPath = new File(resultDir, name)
    resultPath.mkdirs() //Ensuring it

    logger.info(s"Running $name testset with ${gridFiles.size} grids:")
    gridFiles.zipWithIndex foreach {
      case (gridFile, index) =>
        val filePath = gridFile.getPath
        val sudokuGame = SudokuGame(SudokuGrid.readGridFromFile(gridSize, filePath))

        logger.info(s"Solving grid #$index ($filePath}) ...")
        val startTime = System.nanoTime()
        sudokuGame.trySolve()

        //Calculation test duration
        val duration = System.nanoTime() - startTime
        val secondsPassed = duration * secondsInNanos
        val formattedSeconds = "%.3f".format(secondsPassed)

        sudokuGame.getSolution match {
          case Some(resultingGrid)  =>
            logger.info(s"Grid #$index solved in $formattedSeconds seconds")

            val resultFile = new File(resultPath.getPath, gridFile.getName)
            SudokuGrid.writeGridToFile(resultingGrid, resultFile.getPath)

          case None                 =>
            logger.info(s"Grid #$index cannot be solved!")
        }
    }
    logger.info(s"Testset $name completed!")
  }

  //Path where results will be stored
  private val resultDir = AppConfig.config.getString("test-set-result-dir")

  private val secondsInNanos = 0.000000001

  private val csvFileFilter = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = name.toLowerCase.endsWith(".csv")
  }

}
