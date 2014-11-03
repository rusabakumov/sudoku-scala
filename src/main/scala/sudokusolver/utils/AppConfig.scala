package sudokusolver.utils

import com.typesafe.config.ConfigFactory

/**
 * Global object that holds initialized config object
 */
object AppConfig {

  val config  = ConfigFactory.load().getConfig("sudoku-solver")

}
