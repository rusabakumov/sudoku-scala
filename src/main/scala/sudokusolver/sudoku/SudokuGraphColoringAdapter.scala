package sudokusolver.sudoku

import sudokusolver.graphcoloring._

import scala.collection.mutable

/**
 * Converts sudoku grid to the graph coloring problem state and vice versa
 */
object SudokuGraphColoringAdapter {

  /**
   * Implementations of graph coloring problem base objects
   */
  case class SudokuVertex(x: Int, y: Int) extends Vertex

  case class SudokuVertexColor(c: Int) extends VertexColor

  def ColoringResultToSudokuGrid(gridSize: Int,
                                 coloring: GraphColoring[SudokuVertex, SudokuVertexColor]): SudokuGrid = {
    val cells = new Array[Array[Int]](gridSize)

    //Creating empty grid to fill it later
    for (i <- 0 until gridSize) {
      cells(i) = new Array[Int](gridSize)
    }

    coloring.coloring foreach {
      case ColoredVertex(SudokuVertex(i, j), SudokuVertexColor(c)) => cells(i)(j) = c
    }

    SudokuGrid(gridSize, cells)
  }

  def SudokuGridToColoringState(grid: SudokuGrid): ColoringState[SudokuVertex, SudokuVertexColor] = {
    /**
     * We need to construct three types of data:
     * 1. List of all vertices in graph with their initial colors
     * 2. Connections between vertices
     * 3. All available colors for vertices
     */

    val verticesWithColors = for {
      i <- 0 until grid.size
      j <- 0 until grid.size
    } yield {
      val color = if (grid.cells(i)(j) > 0) {
        Some(SudokuVertexColor(grid.cells(i)(j)))
      } else {
        None
      }

      (SudokuVertex(i, j), color)
    }

    val vertexConnections = createRowConnectionsForGridVertices(grid.size) ++
      createColumnConnectionsForGridVertices(grid.size) ++
      createRegionConnectionsForGridVertices(grid.regionSize)

    val availableColors = for (i <- 1 to grid.size) yield {
      SudokuVertexColor(i)
    }

    ColoringState(verticesWithColors.toList, vertexConnections.toList, availableColors.toSet)
  }

  private def createRowConnectionsForGridVertices(gridSize: Int): List[(SudokuVertex, SudokuVertex)] = {
    val rowConnections = for {
      row <- 0 until gridSize
      i   <- 0 until gridSize
      j   <- (i + 1) until gridSize
    } yield SudokuVertex(row, i) -> SudokuVertex(row, j)
    rowConnections.toList
  }

  private def createColumnConnectionsForGridVertices(gridSize: Int): List[(SudokuVertex, SudokuVertex)] = {
    val columnConnections = for {
      column  <- 0 until gridSize
      i       <- 0 until gridSize
      j       <- (i + 1) until gridSize
    } yield SudokuVertex(i, column) -> SudokuVertex(j, column)
    columnConnections.toList
  }

  /**
   * Connections for square regions of sudoku grid
   * For 9x9 grid they have 3x3 size, for 16x16 - 4x4 etc
   *
   * Region count is the same as region size
   */
  private def createRegionConnectionsForGridVertices(regionSize: Int): List[(SudokuVertex, SudokuVertex)] = {
    val regionConnections = for {
      regionI <- 0 until regionSize
      regionJ <- 0 until regionSize
    } yield {
      val regionStartI = regionI * regionSize
      val regionStartJ = regionJ * regionSize

      //Creating indices of all vertices in the region first
      val regionVertexIndices = for {
        i <- 0 until regionSize
        j <- 0 until regionSize
      } yield SudokuVertex(regionStartI + i, regionStartJ + j)

      //And creating all connections between them
      for {
        i <- 0 until regionVertexIndices.size
        j <- i + 1 until regionVertexIndices.size
      } yield regionVertexIndices(i) -> regionVertexIndices(j)
    }
    regionConnections.flatten.toList
  }

}
