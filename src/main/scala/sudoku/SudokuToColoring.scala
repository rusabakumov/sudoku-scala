package sudoku

import sudoku.ColoringBacktrack.Coloring

import scala.collection.mutable

object SudokuToColoring {

  case class SudokuVertexIndex(x: Int, y: Int) extends VertexIndex

  case class SudokuVertexColor(c: Int) extends VertexColor

  def SudokuToColoring(grid: SudokuGrid): ColoringState = {
    val N = grid.order * grid.order

    //Creating indices
    val verticesWithColors = for {
      i <- 0 until N
      j <- 0 until N
    } yield {
      val color = if (grid.cells(i)(j) > 0) {
        Some(SudokuVertexColor(grid.cells(i)(j)))
      } else {
        None
      }

      (SudokuVertexIndex(i, j), color)
    }

    //Creating connections
    val adjacencyList = createConnectionsForGridVertices(grid)

    val availableColors = for (i <- 1 to N) yield {
      SudokuVertexColor(i)
    }

    ColoringState(verticesWithColors.toList, adjacencyList.toList, availableColors.toSet)
  }

  def ColoringResultToSudokuGrid(coloring: Coloring): SudokuGrid = {
    val N = math.sqrt(coloring.size).toInt //FIXME #PIZDEC
    val order = math.sqrt(N).toInt //FIXME #PIZDEC

    val cells = new Array[Array[Int]](N)
    //Creating empty arrays to fill later
    for (i <- 0 until N) {
      cells(i) = new Array[Int](N)
    }

    coloring foreach {
      case (SudokuVertexIndex(i, j), SudokuVertexColor(c)) => cells(i)(j) = c
    }

    SudokuGrid(order, cells)
  }

  private def createConnectionsForGridVertices(grid: SudokuGrid) = {
    val N = grid.order * grid.order

    val adjacencyList = mutable.ArrayBuffer[(VertexIndex, VertexIndex)]()

    for (k <- 0 until N) {
      //Creating connections for row i
      for {
        i <- 0 until N
        j <- (i + 1) until N
      } {
        val startIndex = SudokuVertexIndex(k, i)
        val endIndex = SudokuVertexIndex(k, j)
        adjacencyList += startIndex -> endIndex
      }

      //Creating connections for column i
      for {
        i <- 0 until N
        j <- (i + 1) until N
      } {
        val startIndex = SudokuVertexIndex(i, k)
        val endIndex = SudokuVertexIndex(j, k)
        adjacencyList += startIndex -> endIndex
      }
    }

    //Connections for regions
    for {
      regionI <- 0 until grid.order
      regionJ <- 0 until grid.order
    } {
      val regionStartI = regionI * grid.order
      val regionStartJ = regionJ * grid.order

      val regionVertexIndices = for {
        i <- 0 until grid.order
        j <- 0 until grid.order
      } yield {
        SudokuVertexIndex(regionStartI + i, regionStartJ + j)
      }

      for {
        i <- 0 until regionVertexIndices.size
        j <- i + 1 until regionVertexIndices.size
      } {
        val startIndex = regionVertexIndices(i)
        val endIndex = regionVertexIndices(j)
        adjacencyList += startIndex -> endIndex
      }
    }

    adjacencyList
  }

}
