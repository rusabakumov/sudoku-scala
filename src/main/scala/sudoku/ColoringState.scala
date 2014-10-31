package sudoku

import sudoku.ColoringBacktrack.Coloring

trait VertexIndex

trait VertexColor

case class VertexState(index: VertexIndex,
                       color: Option[VertexColor],
                       suitableColors: Set[VertexColor],
                       adjacentVertices: List[VertexIndex])

case class ColoringState(vertices: List[VertexState]) {

  /**
   * Checks that current partial coloring is correct and that it's not deadlock (no uncolored vertices with no suitable colors)
   */
  def isConsistent: Boolean = {
    vertices forall {
      //If vertex is colored - no neighbors colored the same way
      case VertexState(_, Some(color), _, neighbors) => neighbors forall { case vertexIndex =>
        vertices.find(_.index == vertexIndex).forall(_.color != Some(color))
      }
      case VertexState(_, None, suitableColors, _) => suitableColors.nonEmpty
    }
  }

  /**
   * Updates state by painting vertex with given index to given color and returning updated one
   */
  def paintVertex(index: VertexIndex, color: VertexColor): ColoringState = vertices.find(_.index == index) match {
    case Some(vertex) =>
      if (!vertex.suitableColors.contains(color)) {
        throw new IllegalStateException(s"Color $color is not available for vertex $index!")
      }

      //TODO Check that we can paint (maybe)

      val updatedVertex = vertex.copy(color = Some(color), suitableColors = Set.empty[VertexColor])

      val updatedVertices = vertices map {
        //Newly colored vertex
        case VertexState(idx, _, _, _) if idx == index  => updatedVertex
        case v @ VertexState(idx, _, suitableColors, _)
          if vertex.adjacentVertices.contains(idx)      => v.copy(suitableColors = suitableColors - color)
        case v : VertexState                            => v
      }
      copy(vertices = updatedVertices)

    case None => throw new IllegalStateException(s"Unknown vertex $index to paint")
  }

  def isComplete: Boolean = vertices.forall(_.color.isDefined) && isConsistent

  def getColoring: Option[Coloring] = {
    if (isComplete) {
      Some(vertices map ( v => (v.index, v.color.getOrElse(throw new IllegalStateException("WTF"))) ))
    } else None
  }

}

object ColoringState {

  /**
   * Constructs initial coloring state from available vertices, their connections and available colors
   * Note: adjacency list contains bidirectional edges (they are not duplicated)
   * @return
   */
  def apply(verticesWithColors  : List[(VertexIndex, Option[VertexColor])],
            adjacencyList       : List[(VertexIndex, VertexIndex)],
            availableColors     : Set[VertexColor]): ColoringState = {
    //At first, creating state with no painting
    val vertexStates = verticesWithColors map {
      case (index, _) =>
        //Searching for all neighbors in the list of edges
        val neighbors = adjacencyList flatMap {
          case (from, to) if from == index  => List(to)
          case (from, to) if to == index    => List(from)
          case _                            => Nil
        }
        VertexState(index, None, availableColors, neighbors)
    }

    //Then creating state and apply initial coloring
    val blankState = ColoringState(vertexStates)

    val initialState = verticesWithColors.foldLeft[ColoringState](blankState) {
      //Paint is initial color is given
      case (state, (vertexIndex, Some(color))) => state.paintVertex(vertexIndex, color)
      case (state, _)  => state
    }

    if (!initialState.isConsistent) {
      throw new IllegalArgumentException("Given sudoku grid already incorrect!")
    }

    initialState
  }

}