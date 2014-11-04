package sudokusolver.graphcoloring

import sudokusolver.utils.Logging

/**
 * Represents state of graph coloring problem (graph structure and partial coloring)
 * Contains methods for checking consistency of the state and transition methods to build new states (painting one vertex)
 *
 * Transition method (paintVertex) maintains state consistency (suitable colors are safe, vertex can be painted to any
 * suitable color without breaking state consistency)
 *
 * @param vertices - list of states for all vertices in graph
 */
case class ColoringState[V <: Vertex, VC <: VertexColor](vertices: List[VertexState[V, VC]]) extends Logging {
  /**
   * Checks that current partial coloring is consistent at this state: all uncolored vertices has at least one suitable color
   * The reason to have this separate from isCorrect is the efficiency - it requires only one pass over vertices
   */
  def isConsistent: Boolean = {
    vertices forall {
      case VertexState(vertex, None, suitableColors, _) =>
        logger.trace(s"Vertex $vertex is not colored and has no suitable colors!")
        suitableColors.nonEmpty
      case _  => true
    }
  }

  /**
   * Checks that current partial coloring is correct at this state: there is no painting conflicts
   * (no connected vertices painted with same color)
   */
  def isCorrect: Boolean = {
    vertices forall {
      //If vertex is colored - no neighbors colored with the same color
      case VertexState(vertex, Some(color), _, neighbors) => neighbors forall {
        case neighborVertex => vertices.find(_.vertex == neighborVertex).forall { neighborVertexState =>
          val colorsNotSame = neighborVertexState.color != Some(color)

          if (!colorsNotSame) {
            logger.trace(s"Connected vertices $vertex and $neighborVertex have the same color $color!")
          }

          colorsNotSame
        }
      }
      case VertexState(vertex, None, suitableColors, _) => true
    }
  }

  /**
   * Checks that this state is final - all vertices colored
   */
  def isComplete: Boolean = vertices.forall(_.color.isDefined)

  /**
   * Updates state by painting given vertex to given color and returns updated state
   *
   * Also updates list of suitable colors for neighbor vertices - each color in this list assumed to be "safe"
   * (vertex can be painted to it without breaking consistency)
   */
  def paintVertex(vertex: V, color: VC): ColoringState[V, VC] = vertices.find(_.vertex == vertex) match {
    case Some(vertexState) =>
      if (!vertexState.suitableColors.contains(color)) {
        throw new IllegalStateException(s"Color $color is not available for vertex $vertex!")
      }

      val updatedVertexState: VertexState[V, VC] = vertexState.copy(color = Some(color), suitableColors = Set.empty[VC])

      val updatedVertices = vertices map {
        //Newly colored vertex
        case VertexState(v, _, _, _) if v == vertex         => updatedVertexState
        case vs @ VertexState(v, _, suitableColors, _)
          if vertexState.adjacentVertices.contains(v)       => vs.copy(suitableColors = suitableColors - color)
        case vs : VertexState[V, VC]                        => vs
      }
      copy(vertices = updatedVertices)

    case None => throw new IllegalStateException(s"Unknown vertex $vertex to paint")
  }

  /**
   * Build graph coloring object if the state is final. Returns None otherwise
   */
  def getColoring: Option[GraphColoring[V, VC]] = {
    if (isComplete) {
      val verticesWithColors = vertices map { vertexState =>
        ColoredVertex(
          vertexState.vertex,
          vertexState.color.getOrElse(throw new IllegalStateException("Found not colored vertex in solved coloring"))
        )
      }
      Some(GraphColoring(verticesWithColors))
    } else {
      None
    }
  }

}

object ColoringState {

  /**
   * Constructs initial coloring state from list of vertices with initial coloring,
   * their connections and available colors
   * Note: adjacency list contains bidirectional edges (edge 1-2 assumes that edge 2-1 also exists)
   */
  def apply[V <: Vertex, VC <: VertexColor](verticesWithColors: List[(V, Option[VC])],
                                            adjacencyList: List[(V, V)],
                                            availableColors: Set[VC]): ColoringState[V, VC] = {
    //At first, creating vertex states without coloring
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
    val blankState = ColoringState(vertexStates)

    //Apply initial coloring using state methods
    val initialState = verticesWithColors.foldLeft[ColoringState[V, VC]](blankState) {
      //Paint is initial color is given
      case (state, (vertexIndex, Some(color))) => state.paintVertex(vertexIndex, color)
      case (state, _)  => state
    }

    if (!initialState.isConsistent) {
      throw new IllegalArgumentException("Given coloring problem already incorrect!")
    }

    initialState
  }

}