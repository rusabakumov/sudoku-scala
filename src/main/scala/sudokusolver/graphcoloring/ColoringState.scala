package sudokusolver.graphcoloring

/**
 * Represents state of graph coloring problem (graph structure and partial coloring)
 * Contains methods for checking consistency of the state and transition methods to build new states (painting one vertex)
 *
 * @param vertices - list of states for all vertices in graph
 */
case class ColoringState[V <: Vertex, VC <: VertexColor](vertices: List[VertexState[V, VC]]) {
  /**
   * Checks that current partial coloring is correct at this state:
   * all uncolored vertices has at least one suitable color
   */
  def isConsistent: Boolean = {
    vertices forall {
      //If vertex is colored - no neighbors colored with the same color
      case VertexState(_, Some(color), _, neighbors) => neighbors forall { case vertexIndex =>
        vertices.find(_.vertex == vertexIndex).forall(_.color != Some(color))
      }
      case VertexState(_, None, suitableColors, _) => suitableColors.nonEmpty
    }
  }

  /**
   * Checks that this state is final - all vertices colored in correct way
   */
  def isComplete: Boolean = vertices.forall(_.color.isDefined) && isConsistent

  /**
   * Updates state by painting given vertex to given color and returns updated state
   */
  def paintVertex(vertex: V, color: VC): ColoringState[V, VC] = vertices.find(_.vertex == vertex) match {
    case Some(vertexState) =>
      if (!vertexState.suitableColors.contains(color)) {
        throw new IllegalStateException(s"Color $color is not available for vertex $vertex!")
      }

      val updatedVertexState: VertexState[V, VC] = vertexState.copy(color = Some(color), suitableColors = Set.empty[VC])

      val updatedVertices = vertices map {
        //Newly colored vertex
        case VertexState(idx, _, _, _) if idx == vertex   => updatedVertexState
        case vs @ VertexState(idx, _, suitableColors, _)
          if vertexState.adjacentVertices.contains(idx)   => vs.copy(suitableColors = suitableColors - color)
        case vs : VertexState[V, VC]                      => vs
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