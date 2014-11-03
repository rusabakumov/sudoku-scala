package sudokusolver.graphcoloring

/**
 * Represents one instance of graph coloring problem.
 * By given graph with initial partial coloring (not mandatory), tries to paint it's vertices witg available colors
 * @param initialState - description of the graph to deal with (vertices with initial colors, connections,
 *                     available colors for vertices)
 * @tparam V  - type of vertex
 * @tparam VC - type of vertex color
 */
case class ColoringProblem[V <: Vertex, VC <: VertexColor](initialState: ColoringState[V, VC]) {
  private var resultingState: Option[ColoringState[V, VC]] = None

  def trySolve() = {
    resultingState = findColoring(initialState)
  }

  def getSolution: Option[GraphColoring[V, VC]] = resultingState flatMap (_.getColoring)

  /**
   * Implements backtracking on vertex colors.
   *
   * Selects a vertex with a minimal amount of suitable colors, paints it to the first of them and tries to solve the
   * reduced problem. If no coloring found, tries another suitable color and so on, until solution is found.
   *
   * @param state - state with current partial coloring and suitable colors for vertices
   * @return any state with full and consistent coloring or None, if it's not exists
   */
  private def findColoring(state: ColoringState[V, VC]): Option[ColoringState[V, VC]] = {
    if (state.isComplete) {
      return Some(state)
    }

    for {
      nonColoredVertex  <- state.vertices.filter(_.color.isEmpty)
      color             <- nonColoredVertex.suitableColors
      updatedState      = state.paintVertex(nonColoredVertex.vertex, color)
      if updatedState.isConsistent
      finalState        <- findColoring(updatedState)
    } {
      return Some(finalState)
    }

    None
  }

}
