package sudokusolver.graphcoloring

import sudokusolver.utils.Logging

/**
 * Represents one instance of graph coloring problem.
 * By given graph with initial partial coloring (not mandatory), tries to paint it's vertices witg available colors
 * @param initialState - description of the graph to deal with (vertices with initial colors, connections,
 *                     available colors for vertices)
 * @tparam V  - type of vertex
 * @tparam VC - type of vertex color
 */
case class ColoringProblem[V <: Vertex, VC <: VertexColor](initialState: ColoringState[V, VC]) extends Logging {
  if (!initialState.isConsistent) {
    logger.error("Initial state passed to coloring problem is not consistent!")
  }

  private var resultingState: Option[ColoringState[V, VC]] = None

  def trySolve() = {
    findColoring(initialState) map { finalState =>
      if (!finalState.isConsistent) {
        //A bit of sanity checking
        throw new RuntimeException("Internal error: graph coloring algorithm returned inconsistent result")
      }
      resultingState = Some(finalState)
    }
  }

  def solution: Option[GraphColoring[V, VC]] = resultingState flatMap (_.getColoring)

  /**
   * Implements backtracking on vertex colors.
   *
   * Selects a vertex with a minimal amount of suitable colors, paints it to the first of them and tries to solve the
   * reduced problem. If no coloring found, tries another suitable color and so on, until solution is found or
   * all colors tried.
   *
   * @param state - state with current partial coloring and suitable colors for vertices
   * @return any state with full and consistent coloring or None, if it's not exists
   */
  private def findColoring(state: ColoringState[V, VC]): Option[ColoringState[V, VC]] = {
    //Filtering out non-colored vertices
    state.vertices.filter(_.color.isEmpty) match {
      case Nil                =>
        //This is final state, all vertices are colored
        Some(state)

      case nonColoredVertices =>
        //Searching for the not colored vertex with minimal suitable colors
        val vertexToPaint = nonColoredVertices.minBy(_.suitableColors.size)
        for (
          color         <- vertexToPaint.suitableColors;
          updatedState  = state.paintVertex(vertexToPaint.vertex, color);
          if updatedState.isConsistent;
          finalState    <- findColoring(updatedState)
        ) {
          //Returning first found final state
          return Some(finalState)
        }
        None
    }
  }

}
