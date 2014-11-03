package sudokusolver.graphcoloring

/**
 * Holds solution of graph coloring problem - list of vertices with their colors
 */
case class GraphColoring[V <: Vertex, VC <: VertexColor](coloring: List[ColoredVertex[V, VC]])
