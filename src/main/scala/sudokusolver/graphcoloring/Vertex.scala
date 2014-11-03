package sudokusolver.graphcoloring

/**
 * Should be implemented in objects that use graph coloring problem
 */
trait Vertex

/**
 * Should be implemented in objects that use graph coloring problem to solve
 */
trait VertexColor

case class ColoredVertex[V <: Vertex, VC <: VertexColor](vertex: V, color: VC)

/**
 * State of vertex during the process of searching the solution
 * @param vertex - vertex itself
 * @param color - option of current color
 * @param suitableColors - set of suitable colors to paint this vertex
 * @param adjacentVertices - list of neighbors in graph
 */
case class VertexState[V <: Vertex, VC <: VertexColor](vertex: V,
                                                       color: Option[VC],
                                                       suitableColors: Set[VC],
                                                       adjacentVertices: List[V])
