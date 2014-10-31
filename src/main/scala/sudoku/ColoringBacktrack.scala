package sudoku

object ColoringBacktrack {

  type Coloring = List[(VertexIndex, VertexColor)]

  def findColoring(state: ColoringState): Option[Coloring] = {
    if (state.isComplete) {
      return state.getColoring
    }

    for {
      nonColoredVertex  <- state.vertices.filter(_.color.isEmpty)
      color             <- nonColoredVertex.suitableColors
      updatedState      = state.paintVertex(nonColoredVertex.index, color)
      if updatedState.isConsistent
      coloring          <- findColoring(updatedState)
    } {
      return Some(coloring)
    }

    None
//
//    state.vertices.filter(_.color.isEmpty) foreach { vertex =>
//      vertex.suitableColors foreach { color =>
//        val updatedState = state.paintVertex(vertex.index, color)
//        if (updatedState.isConsistent) {
//          val result = findColoring(updatedState)
//          if (result.isDefined) {
//            return result
//          }
//        }
//      }
//    }
//
//    None
  }

}
