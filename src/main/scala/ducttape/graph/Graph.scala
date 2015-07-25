package ducttape.graph

import scala.collection.mutable.HashMap

/**
 *
 *
 * @author Lane Schwartz
 */
class Graph extends Traversable[Vertex] {

  // For convenience, within this class (and only within this class),
  //   the shorthand V may be used to represent the full Vertex[String,VertexContents] type
  //private type V = Vertex[String,VertexContents,EdgeContents]

  // For convenience, within this class (and only within this class),
  //   the shorthand E may be used to represent the full Edge[Vertex[String,VertexContents],EdgeContents] type
  //private type E = Edge[V,EdgeContents]



  private val vertexMap = new HashMap[String,Vertex]


  def contains(id:String) : Boolean = {
    vertexMap.contains(id)
  }


  def addVertex(id:String,vertexType:VertexType, contents:Any, comment: Option[String]=None) : Vertex = {

    val vertex = new Vertex(id, vertexType, contents, comment)

    vertexMap(id) = vertex

    return vertex
  }

  def foreach[U](f: Vertex => U) = vertexMap.values.foreach(f)

/*
  def getVertex(id:String) : Option[V] = {
    return vertexMap.get(id)
  }
*/
  def constructEdge(from:Vertex, to:Vertex, contents:Option[Any]=None, comment: Option[String]=None) : Unit = {

    val edge = new Edge(from, to, contents, comment)
    from.outgoingEdges(to.id) = edge
    to.incomingEdges(from.id) = edge

  }


  override def toString : String = {
    val s = new StringBuilder()

    vertexMap.foreach({ case (key,value) =>

      if (!key.toString().startsWith("$")) {

      s.append("\n************************\n")
//      s.append("task:\t")
//      s.append(key.toString())
//      s.append("\n")

      s.append("\nIncoming edges of ")
      s.append(key.toString())
      s.append(":\n")

      value.incomingEdges.keySet.foreach({ from =>
        s.append(from.toString())
        s.append(" -> ")
        s.append(key.toString())
        s.append("\n")
      })

      s.append("\nOutgoing edges of ")
      s.append(key.toString())
      s.append(":\n")

      value.outgoingEdges.keySet.foreach({ to =>
        s.append(key.toString())
        s.append(" -> ")
        s.append(to.toString())
        s.append("\n")
      })
      }
    })


    return s.toString()
  }

}
