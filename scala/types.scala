package ducttape

import collection._

//import ducttape.hyperdag._

object Types {

  class HashMultiMap[A,B] extends mutable.HashMap[A,mutable.Set[B]] with mutable.MultiMap[A,B];

  class Task(val name: String,
             val comments: Seq[String],
             val commands: Seq[String]) {}

  class IOEdge(val files: Map[String,String],     // var -> absolute path
               val params: Map[String,String]) {} // var -> value

  class Branch(val name: String) {}

  class HyperWorkflow(val dag: PackedDag[Task,Branch,IOEdge]) {}
}
