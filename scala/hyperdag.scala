package ducttape.hyperdag

import collection.mutable._
import collection.immutable

import ducttape.Types._

// RDag - Realization HyperDAG, which encodes the realizations at each HyperVertex
// HyperDag - Encodes all DAGs in HyperDAG

class Deque[T] {
  private var head = new DoubleLinkedList[T]
  private var tail = head

  def first(): T = {
    head.elem
  }

  def last(): T = {
    tail.elem
  }

  def removeHead(n: Int = 1) = {
  }

  def removeTail(n: Int = 1) = {
  }

  def append(item: T) = {
  }

  def prepend(item: T) = {
  }

  def size(): Int = {
    0
  }
}

class RDagBuilder {
  def withParent(parent: RDag, isPacking: Boolean) = {
    
  }

  def build(): RDag = {
    new RDag
  }
}

// realization DAG
class RDag {

  // accumulated variables
  private val accVars = new HashSet[RVar]
  //val sortedVars = new Array[RVar](?)
  private val varOrder = new HashMap[RVar,Int]

  val packing = true
  val name = "ohai"

  def combinationIterator(callback: RInstList => Unit,
                          selection: RSelection) {

    val combo = new Array[RInst](accVars.size)
    val frontier = new Deque[RVertex]
    val visited = new HashSet[RVertex]
    traverseFrontier(callback, combo, selection, frontier, visited)
  }

  private def traverseFrontier(callback: RInstList => Unit,
                               combo: Array[RInst],
                               selection: RSelection,
                               frontier: Deque[RVertex],
                               visited: Set[RVertex]): Unit = {

    println("LOG: %s: traverseFronteir: fronteir = %s; visited = %s; combo = %s".format(name, frontier, visited, combo))

    if(frontier.size == 0) {
      // convert from an array possibly containing nulls to a list, never
      // containing null
      callback(toList(combo))
    } else {
      val vertex: RVertex = frontier.first
      visited += vertex
      val pos = varOrder.get(vertex.rvar).getOrElse(-1)
      assert(pos != -1, "Canonical order for %s not found".format(vertex.rvar))
      
      for(inst <- vertex.rvar.values) {
        combo(pos) = inst
        if(selection == null || selection.couldContain(combo)) {
          val antecedents: Set[RVertex] = vertex.incomingHyperedges.getOrElse(inst, Set.empty)

          var addedDepth = 0
          for(ant <- antecedents if !visited(ant)) {
            frontier.append(ant)
            addedDepth += 1
          }
          traverseFrontier(callback, combo, selection, frontier, visited)
          frontier.removeTail(addedDepth)
        } else {
          // selection could not contain this combo
        }
        combo(pos) = null
      }

      frontier.prepend(vertex)
      visited -= vertex
    }
  }

  private def toList(arr: Array[RInst]): immutable.IndexedSeq[RInst] = {
    for(i <- 0 until arr.length if arr(i) != null) yield arr(i)
  }
}

class RSelectionSet {
  val included = new HashSet[RInst]

  def containsAll(instances: Array[RInst]): Boolean = {
    for(inst <- instances if inst != null) {
      if(!included(inst)) {
        return false
      }
    }
    return true
  }
}

// realization selection
class RSelection {

  val selectionSets = new ListBuffer[RSelectionSet]

  /**
   * Determine if this selection could potentially contain the given combo
   * 
   * @param combo The combination of realization instances, which
   *              can contain null entries 
   */
  def couldContain(combo: Array[RInst]): Boolean = {
    for(set <- selectionSets) {
      if(set.containsAll(combo)) {
        return true
      }
    }
    return false
  }
}

// realization instance
class RInst {
  val rvar = new RVar
}

// realization variable
class RVar {
  val values = new ListBuffer[RInst]
}

// realization vertex
class RVertex { 
  val rvar = new RVar
  // TODO: Multimap type
  val incomingHyperedges = new HashMap[RInst, Set[RVertex]]
}

class HyperDag[V,E] (xxx: V) {
  val hypervertices = new ListBuffer[HyperVertex[V]]

  def add(v: V) = {
    
  }

  def hyperIterator(): Seq[HyperVertex[V]] = {
    for(hv <- hypervertices) yield {
      new HyperVertex[V];
    }
  }

  def iterator(): Seq[V] = {
    for(hv <- hypervertices) yield {
      xxx
    }
  }
}

class HyperVertex[V] {
  
}

class HyperEdge[E] {
      
}
