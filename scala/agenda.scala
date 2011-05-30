package ducttape.hyperdag


import scala.collection.JavaConversions._
import java.util.concurrent._

import ducttape.Types._

// agenda-based DAG iterator that allows for parallelization
class PackedDagWalker[V](dag: PackedDag[V,_,_]) extends Walker[PackedVertex[V]] {

  private class ActiveVertex(val v: PackedVertex[V]) {
    assert(v != null)
    val filled = new Array[ActiveVertex](dag.inEdges(v).size)
  }

  // taken and agenda must always be jointly locked when updating since if both are zero,
  // it means we're done
  private val active = new mutable.HashMap[PackedVertex[V],ActiveVertex]
                         with mutable.SynchronizedMap[PackedVertex[V],ActiveVertex]
  private val agenda = new ArrayBlockingQueue[ActiveVertex](dag.size)
  private val taken = new mutable.HashSet[ActiveVertex] with mutable.SynchronizedSet[ActiveVertex]
  private val completed = new mutable.HashSet[ActiveVertex] with mutable.SynchronizedSet[ActiveVertex]

  // first, visit the roots
  for(root <- dag.roots.iterator) {
    val actRoot = new ActiveVertex(root)
    agenda.offer(actRoot)
    active += root -> actRoot
  }

  def getCompleted(): Traversable[PackedVertex[V]] = for(act <- completed) yield act.v
  def getRunning(): Traversable[PackedVertex[V]] = for(act <- taken) yield act.v
  def getReady(): Traversable[PackedVertex[V]] = for(act <- agenda) yield act.v
//  def getBlocked(): Traversable[PackedVertex[P]] = 
  
  override def take(): Option[PackedVertex[V]] = {
    if(agenda.size == 0 && taken.size == 0) {
      return None
    } else {
      agenda.synchronized {
        val key: ActiveVertex = agenda.take
        taken += key
        Some(key.v)
      }
    }
  }

  override def complete(item: PackedVertex[V]) = {
    require(active.contains(item), "Cannot find active vertex for %s in %s".format(item, active))
    val key: ActiveVertex = active(item)

    // we always lock agenda & completed jointly
    agenda.synchronized {
      taken -= key
    }

    // first, match fronteir vertices
    for(consequent <- dag.children(key.v)) {
      val activeCon = active.getOrElseUpdate(consequent, new ActiveVertex(consequent))
      var allFilled = true
      val antecedents = dag.parents(consequent)
      for(i <- 0 until activeCon.filled.size) {
        if(key.v == antecedents(i)) {
          activeCon.filled(i) = key
          } else if(activeCon.filled(i) == null) {
            allFilled = false
          }
      }
      // this consequent has all its dependencies fulfilled
      if(allFilled) {
        agenda.synchronized {
          agenda.offer(activeCon)
        }
        // TODO: We could sort the agenda here to impose different objectives...
      }
    }

    // finally visit this vertex
    completed += key
  }
}

// hedge filter allows us to exclude certain hyperedges from
// the edge derivation (e.g. edges without a realization name)
// the "edge derivation" == the realization
// TODO: SPECIFY GOAL VERTICES
class UnpackedDagWalker[V,H,E](val dag: PackedDag[V,H,E],
                               val hedgeFilter: H => Boolean,
                               val selectionFilter: MultiSet[H] => Boolean)
  extends Walker[UnpackedVertex[V,H,E]] {

    class ActiveVertex(val v: PackedVertex[V],
                       val e: Option[HyperEdge[H,E]]) {

    // accumulate parent realizations
    // indices: sourceEdge, whichUnpacking, whichRealization
    val filled = new Array[Set[Set[H]]](if(e.isEmpty) 0 else e.get.e.size)

    private def unpack(i: Int,
                       iFixed: Int,
                       combo: MultiSet[H],
                       parentReals: Array[Set[H]],
                       callback: Seq[H] => Unit) = {

      // hedgeFilter has already been applied
      if(i == filled.size) {
        if(selectionFilter(combo)) {
          callback(new UnpackedVertex[V,H,E](v, e,
                         combo.toList, parentReals.toList))
        }
      } else if(i == iFixed) {
          unpack(i+1, iFixed, combo, parentReals, callback)
      } else {
        // for each backpointer to a realization...
        for(parentRealization: Set[H] <- filled(i)) {
          combo ++= parentRealization
          parentReals(i) = parentRealization
          unpack(i+1, iFixed, combo, parentReals, callback)
          combo --= parentRealization
        }     
      }
    }

    def unpack(iFixed: Int,
               fixedRealization: Set[H],
               callback: Seq[H] => Unit): Seq[Set[H]] = {

      val combo = new MultiSet[H]
      val parentReals = new Array[Set[H]](filled.size)
      parentReals(iFixed) = fixedRealization
      combo += fixedRealization
      unpack(0, iFixed, fixedRealization, combo, parentReals, callback)
    }
    
    // get the *new* unpackings possible based on all
    // backpointers we've saved so far, but fixing
    // one of the hyperedge sources at a particular
    // realization
/*
    def unpack(): Seq[UnpackedVertex[V,H,E]] = {
      realization = new mutable.Set[H]
      for(parentSet <- filled)
        realization ++= parentSet
      if(!e.isEmpty && hedgeFilter(e.get))
        realization += e.get
      val v = dag.sink(e)
      new UnpackedVertex[V,H,E](v, e, realization, filled.toList)
  }
*/

  private val active = new mutable.HashMap[UnpackedVertex[V,H,E],ActiveVertex]
                         with mutable.SynchronizedMap[UnpackedVertex[V,H,E],ActiveVertex]
  // TODO: dag.size might not be big enough for this unpacked version...
  private val agenda = new ArrayBlockingQueue[ActiveVertex](dag.size)
  private val taken = new mutable.HashSet[ActiveVertex] with mutable.SynchronizedSet[ActiveVertex]
  private val completed = new mutable.HashSet[ActiveVertex] with mutable.SynchronizedSet[ActiveVertex]

  // first, visit the roots
  for(root <- dag.roots.iterator) {
    val actRoot = new ActiveVertex(root, None, List.empty)
    agenda.offer(actRoot)
    active += root -> actRoot
  }

  override def take(): Option[UnpackedVertex[V,H,E]] = {
    if(agenda.size == 0 && taken.size == 0) {
      return None
    } else {
      agenda.synchronized {
        val key: ActiveVertex = agenda.take
        taken += key
        return Some(key.toUnpacked)
      }
    }
  }

  override def complete(item: UnpackedVertex[V,H,E]) = {
    require(active.contains(item), "Cannot find active vertex for %s in %s".format(item, active))
    val key: ActiveVertex = active(item)

    // TODO: Are elements of taken properly hashable?

    // we always lock agenda & completed jointly
    agenda.synchronized {
      taken -= key
    }

    // first, match fronteir vertices
    // note: consequent is an edge unlike the packed walker
    for(consequent <- dag.outEdges(key.v)) {
      val activeCon = active.getOrElseUpdate(consequent,
                        new ActiveVertex(consequent, Some(outEdge)))

      val antecedents = dag.sources(consequent)
      for(iEdge <- 0 until activeCon.filled.size) {
        // save a backpointer to the realizations (hyperedge derivation)
        // as of this parent antecedent vertex
        if(item.v == antecedents(iEdge)) {
          // before adding backpointer, take cross-product
          // of unpackings possible when holding this realization
          // fixed
          activeCon.unpack(iEdge, item.realization,
            (realization: Seq[H]) => {
              agenda.synchronized {
                agenda.offer(activeCon)
                // TODO: We could sort the agenda
                // here to impose different objectives...
              }
            })
          activeCon.filled(iEdge) += item.realization
        }
      }
    }

    // finally visit this vertex
    completed += key
  }
}

