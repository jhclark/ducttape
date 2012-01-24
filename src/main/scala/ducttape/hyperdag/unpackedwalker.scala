package ducttape.hyperdag

import collection._
import java.util.concurrent._

import ducttape.util._

// hedge filter allows us to exclude certain hyperedges from
// the edge derivation (e.g. edges without a brach name / the default branch name)
//
// the selectionFilter allows us to prevent exhaustive traversal of all derivations
// (i.e. prevent combinatorial explosion)
//
// the constraintFilter allows for higher order semantics to be imposed on the traversal
// for example, that for hyperedges (branches) linked to the same metaedge (branch point),
// we want to choose a branch once and consistently use the same branch choice within each derivation
//
// Null can be used as the type of F (the FilterState) if a constraintFilter is not desired
//
// the "edge derivation" == the realization
// TODO: SPECIFY GOAL VERTICES
class UnpackedDagWalker[V,H,E,F](val dag: HyperDag[V,H,E],
        val selectionFilter: MultiSet[H] => Boolean = (_:MultiSet[H]) => true,
        val hedgeFilter: HyperEdge[H,E] => Boolean = (_:HyperEdge[H,E]) => true,
        val initState: F = null, // shouldn't be null if we specify a constraintFilter
        val constraintFilter: (F, MultiSet[H], Seq[H]) => Option[F]
                              = (prevState:F,_:MultiSet[H],_:Seq[H]) => Some(prevState))
  extends Walker[UnpackedVertex[V,H,E]] {

  type SelectionFilter = MultiSet[H] => Boolean
  type HyperEdgeFilter = HyperEdge[H,E] => Boolean
  type ConstraintFilter[F] = (F, MultiSet[H], Seq[H]) => Option[F]
    
  class ActiveVertex(val v: PackedVertex[V],
                     val he: Option[HyperEdge[H,E]]) {

    // accumulate parent realizations
    // TODO: Could we make this more space efficient by only accumulating deltas?
    // indices: sourceEdge, whichUnpacking, whichRealization
    val filled = new Array[mutable.ListBuffer[Seq[H]]](if(he.isEmpty) 0 else dag.sources(he.get).size)
    for(i <- 0 until filled.size) filled(i) = new mutable.ListBuffer[Seq[H]]

    // TODO: smear
    override def hashCode = v.hashCode ^ he.hashCode
    override def equals(obj: Any) = obj match { case that: ActiveVertex => (that.v == this.v) && (that.he == this.he) }
    override def toString = "%s(he=%s)".format(v,he)

    // recursively scan left-to-right across the array called filled,
    // building up the combination of hyperedges that forms a realization
    // we can bail at any point during this process if a filter fails
    private def unpack(i: Int,
                       iFixed: Int,
                       combo: MultiSet[H],
                       parentReals: Array[Seq[H]],
                       prevState: F,
                       callback: UnpackedVertex[V,H,E] => Unit) {

//      println("filled : %s %s %d/%d fixed=%d %s %s".format(v,he.getOrElse(""),i,filled.size, iFixed, parentReals.toList, combo))

      // hedgeFilter has already been applied
      if(i == filled.size) {
        if(selectionFilter(combo)) {
          callback(new UnpackedVertex[V,H,E](v, he,
                         combo.toList, parentReals.toList))
        }
      } else {
        //unpack(i+1, iFixed, combo, parentReals, prevState, callback)
        // NOTE: We previously set parentReals(iFixed) to be the fixed realization
        val myParentReals: Iterable[Seq[H]] = if(i == iFixed) Seq(parentReals(iFixed)) else filled(i)
        // for each backpointer to a realization...
        // if we have zero, this will terminate the recursion, as expected
        for(parentRealization: Seq[H] <- myParentReals) {
          // TODO: Get prevState
          // check if we meet external semantic constraints
          constraintFilter(prevState, combo, parentRealization) match {
            case None => ; // illegal state, skip it
            case Some(nextState) => {
              //System.err.println(i + " pre-add combo=" + combo)
              combo ++= parentRealization
              //System.err.println(i + " post-add combo=" + combo)
              parentReals(i) = parentRealization
              //System.err.println(i + " pre-recurse parentRealization=" + parentRealization.toList)
              unpack(i+1, iFixed, combo, parentReals, nextState, callback)
              //System.err.println(i + " post-recurse parentRealization=" + parentRealization.toList)
              //System.err.println(i + " pre-remove combo=" + combo)
              combo --= parentRealization
              //System.err.println(i + " post-remove combo=" + combo)
            }
          }
        }     
      }
    }

    // get the *new* unpackings possible based on all
    // backpointers we've saved so far, but fixing
    // one of the hyperedge sources at a particular realization
    def unpack(iFixed: Int,
               fixedRealization: Seq[H],
               callback: UnpackedVertex[V,H,E] => Unit) {
      
      val combo = new MultiSet[H]
      // allow user to filter out certain hyperedges from the derivation
      if(!he.isEmpty && hedgeFilter(he.get))
        combo += he.get.h
      val parentReals = new Array[Seq[H]](filled.size)
      parentReals(iFixed) = fixedRealization
      combo ++= fixedRealization
      unpack(0, iFixed, combo, parentReals, initState, callback)
    }
  } // end ActiveVertex

  private val activeRoots = new mutable.HashMap[PackedVertex[V],ActiveVertex]
                   with mutable.SynchronizedMap[PackedVertex[V],ActiveVertex]
  private val activeEdges = new mutable.HashMap[HyperEdge[H,E],ActiveVertex]
                   with mutable.SynchronizedMap[HyperEdge[H,E],ActiveVertex]
  // TODO: dag.size might not be big enough for this unpacked version...
  // XXX: Could this ever cause deadlock if the bounded queue is exhausted?
  private val agenda = new ArrayBlockingQueue[UnpackedVertex[V,H,E]](dag.size)
  private val taken = new mutable.HashSet[UnpackedVertex[V,H,E]] with mutable.SynchronizedSet[UnpackedVertex[V,H,E]]
  private val completed = new mutable.HashSet[UnpackedVertex[V,H,E]] with mutable.SynchronizedSet[UnpackedVertex[V,H,E]]

  // first, visit the roots, which are guaranteed not to be packed
  for(root <- dag.roots.iterator) {
    val actRoot = new ActiveVertex(root, None)
    val unpackedRoot = new UnpackedVertex[V,H,E](root, None,
                             List.empty, List.empty)
    agenda.offer(unpackedRoot)
    activeRoots += root -> actRoot
  }

  override def take(): Option[UnpackedVertex[V,H,E]] = {
    // TODO: XXX: If agenda.size is zero, do we have to wait if taken.size == 0?
    // always synchronize on agenda for any agenda/taken operations
    while(agenda.size == 0 && taken.size == 0) {
      agenda.synchronized {
        agenda.wait
      }
    }

    if(agenda.size == 0) {
      return None
    } else {
      agenda.synchronized {
        val key: UnpackedVertex[V,H,E] = agenda.take
        taken += key
        return Some(key)
      }
    }
  }

  override def complete(item: UnpackedVertex[V,H,E]) = {
    require(activeRoots.contains(item.packed) || item.edge.exists(activeEdges.contains(_)),
            "Cannot find active vertex for %s in activeRoots/activeEdges".format(item))
    val key: ActiveVertex = activeRoots.getOrElse(item.packed, activeEdges(item.edge.get))

    // we always lock agenda & completed & taken jointly
    agenda.synchronized {
      taken -= item
    }

    // first, match fronteir vertices
    // note: consequent is an edge unlike the packed walker
    for(consequentE: HyperEdge[H,E] <- dag.outEdges(key.v)) {
      val consequentV = dag.sink(consequentE)
      def newActiveVertex = { // thunk
        // get() will throw if we don't get a valid state -- we should be guaranteed a valid state
        // TODO: Can the following line be written prettier? Also, consequentE should never be null
        val h = if(consequentE == null || consequentE.h == null) Seq() else Seq(consequentE.h)
        new ActiveVertex(consequentV, Some(consequentE))
      }
      val activeCon = activeEdges.getOrElseUpdate(consequentE, newActiveVertex)

      val antecedents = dag.sources(consequentE)
      for(iEdge <- 0 until activeCon.filled.size) {
        // save a backpointer to the realizations (hyperedge derivation)
        // as of this parent antecedent vertex
        if(item.packed == antecedents(iEdge)) {
          // before adding backpointer, take cross-product
          // of unpackings possible when holding this realization fixed.
          // if no complete unpacked vertex exists (i.e. the dependencies for
          // any single realization are not satisfied), the below callback
          // function will not be called during this invocation
          activeCon.unpack(iEdge, item.realization,
            // callback function:
            (unpackedV: UnpackedVertex[V,H,E]) => {
              agenda.synchronized {
                // TODO: This agenda membership test could be slow O(n)
                //assert(!agenda.contains(unpackedV) && !taken(unpackedV) && !completed(unpackedV));
                agenda.offer(unpackedV)
                // TODO: We could sort the agenda here to impose different objectives...
              }
            })
          activeCon.filled(iEdge) += item.realization
        }
      }
    }

    // finally visit this vertex
    agenda.synchronized {
      completed += item
    }
    agenda.notifyAll
  }
}
