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
// Null can be used as the type of FilterState if a constraintFilter is not desired
//
// the "edge derivation" == the realization
// TODO: SPECIFY GOAL VERTICES
class UnpackedDagWalker[V,H,E,FilterState](val dag: HyperDag[V,H,E],
        val selectionFilter: MultiSet[H] => Boolean = (_:MultiSet[H]) => true,
        val hedgeFilter: HyperEdge[H,E] => Boolean = (_:HyperEdge[H,E]) => true,
        val initState: FilterState = null, // shouldn't be null if we specify a constraintFilter
        val constraintFilter: (FilterState, MultiSet[H], Seq[H]) => Option[FilterState]
                              = (prevState:FilterState,_:MultiSet[H],_:Seq[H]) => Some(prevState))
  extends Walker[UnpackedVertex[V,H,E]] {

  type SelectionFilter = MultiSet[H] => Boolean
  type HyperEdgeFilter = HyperEdge[H,E] => Boolean
  type ConstraintFilter[FilterState] = (FilterState, MultiSet[H], Seq[H]) => Option[FilterState]
    
  class ActiveVertex(val v: PackedVertex[V],
                     val he: Option[HyperEdge[H,E]],
                     val state: FilterState) {

    // accumulate parent realizations
    // indices: sourceEdge, whichUnpacking, whichRealization
    val filled = new Array[mutable.ListBuffer[Seq[H]]](if(he.isEmpty) 0 else dag.sources(he.get).size)
    for(i <- 0 until filled.size) filled(i) = new mutable.ListBuffer[Seq[H]]

    // TODO: smear
    override def hashCode = v.hashCode ^ he.hashCode
    override def equals(obj: Any) = obj match { case that: ActiveVertex => (that.v == this.v) && (that.he == this.he) }
    override def toString = "%s(he=%s)".format(v,he)

    private def unpack(i: Int,
                       iFixed: Int,
                       combo: MultiSet[H],
                       parentReals: Array[Seq[H]],
                       prevState: FilterState,
                       callback: UnpackedVertex[V,H,E] => Unit) {

//      println("filled : %s %s %d/%d fixed=%d %s %s".format(v,he.getOrElse(""),i,filled.size, iFixed, parentReals.toList, combo))

      // hedgeFilter has already been applied
      if(i == filled.size) {
        if(selectionFilter(combo)) {
          callback(new UnpackedVertex[V,H,E](v, he,
                         combo.toList, parentReals.toList))
        }
      } else if(i == iFixed) {
        unpack(i+1, iFixed, combo, parentReals, prevState, callback)
      } else {
        // for each backpointer to a realization...
        // if we have zero, this will terminate the recursion, as expected
        for(parentRealization: Seq[H] <- filled(i)) {
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
      unpack(0, iFixed, combo, parentReals, state, callback)
    }
  } // end ActiveVertex

  private val activeRoots = new mutable.HashMap[PackedVertex[V],ActiveVertex]
                   with mutable.SynchronizedMap[PackedVertex[V],ActiveVertex]
  private val activeEdges = new mutable.HashMap[HyperEdge[H,E],ActiveVertex]
                   with mutable.SynchronizedMap[HyperEdge[H,E],ActiveVertex]
  // TODO: dag.size might not be big enough for this unpacked version...
  private val agenda = new ArrayBlockingQueue[UnpackedVertex[V,H,E]](dag.size)
  private val taken = new mutable.HashSet[UnpackedVertex[V,H,E]] with mutable.SynchronizedSet[UnpackedVertex[V,H,E]]
  private val completed = new mutable.HashSet[UnpackedVertex[V,H,E]] with mutable.SynchronizedSet[UnpackedVertex[V,H,E]]

  // first, visit the roots, which are guaranteed not to be packed
  for(root <- dag.roots.iterator) {
    val actRoot = new ActiveVertex(root, None, initState)
    val unpackedRoot = new UnpackedVertex[V,H,E](root, None,
                             List.empty, List.empty)
    agenda.offer(unpackedRoot)
    activeRoots += root -> actRoot
  }

  override def take(): Option[UnpackedVertex[V,H,E]] = {
    if(agenda.size == 0 && taken.size == 0) {
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
    for(consequentE <- dag.outEdges(key.v)) {
      val consequentV = dag.sink(consequentE)
      val activeCon = activeEdges.getOrElseUpdate(consequentE,
                                                  {new ActiveVertex(consequentV, Some(consequentE), key.state)} )

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
  }
}
