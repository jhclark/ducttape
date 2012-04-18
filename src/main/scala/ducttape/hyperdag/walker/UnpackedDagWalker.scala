package ducttape.hyperdag.walker

import collection._
import java.util.concurrent._

import ducttape.hyperdag._
import ducttape.util._


/** the hedge filter allows us to hide certain hyperedges from
 *  the edge derivation (e.g. edges without a branch name / the default branch name)
 *  however, this filter is cosmetic only and does not result in any additional 
 *
 *  the selectionFilter allows us to prevent exhaustive traversal of all derivations
 *  (i.e. prevent combinatorial explosion)
 *
 *  the constraintFilter allows for higher order semantics to be imposed on the traversal
 *  for example, that for hyperedges (branches) linked to the same metaedge (branch point),
 *  we want to choose a branch once and consistently use the same branch choice within each derivation
 *
 *  the vertex filter allows early termination when it is guaranteed that no future
 *  vertices should be reachable
 *
 *  the comboTransformer is used to implement anti-hyperedges by taking in the result of aggregating
 *  all parent hyper edges and the currently active hyperedge (the combo) and generating a transformed
 *  version (e.g. anti-hyperedges remove entirely one of these edges)
 *
 *  the "edge derivation" == the realization */
// TODO: SPECIFY GOAL VERTICES
class UnpackedDagWalker[V,H,E,F](
        val dag: HyperDag[V,H,E],
        val selectionFilter: SelectionFilter[H] = new DefaultSelectionFilter[H],
        val hedgeFilter: HyperEdgeFilter[H,E] = new DefaultHyperEdgeFilter[H,E],
        val constraintFilter: ConstraintFilter[V,H,F] = new DefaultConstraintFilter[V,H,F],
        val vertexFilter: VertexFilter[V,H,E] = new DefaultVertexFilter[V,H,E],
        val comboTransformer: ComboTransformer[H,E] = new DefaultComboTransformer[H,E])
  extends Walker[UnpackedVertex[V,H,E]] {

  // TODO: Factor this out into a class all its own?
  // TODO: Document why active vertices are isomorphic to hyperedges (or no hyperedge)
  class ActiveVertex(val v: PackedVertex[V],
                     val he: Option[HyperEdge[H,E]]) {

    // accumulate parent realizations (parallel array with the sources of this hyperedge)
    // TODO: Could we make this more space efficient by only accumulating deltas?
    // indices: sourceEdge, whichUnpacking, whichRealization
    val filled = {
      val sz = if (he.isEmpty) 0 else dag.sources(he.get).size
      new Array[mutable.ListBuffer[Seq[H]]](sz)
    }
    for (i <- 0 until filled.size) filled(i) = new mutable.ListBuffer[Seq[H]]

    // TODO: smear
    override def hashCode() = v.hashCode ^ he.hashCode
    override def equals(obj: Any) = obj match { case that: ActiveVertex => (that.v == this.v) && (that.he == this.he) }
    override def toString() = "%s(he=%s)".format(v,he)

    // recursively scan left-to-right across the array called filled,
    // building up the combination of hyperedges that forms a realization
    // we can bail at any point during this process if a filter fails
    private def unpack(i: Int,
                       iFixed: Int,
                       combo: MultiSet[H],
                       parentReals: Array[Seq[H]],
                       prevState: F,
                       callback: UnpackedVertex[V,H,E] => Unit) {

      //err.println("filled : %s %s %d/%d fixed=%d %s %s".format(v,he.getOrElse(""),i,filled.size, iFixed, parentReals.toList, combo))

      // hedgeFilter has already been applied
      if (i == filled.size) {
        comboTransformer(he, combo) match {
          case None => ; // combination could not continue (e.g. anti-hyperedge was not matched)
          case Some(transformedCombo: MultiSet[_]) => {
            if (selectionFilter(transformedCombo)) {
              callback(new UnpackedVertex[V,H,E](v, he, transformedCombo.toList, parentReals.toList))
            }
          } 
        }
      } else {
        //unpack(i+1, iFixed, combo, parentReals, prevState, callback)
        // NOTE: We previously set parentReals(iFixed) to be the fixed realization
        val myParentReals: Iterable[Seq[H]] = if (i == iFixed) Seq(parentReals(iFixed)) else filled(i)
        // for each backpointer to a realization...
        // if we have zero, this will terminate the recursion, as expected
        for (parentRealization: Seq[H] <- myParentReals) {
          // TODO: Get prevState
          // check if we meet external semantic constraints
          constraintFilter(v, prevState, combo, parentRealization) match {
            case None => ; // illegal state, skip it
            case Some(nextState) => {
              //System.err.println(i + " hyperedge: " + he)
              //System.err.println(i + " pre-add combo=" + combo)
              combo ++= parentRealization
              //System.err.println(i + " post-add combo=" + combo)
              parentReals(i) = parentRealization
              //System.err.println(i + " pre-recurse parentRealization=" + parentRealization.toList)
              //System.err.println(i + " pre-recurse parentReals=" + parentReals.toList)
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
      if (!he.isEmpty && hedgeFilter(he.get))
        combo += he.get.h
      val parentReals = new Array[Seq[H]](filled.size)
      parentReals(iFixed) = fixedRealization
      combo ++= fixedRealization
      unpack(0, iFixed, combo, parentReals, constraintFilter.initState, callback)
    }
  } // end ActiveVertex

  // TODO: Remove internal map synchronization
  private val activeRoots = new mutable.HashMap[PackedVertex[V],ActiveVertex]
                   with mutable.SynchronizedMap[PackedVertex[V],ActiveVertex]
  private val activeEdges = new mutable.HashMap[HyperEdge[H,E],ActiveVertex]
                   with mutable.SynchronizedMap[HyperEdge[H,E],ActiveVertex]
  private val agenda = new java.util.LinkedList[UnpackedVertex[V,H,E]]//new ArrayBlockingQueue[UnpackedVertex[V,H,E]](dag.size)
  private val taken = new mutable.HashSet[UnpackedVertex[V,H,E]] with mutable.SynchronizedSet[UnpackedVertex[V,H,E]]
  private val completed = new mutable.HashSet[UnpackedVertex[V,H,E]] with mutable.SynchronizedSet[UnpackedVertex[V,H,E]]

  // first, visit the roots, which are guaranteed not to be packed
  for (root <- dag.roots.iterator) {
    val actRoot = new ActiveVertex(root, None)
    val unpackedRoot = new UnpackedVertex[V,H,E](root, None, Nil, Nil)
    agenda.add(unpackedRoot)
    activeRoots += root -> actRoot
  }

  val waitingToTakeLock: AnyRef = new Object
  val agendaTakenLock: AnyRef = new Object

  // WARNING: may never return if complete() is not called as expected
  // this is why take/compelte are protected and only foreach and iterator are exposed
  // (use foreach to prevent this)
  override def take(): Option[UnpackedVertex[V,H,E]] = {

    // this method just handles getting the next vertex, if any
    // take() must also perform some vertex filtering before returning
    def getNext(): Option[UnpackedVertex[V,H,E]] = {
      def poll() = { // atomically get both the size and any waiting vertex
        val (key, takenSize) = agendaTakenLock.synchronized {
          // after polling the agenda, we must update taken
          // before releasing our lock
          val key = Optional.toOption(agenda.poll())
          val takenSize = taken.size
          key match {
            case Some(v) => taken += v
            case None => ;
          }
          (key, takenSize)
        }        
        (key, takenSize)
      } // poll

      val key: Option[UnpackedVertex[V,H,E]] = waitingToTakeLock.synchronized {
        var (key: Option[UnpackedVertex[V,H,E]], takenSize: Int) = poll()
        while (key == None && takenSize > 0) {
          // wait, releasing our lock until we're notified of changes
          // in state of agenda and taken
          waitingToTakeLock.wait()
          
          val (key1, takenSize1) = poll()
          key = key1
          takenSize = takenSize1
        }
        if (key != None) {
          // * notify other threads that both the agenda and waiting
          //   vertices might be empty
          // * do this outside of agendaTakenLock
          //   to avoid nested locks
          // * do this only if we got a key (thus updating the agenda and taken)
          waitingToTakeLock.notifyAll()
        }
        key
      }
      key
    } // getNext
    
    var result = getNext()
    // some extra machinery to handle vertex-level filtering
    while (result != None && !vertexFilter(result.get)) {
      //System.err.println("U Vertex filter does not contain: " + result.get)
      complete(result.get, continue=false)
      result = getNext()
    }
    result
  }

  override def complete(item: UnpackedVertex[V,H,E], continue: Boolean = true) = {
    require(activeRoots.contains(item.packed) || item.edge.exists(activeEdges.contains(_)),
            "Cannot find active vertex for %s in activeRoots/activeEdges".format(item))
    val key: ActiveVertex = activeRoots.getOrElse(item.packed, activeEdges(item.edge.get))

    // we always lock agenda & completed & taken jointly
    // we must hold on to our lock until all possible consequents
    // of "taken" have been added to the agenda. otherwise, we
    // might terminate early since there will appear to be zero
    // agenda items and zero taken items, even though had
    // we kept take() waiting a bit longer, it would discover
    // new agenda items
    agendaTakenLock.synchronized {
      taken -= item

      if (continue) {
        // first, match fronteir vertices
        // note: consequent is an edge unlike the packed walker
        for (consequentE: HyperEdge[H,E] <- dag.outEdges(key.v)) {
          val consequentV = dag.sink(consequentE)
          def newActiveVertex() = { // thunk
            // get() will throw if we don't get a valid state -- we should be guaranteed a valid state
            // TODO: Can the following line be written prettier? Also, consequentE should never be null
            val h = if (consequentE == null || consequentE.h == null) Seq() else Seq(consequentE.h)
            new ActiveVertex(consequentV, Some(consequentE))
          }
          val activeCon: ActiveVertex = activeEdges.getOrElseUpdate(consequentE, newActiveVertex())
          
          val antecedents = dag.sources(consequentE)
          for (iEdge <- 0 until activeCon.filled.size) {
            // save a backpointer to the realizations (hyperedge derivation)
            // as of this parent antecedent vertex
            if (item.packed == antecedents(iEdge)) {
              // before adding backpointer, take cross-product
              // of unpackings possible when holding this realization fixed.
              // if no complete unpacked vertex exists (i.e. the dependencies for
              // any single realization are not satisfied), the below callback
              // function will not be called during this invocation
              activeCon.unpack(iEdge, item.realization,
                // callback function:
                (unpackedV: UnpackedVertex[V,H,E]) => {
                  // still have the lock on agenda...
                  // TODO: This agenda membership test could be slow O(n)
                  //assert(!agenda.contains(unpackedV) && !taken(unpackedV) && !completed(unpackedV));
                  agenda.add(unpackedV)
                  // TODO: We could sort the agenda here to impose different objectives...
                })
              //System.err.println("For active consequent %s, setting filled(%d) = %s from item %s".format(activeCon, iEdge, item.realization, item))
              activeCon.filled(iEdge) += item.realization
            }
          }
        }
      }

      // finally visit this vertex
      // still holding on to our lock...
      completed += item
    } // and... unlock the agenda and taken buffers
    waitingToTakeLock.synchronized {
      waitingToTakeLock.notifyAll()
    }
  }
}
