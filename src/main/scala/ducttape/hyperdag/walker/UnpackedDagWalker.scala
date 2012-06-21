package ducttape.hyperdag.walker

import collection._
import java.util.concurrent._

import ducttape.hyperdag._
import ducttape.util._
import annotation.tailrec

import grizzled.slf4j.Logging


/** Actually, this is an UnpackedHyperDagWalker... 
 * 
 *  the constraintFilter allows for higher order semantics to be imposed on the traversal
 *  for example, that for hyperedges (branches) linked to the same metaedge (branch point),
 *  we want to choose a branch once and consistently use the same branch choice within each derivation.
 *  The initState is used for intializing the beginning of each unpack iteration for each
 *  active hyperedge. The final state is sent to the constraintFilter just before accepting
 *  a candidate unpacked vertex, after which the state is discarded.
 *
 *  the vertex filter allows early termination when it is guaranteed that no future
 *  vertices should be reachable
 *  
 *  The RealizationMunger allows user-defined transformations and filtering of the derivation process.
 *  Uses:
 *  - beginHyperedge() is used to cosmetically remove epsilon hyperedges introduced by the MetaHyperDag
 *    from the derivation
 *  - the finishHyperEdge() is used to implement things such as branch grafts by taking in the result of aggregating
 *    all parent hyper edges and the currently active hyperedge (the combo) and generating a transformed
 *    version (e.g. branch grafts remove entirely one of these edges from the combo and state F). it allows us to
 *    prevent exhaustive traversal of all derivations (i.e. prevent combinatorial explosion) by returning None
 *    if any single component incoming edge of a hyperedge does not match its constraints.
 *
 *  the "edge derivation" == the realization
 *  
 *  The type D is the "derivation type" of the hyperedge. Usually, having D=H is fine, but
 *  D may be any function of H via the function toD() */
class UnpackedDagWalker[V,H,E,D,F](
        val dag: HyperDag[V,H,E],
        munger: RealizationMunger[V,H,E,D,F] = new DefaultRealizationMunger[V,H,E,D,F],
        vertexFilter: VertexFilter[V,H,E,D] = new DefaultVertexFilter[V,H,E,D],
        toD: H => D = new DefaultToD[H])
       (implicit ordering: Ordering[D])
  extends Walker[UnpackedVertex[V,H,E,D]] with Logging {

  // TODO: Factor this out into a class all its own?
  // TODO: Document why active vertices are isomorphic to hyperedges (or no hyperedge)
  // v is the sink vertex
  // he is the active hyperedge that leads to this vertex
  //   (only one hyperedge will be active per derivation)
  //   (the hyperedge will be None iff this vertex has zero incoming hyperedges in the original hyperdag)
  class ActiveVertex(val v: PackedVertex[V],
                     val he: Option[HyperEdge[H,E]]) {

    // accumulate parent realizations (parallel array with the sources of this hyperedge)
    // TODO: Could we make this more space efficient by only accumulating deltas?
    // indices: sourceEdge, whichUnpacking, whichRealization
    val filled = {
      val sz = if (he.isEmpty) 0 else dag.sources(he.get).size
      new Array[mutable.ListBuffer[Seq[D]]](sz)
    }
    for (i <- 0 until filled.size) filled(i) = new mutable.ListBuffer[Seq[D]]

    // TODO: smear
    override def hashCode() = v.hashCode ^ he.hashCode
    override def equals(obj: Any) = obj match { case that: ActiveVertex => (that.v == this.v) && (that.he == this.he) }
    override def toString() = "%s(he=%s)".format(v,he)

    // recursively scan left-to-right across the array called filled,
    // building up the combination of hyperedges that forms a realization
    // we can bail at any point during this process if a filter fails
    private def unpack(i: Int,
                       iFixed: Int,
                       combo: MultiSet[D],
                       parentReals: Array[Seq[D]],
                       prevState: F,
                       callback: UnpackedVertex[V,H,E,D] => Unit) {

      trace("filled : %s %s %d/%d fixed=%d %s %s".format(v,he.getOrElse(""),i,filled.size, iFixed, parentReals.toList, combo))

      if (i == filled.size) {
        munger.finishHyperedge(v, he, combo) match {
          case None => ; // combination could not continue (e.g. a branch graft was not matched)
          case Some(transformedCombo: MultiSet[_]) => {
            val sortedReal: Seq[D] = transformedCombo.toList.sorted(ordering)
            callback(new UnpackedVertex[V,H,E,D](v, he, sortedReal, parentReals.toList))
          } 
        }
      } else {
        // NOTE: We previously set parentReals(iFixed) to be the fixed realization
        val myParentReals: Iterable[Seq[D]] = if (i == iFixed) Seq(parentReals(iFixed)) else filled(i)
        debug {
          // he is guaranteed to be defined if we have more than zero parent reals
          val parents: Seq[PackedVertex[V]] = dag.sources(he.get)
          val parent = parents(i)
          "Applying constraint filter for parent %s".format(parent)
        }
        
        val edges: Seq[E] = he.map(_.e).getOrElse(Nil)

        // for each backpointer to a realization...
        // if we have zero, this will terminate the recursion, as expected
        for ( (parentRealization, e) <- myParentReals.zip(edges)) {
          // parentRealization: Seq[D]
          // e: E
          // check if we meet external semantic constraints
          munger.traverseEdge(v, he, e, prevState, combo, parentRealization) match {
            case None => ; // illegal state, skip it
            case Some(nextState) => {
              trace(i + " hyperedge: " + he)
              trace(i + " pre-add combo=" + combo)
              combo ++= parentRealization
              trace(i + " post-add combo=" + combo)
              parentReals(i) = parentRealization
              trace(i + " pre-recurse parentRealization=" + parentRealization.toList)
              trace(i + " pre-recurse parentReals=" + parentReals.toList)
              unpack(i+1, iFixed, combo, parentReals, nextState, callback)
              trace(i + " post-recurse parentRealization=" + parentRealization.toList)
              trace(i + " pre-remove combo=" + combo)
              combo --= parentRealization
              trace(i + " post-remove combo=" + combo)
            }
          }
        }
      }
    }

    // get the *new* unpackings possible based on all
    // backpointers we've saved so far, but fixing
    // one of the hyperedge sources at a particular realization
    def unpack(iFixed: Int,
               fixedRealization: Seq[D],
               callback: UnpackedVertex[V,H,E,D] => Unit) {

      // allow user to filter out certain hyperedges from the derivation
      // (e.g. hyperedges from Epsilon vertices)
      val hedgeRealOpt: Option[Seq[D]] = {
        val proposedReal = he.map(he => Seq(toD(he.h))).getOrElse(Nil)
        munger.beginHyperedge(v, he, proposedReal)
      }
      
      // did munger decide to no longer continue with this hyperedge?
      hedgeRealOpt match {
        case None => ;
        case Some(hedgeReal) => {
          "Applying constraint filter for hyperedge %s (not parent)".format(he)
          munger.beginEdges(v, he, hedgeReal) match {
            case None => ; // illegal state, skip it
            case Some(nextState) => {
              val combo = new MultiSet[D]
              combo ++= hedgeReal
              val parentReals = new Array[Seq[D]](filled.size)
              parentReals(iFixed) = fixedRealization
              //combo ++= fixedRealization
              unpack(0, iFixed, combo, parentReals, nextState, callback)
            }
          }
        }
      }
    }
  } // end ActiveVertex

  // NOTE: All synchronization is done via our 2 lock objects
  private val activeRoots = new mutable.HashMap[PackedVertex[V],ActiveVertex]
  private val activeEdges = new mutable.HashMap[HyperEdge[H,E],ActiveVertex]
  private val agenda = new java.util.LinkedList[UnpackedVertex[V,H,E,D]]
  private val taken = new mutable.HashSet[UnpackedVertex[V,H,E,D]]
  private val completed = new mutable.HashSet[UnpackedVertex[V,H,E,D]]
  
  // XXX: O(unpacked_vertices)
  // dedupe vertices, made necessary by realization mangling
  // this requires that the realizations be sorted so that equals() works as expected
  private val seen = new mutable.HashSet[UnpackedVertex[V,H,E,D]]

  // first, visit the roots, which are guaranteed not to be packed
  for (root <- dag.roots) {
    val actRoot = new ActiveVertex(root, None)
    val unpackedRoot = new UnpackedVertex[V,H,E,D](root, None, Nil, Nil)
    agenda.add(unpackedRoot)
    activeRoots += root -> actRoot
  }
  debug("Seeded unpacked HyperDAG walker with roots: %s".format(agenda))

  val waitingToTakeLock: AnyRef = new Object
  val agendaTakenLock: AnyRef = new Object

  // WARNING: may never return if complete() is not called as expected
  // this is why take/complete are protected and only foreach and iterator are exposed
  // (use foreach to prevent this)
  override def take(): Option[UnpackedVertex[V,H,E,D]] = {

    def poll() = { // atomically get both the size and any waiting vertex
      agendaTakenLock.synchronized {
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
    }
    
    // this method just handles getting the next vertex, if any
    // take() must also perform some vertex filtering before returning
    def getNext(): Option[UnpackedVertex[V,H,E,D]] = {
      waitingToTakeLock.synchronized {
        // poll until we get an item or there are no more items
        @tailrec def pollUntilDone(): Option[UnpackedVertex[V,H,E,D]] = {
          poll() match {
            case (Some(key), _) => Some(key)
            case (None, 0) => None // no key, no taken items: we're done
            case (None, takenSize) => {
              // keep trying, there could be more
              debug("%d taken items are still outstanding, we may have work to do yet: %s".format(takenSize, taken))
              // wait, releasing our lock until we're notified of changes
              // in state of agenda and taken
              waitingToTakeLock.wait()
              debug("Received notification of newly completed item")
              pollUntilDone() // try again
            }
          }
        }
        
        val key = pollUntilDone()
        // still holding waitingToTakeLock...  
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
    } // getNext
    
    // handle vertex-level filtering
    @tailrec def getSkippingFiltered(): Option[UnpackedVertex[V,H,E,D]] = getNext() match {
      case None => None
      case Some(v) => {
        if (!vertexFilter(v)) {
          debug("Unpacked Vertex filter does not contain: " + v)
          complete(v, continue=false)
          getSkippingFiltered()
        } else {
          debug("Yielding: %s".format(v))
          Some(v)
        }
      }
    }
    getSkippingFiltered()
  }

  override def complete(item: UnpackedVertex[V,H,E,D], continue: Boolean = true) = {
    
    // we always lock agenda & completed & taken jointly
    // we must hold on to our lock until all possible consequents
    // of "taken" have been added to the agenda. otherwise, we
    // might terminate early since there will appear to be zero
    // agenda items and zero taken items, even though had
    // we kept take() waiting a bit longer, it would discover
    // new agenda items
    agendaTakenLock.synchronized {
      require(activeRoots.contains(item.packed) || item.edge.exists(activeEdges.contains(_)),
            "Cannot find active vertex for %s in activeRoots/activeEdges".format(item))
      val key: ActiveVertex = activeRoots.getOrElse(item.packed, { activeEdges(item.edge.get) })
      debug("Completing: %s".format(item))
      
      completed += item
      val removed = taken.remove(item)
      if (!removed) {
        throw new RuntimeException("Completed item %s not found in taken set: %s".format(item, taken))
      }
      debug("Remaining taken vertices after completing %s: %s".format(item, taken))
      
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
          val activeCon: ActiveVertex = activeEdges.getOrElseUpdate(consequentE, { newActiveVertex() })
          
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
                (unpackedV: UnpackedVertex[V,H,E,D]) => {
                  // still have the lock on agenda...
                  // TODO: This agenda membership test could be slow O(n)
                  //assert(!agenda.contains(unpackedV) && !taken(unpackedV) && !completed(unpackedV));
                  
                  // TODO: Should we have to test for uniqueness like this?
                  // NOTE: We only add it to the agenda if we haven't seen "one like it" before
                  // this possibility is introduced by realization mangling
                  if (!seen(unpackedV)) {
                    debug("Adding new vertex to the agenda: " + unpackedV)
                    trace("Seen: " + seen)
                    agenda.add(unpackedV)
                    seen += unpackedV
                  }
                  // TODO: We could sort the agenda here to impose different objectives...
                })
              trace("For active consequent %s, setting filled(%d) = %s from item %s".format(activeCon, iEdge, item.realization, item))
              activeCon.filled(iEdge) += item.realization
            }
          }
        }
      }
    } // and... unlock the agenda and taken buffer

    waitingToTakeLock.synchronized {
      debug("Notifying peer workers of newly completed item")
      waitingToTakeLock.notifyAll()
    }
  }
}
