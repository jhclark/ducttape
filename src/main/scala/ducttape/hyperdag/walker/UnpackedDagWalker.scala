package ducttape.hyperdag.walker

import collection._
import java.util.concurrent._
import java.util.Comparator

import ducttape.hyperdag._
import ducttape.util._
import annotation.tailrec

import grizzled.slf4j.Logging


/** Actually, this is an UnpackedHyperDagWalker... 
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
 *  -  for example, that for hyperedges (branches) linked to the same metaedge (branch point),
 *     we want to choose a branch once and consistently use the same branch choice within each derivation.
 * For example usages, see [[ducttape.hyperdag.walker.RealizationMunger]]
 *
 *  the "edge derivation" == the realization
 *
 *  vertexFilter: specifies a specific set of unpacked vertices that may be included in the traversal
 *
 *  Generic Type Definitions:
 * 
 *  D is the "derivation atom type" of the hyperedge. Usually, having D=H is fine, but
 *  D may be any function of H via the function toD() -- in a workflow, this will be a Branch
 *
 *  F is the [[ducttape.hyperdag.walker.RealizationMunger]] state (historically, a Filter state)
 *  NOTE: F must be immutable.HashSet[D] to use DefaultRealizationMunger
 */
class UnpackedDagWalker[V,H,E,D,F](
    val dag: HyperDag[V,H,E],
    munger: RealizationMunger[V,H,E,D,F],
    vertexFilter: VertexFilter[V,H,E,D] = new DefaultVertexFilter[V,H,E,D],
    toD: H => D = new DefaultToD[H],
    traversal: Traversal = Arbitrary)
   (implicit ordering: Ordering[D])
  extends Walker[UnpackedVertex[V,H,E,D]] with Logging {

  /** get an exact replica of this walker, but starting the traversal over again */
  def duplicate(): UnpackedDagWalker[V,H,E,D,F] = {
    new UnpackedDagWalker(dag, munger, vertexFilter, toD, traversal)
  }

  /**
   * An ActiveVertex builds up the information necessary to create an UnpackedVertex
   * from a PackedVertex and a specific HyperEdge that has been selected as part of the
   * current derivation.
   * 
   * TODO: Factor this out into a class all its own?
   * 
   * v is the sink vertex
   * he is the active hyperedge that leads to this vertex
   *   (only one hyperedge will be active per derivation)
   *   (the hyperedge will be None iff this vertex has zero incoming hyperedges in the original hyperdag)
   */
  class ActiveVertex(val v: PackedVertex[V],
                     val he: Option[HyperEdge[H,E]]) {

    // accumulate parent realizations (parallel array with the sources of this hyperedge)
    // each realization (Seq[D]) corresponds to a source vertex of the hyperedge.
    // elements of the Array are isomorphic to the source vertices of the hyperedge (its parents).
    // the variable-sized ListBuffer contains the different realizations of that parent vertex
    // that have been completed so far.
    //
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
    override def toString() = s"${v}(he=${he})"

    var verticesAccepted: Int = 0
    var verticesDiscarded: Int = 0

    /** get the *new* unpackings possible based on all
     * backpointers we've saved so far, but fixing
     * one of the hyperedge sources at a particular realization
     *
     * iFixed: the index of component edge that was just completed, thus triggering this unpacking
     * fixedRealization: the realization of the component edge that was just completed
     * callback: called at the terminal recursive call to create a new UnpackedVertex */
    def unpack(iFixed: Int,
               fixedRealization: Seq[D],
               callback: UnpackedVertex[V,H,E,D] => Unit) {

      // turn derivation state D into an unpacking state F
      val initState = munger.initHyperedge(he.map(heVal => toD(heVal.h)))
      
      // allow user to filter out certain hyperedges from the derivation
      // (e.g. hyperedges from Epsilon vertices)
      // did munger decide to no longer continue with this hyperedge?
      munger.beginHyperedge(v, he, initState) match {
        case None => ;
        case Some(nextState) => {
          s"Began hyperedge ${he}"
          // TODO: Can we avoid re-allocating this over and over?
          val parentReals = new Array[Seq[D]](filled.size)
          parentReals(iFixed) = fixedRealization
          unpack(0, iFixed, parentReals, nextState, callback)
        }
      }
    }

    // recursively scan left-to-right across the array called filled,
    //   building up the combination of hyperedges that forms a realization
    //   we can bail at any point during this process if a RealizationMunger (filter) fails
    // each recursive call to unpack() is isomorphic to one of the component edges of this vertex's active hyperedge
    //   Note: the final call terminal to unpack() overruns the length of the edges (aka filled).
    private def unpack(i: Int,
                       iFixed: Int,
                       parentReals: Array[Seq[D]],
                       prevState: F,
                       callback: UnpackedVertex[V,H,E,D] => Unit) {

      debug(s"Vertex=${v}; he=${he}; i=${i}/${filled.size}; fixed=${iFixed} ${parentReals.toList}")

      // this will immediately be true if he == None
      if (i == filled.size) {
        munger.finishHyperedge(v, he, prevState) match {
          case None => ; // combination could not continue (e.g. a branch graft was not matched)
          case Some(finalState: F) => {
            val finalReal: Seq[D] = munger.toRealization(finalState)
            val sortedReal: Seq[D] = finalReal.toList.sorted(ordering)
            assert(parentReals.toList.forall { _ != null }, s"parentReals for ${v} should not contain null")
            val uv = new UnpackedVertex[V,H,E,D](v, he, sortedReal, parentReals.toList)

            verticesAccepted += 1
            debug(s"Created new unpacked vertex: ${uv}: Parent realizations: ${uv.parentRealizations}")
            callback(uv)
          } 
        }
      } else {
        assert(he != None)
        
        // NOTE: We previously set parentReals(iFixed) to be the fixed realization
        val myParentReals: Iterable[Seq[D]] = if (i == iFixed) Seq(parentReals(iFixed)) else filled(i)

        // only called for debugging
        def getParent(): PackedVertex[V] = {
          // he is guaranteed to be defined if we have more than zero parent reals
          val parents: Seq[PackedVertex[V]] = dag.sources(he.get)
          parents(i)
        }
        val edge: E = he.get.e(i)
        debug(s"Vertex=${v}; he=${he}; i=${i}/${filled.size}; parent=${getParent} edge=${edge}")

        // for each possible realization of the parent vertex of the current component edge
        // (represented by the current recursive call to unpack())...
        for (parentRealization: Seq[D] <- myParentReals) {
          // check if we meet external semantic constraints
          munger.traverseEdge(v, he, edge, parentRealization, prevState) match {
            case None => {
              // illegal state, skip it
              verticesDiscarded += 1
              debug(s"Vertex=${v}; he=${he}; Edge traversal resulted in illegal state. parent${i}=${getParent}; parentReal=${parentRealization}; e${i}=${edge}")
              if (verticesDiscarded % 10000 == 0) {
                debug(s"Traversed ${verticesAccepted} vertices so far (${verticesDiscarded} others discarded)")
              }
            }
            case Some(nextState) => {
              parentReals(i) = parentRealization
              debug(s"Vertex=${v}; he=${he}; Successfully traversing edge. parent${i}=${getParent}; parentReal=${parentRealization}; e${i}=${edge}")
              unpack(i+1, iFixed, parentReals, nextState, callback)
            }
          }
        }
      }
    }
  } // end ActiveVertex

  private val agendaComparator: Comparator[UnpackedVertex[V,H,E,D]] = {
    def assignVertexIndices(): Map[(PackedVertex[V],Seq[D]),Int] = {
      val vertexIDs = new mutable.HashMap[(PackedVertex[V],Seq[D]),Int]
      val walker = dag.unpackedWalker(munger, vertexFilter, toD, Arbitrary)
      walker.foreach { vertex: UnpackedVertex[V,H,E,D] =>
  	val packedParents: Seq[PackedVertex[V]] = dag.parents(vertex.packed)
        val tuples: Seq[(PackedVertex[V],Seq[D])] = packedParents.zip(vertex.parentRealizations)
        val maxParentID: Int = if (tuples.isEmpty) {
          0
        } else {
          // some vertices may not be found due to the vertexFilter -- give these priority -1
  	  val parentIDs: Seq[Int] = tuples.map { tuple => vertexIDs.getOrElse(tuple, -1) }
          parentIDs.max
        }
  	vertexIDs.put( (vertex.packed, vertex.realization), maxParentID+1)
      }
      System.err.println(s"${traversal}: Assigned ${vertexIDs.size} vertex IDs")
      vertexIDs
    }
    
    traversal match { 
      case Arbitrary    => Arbitrary.comparator()
      case BreadthFirst => BreadthFirst.comparator(assignVertexIndices())
      case DepthFirst   => DepthFirst.comparator(assignVertexIndices())
    }
  }

  // NOTE: All synchronization is done via our 2 lock objects
  private val activeRoots = new mutable.HashMap[PackedVertex[V],ActiveVertex]
  private val activeEdges = new mutable.HashMap[HyperEdge[H,E],ActiveVertex]
  private val agenda: java.util.Queue[UnpackedVertex[V,H,E,D]] = 
    new java.util.PriorityQueue[UnpackedVertex[V,H,E,D]](11, agendaComparator)
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
  debug(s"Seeded unpacked HyperDAG walker with roots: ${agenda}")

  val waitingToTakeLock: AnyRef = new Object
  val agendaTakenLock: AnyRef = new Object

  // WARNING: may never return if complete() is not called as expected
  // this is why take/complete are protected and only foreach and iterator are exposed
  // (use foreach to prevent this)
  override def take(): Option[UnpackedVertex[V,H,E,D]] = {

    def poll() = { // atomically get both the taken size and any waiting vertex
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
              debug(s"${takenSize} taken items are still outstanding, we may have work to do yet: ${taken}")
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
          debug(s"Unpacked Vertex filter does not contain: ${v}")
          complete(v, continue=false)
          getSkippingFiltered()
        } else {
          debug(s"Yielding: ${v}")
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
            s"Cannot find active vertex for ${item} in activeRoots/activeEdges")
      val key: ActiveVertex = activeRoots.getOrElse(item.packed, { activeEdges(item.edge.get) })
      debug(s"Completing: ${item}")
      
      completed += item
      val removed = taken.remove(item)
      if (!removed) {
        throw new RuntimeException(s"Completed item ${item} not found in taken set: ${taken}")
      }
      debug(s"Remaining taken vertices after completing ${item}: ${taken}")
      
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
                    debug(s"Adding new vertex to the agenda: ${unpackedV}")
                    trace(s"Seen: ${seen}")
                    agenda.add(unpackedV)
                    seen += unpackedV
                  }
                  // TODO: We could sort the agenda here to impose different objectives...
                })
              trace(s"For active consequent ${activeCon}, setting filled(${iEdge}) = ${item.realization} from item ${item}")
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
