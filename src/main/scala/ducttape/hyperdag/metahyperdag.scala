package ducttape.hyperdag

import collection._
import ducttape.util._

// an implementation of MetaHyperDAGs based on transforming
// meta-edges into epsilon vertices (but these are hidden from the user)

// m is the payload of this metahyperedge
// In a MetaHyperDag, all vertices have only MetaEdges as their only inputs.
// A derivation rooted at a vertex consists of each input MetaEdges having
// exactly one input HyperEdge assigned to it. Unlike a HyperDag, in which
// only one HyperEdge per vertex can be active in a single derivation,
// a MetaHyperDag requires that *all* input MetaEdges be active for each
// vertex.
class MetaEdge[M,H,E](private[hyperdag] val epsilonV: PackedVertex[_],
                      val m: M,
                      val hyperedges: Seq[HyperEdge[H,E]])  {
  override def hashCode = epsilonV.id
  override def equals(that: Any) = that match {
    case other: MetaEdge[_,_,_] => (other.epsilonV.id == this.epsilonV.id)
  }
  override def toString = m.toString + " " + hyperedges.toString
}

// Unlike a plain UnpackedVertex, with a MetaHyperDAG, our choice of incoming
// Meta edge may affect what our previous constraint state is (since we traverse
// multiple hyperedges). Therefore, we must include a mapping from metaEdge to prevState
//
// All incoming metaedges to a vertex will be assigned exactly one of their member hyperedges.
// This is represented by the sequence "edges". The parentRealizations are actually parallel
// with the number of incoming metaedges for a vertex. The next sequence is parallel with the
// number of edges in the active hyperedge for our current path (the one in this unpacking).
// TODO: Document an example of how to iterate over this easily with zip()
//
// Like UnpackedVertex, this interface explicitly avoids giving unpacked vertices as
// parents so that we can eventually discard more of the explored space
// the prevState can store information such as "what realizations does my parent have?"
// NOTE: It's actually the incoming edges that are meta -- not the vertex itself
class UnpackedMetaVertex[V,H,E](val packed: PackedVertex[V],
                                val edges: Seq[HyperEdge[H,E]],
                                val realization: Seq[H],
                                val parentRealizations: Seq[Seq[Seq[H]]],
                                private[hyperdag] val dual: UnpackedVertex[V,H,E]) {
  // TODO: More smearing of hash codes
  override def hashCode = packed.id ^ realization.hashCode
  override def equals(that: Any) = that match {
    case other: UnpackedMetaVertex[_,_,_] => (other.packed.id == this.packed.id) && (other.realization == this.realization)
  }
  override def toString = packed.toString + " (realization=" + realization.toString +")"
}

// our only job is to hide epsilon vertices during iteration
// TODO: Create trait for getCompleted etc
class PackedMetaDagWalker[V](val dag: MetaHyperDag[V,_,_,_])
  extends Walker[PackedVertex[V]] {
  

  val delegate = new PackedDagWalker[V](dag.delegate)

  // TODO: Filter epsilons and phantoms from completed
  def getCompleted(): Traversable[PackedVertex[V]] = delegate.getCompleted
  def getRunning(): Traversable[PackedVertex[V]] = delegate.getRunning
  def getReady(): Traversable[PackedVertex[V]] = delegate.getReady

  override def complete(item: PackedVertex[V]) = delegate.complete(item)

  override def take(): Option[PackedVertex[V]] = {
    var result = delegate.take
    // never return epsilon vertices nor phantom vertices
    // we're guaranteed to only have one epsilon vertex in between vertices (no chains)
    // but phantom vertices break this
    while(!result.isEmpty && (dag.shouldSkip(result.get))) {
      complete(result.get)
      result = delegate.take
    }
    return result
  }
}

// our only job is to hide epsilon vertices during iteration
// see UnpackedDagWalker for definitions of filter and state types
// F is the FilterState
// TODO: Allow
class UnpackedMetaDagWalker[V,M,H,E,F](val dag: MetaHyperDag[V,M,H,E],
        val selectionFilter: MultiSet[H] => Boolean = Function.const[Boolean,MultiSet[H]](true)_,
        val hedgeFilter: HyperEdge[H,E] => Boolean = Function.const[Boolean,HyperEdge[H,E]](true)_,
        val initState: F,
        val constraintFilter: (PackedVertex[V], F, MultiSet[H], Seq[H]) => Option[F])
  extends Walker[UnpackedMetaVertex[V,H,E]] {

  private val delegate = new UnpackedDagWalker[V,H,E,F](dag.delegate, selectionFilter, hedgeFilter, initState, constraintFilter)

  // we must be able to recover the epsilon-antecedents of non-epsilon vertices
  // so that we can properly populate their state maps
  // unfortunately, this kills space complexity, since we must hold on to these until
  // all members of a packed vertex have been unpacked
  // TODO: Right now, we don't dectect when all members of a packed vertex have
  // been unpacked so that we can reclaim space. Could we refcount them?
  private val epsilons = new mutable.HashMap[(PackedVertex[V],Seq[H]), UnpackedVertex[V,H,E]]

  override def complete(item: UnpackedMetaVertex[V,H,E]) = delegate.complete(item.dual)

  override def take(): Option[UnpackedMetaVertex[V,H,E]] = {
    var result: Option[UnpackedVertex[V,H,E]] = delegate.take
    // never return epsilon vertices nor phantom verties
    // we're guaranteed to only have one epsilon vertex in between vertices (no chains)
    // but phantom vertices break this
    while(!result.isEmpty && dag.shouldSkip(result.get.packed)) {
      //println("TAKE SKIPPING: " + result)
      val uv = result.get
      delegate.complete(uv)
      if(dag.isEpsilon(uv.packed)) {
        // TODO: We'd really prefer not to store these...
        epsilons += (uv.packed, uv.realization) -> uv
      }
      result = delegate.take
    }
    //println("TAKING: " + result)

    return result match {
      case None => None
      case Some(raw: UnpackedVertex[V,H,E]) => {
        
        val activeEdges = new mutable.ListBuffer[HyperEdge[H,E]]
        val metaParentReals = new mutable.ListBuffer[Seq[Seq[H]]]

        dag.delegate.parents(raw.packed) match {
          // skip the case of phantom parents
          case Seq(singleParent) if(dag.isPhantom(singleParent)) => ;
          case parents => {
            assert(parents.size == raw.parentRealizations.size, "Parent size %d != parentReal.size %d".format(parents.size, raw.parentRealizations.size))
            // for parallel to number of incoming meta edges
            for((parentEpsilonV: PackedVertex[V], parentEpsilonReals: Seq[Seq[H]]) <- parents.zip(raw.parentRealizations)) {
              val parentEpsilonUV: UnpackedVertex[V,H,E] = epsilons( (parentEpsilonV, parentEpsilonReals) )
              // use this Seq[Seq[H]], which is parallel to the active hyperedge
              activeEdges += parentEpsilonUV.edge.get
              metaParentReals += parentEpsilonUV.parentRealizations
            }
          }
        }
        Some(new UnpackedMetaVertex[V,H,E](raw.packed, activeEdges, raw.realization, metaParentReals, raw))
      }
    }
  }
}

// immutable
//
// walker will note when epsilon vertices are completed, but not actually
// return them to the user
//
// TODO: Pass filters to dag walker
class MetaHyperDag[V,M,H,E](private[hyperdag] val delegate: HyperDag[V,H,E],
                              private[hyperdag] val metaEdgesByEpsilon: Map[PackedVertex[V],MetaEdge[M,H,E]],
                              private[hyperdag] val epsilonEdges: Set[HyperEdge[H,E]],
                              private[hyperdag] val phantomVertices: Set[PackedVertex[V]]) {

  // don't include epsilon vertices
  val size: Int = delegate.size - metaEdgesByEpsilon.size - phantomVertices.size

  private[hyperdag] def isEpsilon(v: PackedVertex[V]) = metaEdgesByEpsilon.contains(v)
  private[hyperdag] def isPhantom(v: PackedVertex[V]) = phantomVertices.contains(v)
  private[hyperdag] def shouldSkip(v: PackedVertex[V]) = isEpsilon(v) || isPhantom(v)
  private[hyperdag] def isEpsilon(h: HyperEdge[H,E]) = epsilonEdges(h)

  def packedWalker() = new PackedMetaDagWalker[V](this) // TODO: Exclude epsilons from completed, etc.

  def unpackedWalker[F](initFilterState: F, 
                        constraintFilter: (PackedVertex[V], F, MultiSet[H], Seq[H]) => Option[F]) = {
    // TODO: Combine this hedgeFilter with an external one?
    // TODO: Allow filtering baseline from realizations
    // TODO: Exclude epsilons from completed, etc.
    // TODO: Map epsilons and phantoms for constraintFiler in this class instead of putting
    // the burden on the filter
    def selectionFilter(selection: MultiSet[H]) = true
    def hedgeFilter(h: HyperEdge[H,E]) = !isEpsilon(h)

    new UnpackedMetaDagWalker[V,M,H,E,F](this, selectionFilter, hedgeFilter, initFilterState, constraintFilter)
  }

  def inMetaEdges(v: PackedVertex[V]): Seq[MetaEdge[M,H,E]]
    = for(parent <- delegate.parents(v)) yield metaEdgesByEpsilon(parent)
  def inHyperEdges(me: MetaEdge[M,H,E]): Seq[HyperEdge[H,E]]
    = delegate.inEdgesMap.getOrElse(me.epsilonV, Seq.empty)
  def outHyperEdges(v: PackedVertex[V]): Seq[HyperEdge[H,E]]
    = delegate.outEdgesMap.getOrElse(v, Seq.empty)
  def outMetaEdge(he: HyperEdge[H,E]): MetaEdge[M,H,E] = {
    return metaEdgesByEpsilon(delegate.sink(he))
  }

  private def skipEpsilonsAndPhantom(v: PackedVertex[_],
                                     func: PackedVertex[_] => Seq[PackedVertex[V]])
    : Seq[PackedVertex[V]] = {

    import System._
    var directParents = func(v)
    // check if we can just return these parents without modification
    if(directParents.exists(p => shouldSkip(p))) {
      // replace the epsilon vertices by their parents
      // it's guaranteed that those parents are not epsilon vertices themselves
      // TODO: This could be made into an ArrayBuffer if this turns out to be inefficient
      directParents.flatMap({
        case p if(isEpsilon(p)) => func(p).filter(!isPhantom(_)) // skip any phantom grandparents
        case p if(isPhantom(p)) => Seq.empty // skip entirely
        case p => Seq(p) // direct parent is normal
      })
    } else {
      directParents
    }    
  }

  def vertices() = delegate.vertices.filter(!shouldSkip(_))
  def parents(v: PackedVertex[_]): Seq[PackedVertex[V]] = skipEpsilonsAndPhantom(v, delegate.parents)
  def children(v: PackedVertex[_]): Seq[PackedVertex[V]] = skipEpsilonsAndPhantom(v, delegate.children)
  
  def sources(e: MetaEdge[M,H,E]): Seq[PackedVertex[V]] = {
    val srcs = new mutable.ArrayBuffer[PackedVertex[V]]
    for(inEdge <- inHyperEdges(e)) {
      srcs ++= delegate.sources(inEdge)
    }
    srcs
  }
  def sources(e: HyperEdge[H,E]): Seq[PackedVertex[V]] = delegate.sources(e)
  def sink(e: HyperEdge[H,E]): PackedVertex[V] = sink(outMetaEdge(e))
  def sink(e: MetaEdge[M,H,E]): PackedVertex[V] = delegate.children(e.epsilonV).head

  def toGraphViz(): String = delegate.toGraphViz(vertices, parents, {v => v.toString})

  // visualize with all epsilon and phantom vertices
  def toGraphVizDebug(): String = {
    def stringify(v: PackedVertex[V]): String = v match {
        case p if isPhantom(p) => "Phantom#" + p.id
        case p if isEpsilon(p) => "Epsilon:" + metaEdgesByEpsilon(p).m.toString + "#" + p.id
        case p => p.toString
    }
    delegate.toGraphViz(delegate.vertices, delegate.parents, stringify)
  }
}

/** epsilonValue is just a dummy value that will never be given back to the user
  * (needed since we can't directly created objects with generic types)
  *
  * <img src="x.gif" /> */
class MetaHyperDagBuilder[V,M,H,E](private val epsilonV: V = null,
                                   private val epsilonH: H = null,
                                   private val epsilonE: E = null) {
  private val delegate = new HyperDagBuilder[V,H,E]
  private val metaEdgesByEpsilon = new mutable.HashMap[PackedVertex[V], MetaEdge[M,H,E]]
  private val metaEdgeSinks = new mutable.HashMap[PackedVertex[V], mutable.ArrayBuffer[PackedVertex[V]]]
  private val phantomVertices = new mutable.HashSet[PackedVertex[V]]
  
  def addVertex(v: V): PackedVertex[V] = delegate.addVertex(v)

  // phantom edge: an edge with no source vertices
  // phantom vertex: the source vertex of a phantom edge
  // used in ducttape for literal file paths, which may be linked with branches
  // but don't have any executable code associated with them
  // TODO: Should this be incorporated into HyperDAG, too?
  // this method allows sources to be optional, indicating that edges *may*
  // have phantom source vertices
  def addPhantomMetaEdge(m: M,
                  hyperEdgeInfo: Seq[(H, Seq[(Option[PackedVertex[V]],E)])],
                  sink: PackedVertex[V]): MetaEdge[M,H,E] = {
    val hyperEdgeInfoP = for( (h, edgeInfo) <- hyperEdgeInfo) yield {
      val edgeInfoP = for( (vOpt, e) <- edgeInfo) yield {
        vOpt match {
          case Some(v) => (v, e)
          case None => {
            val phantomV = delegate.addVertex(epsilonV)
            phantomVertices += phantomV
            (phantomV, e)
          }
        }
      }
      (h, edgeInfoP)
    }
    addMetaEdge(m, hyperEdgeInfoP, sink)
  }

  def addMetaEdge(m: M,
                  hyperEdgeInfo: Seq[(H, Seq[(PackedVertex[V],E)])],
                  sink: PackedVertex[V]): MetaEdge[M,H,E] = {

    // TODO: Don't always alternate between normal and epsilon vertices to save some memory

    val meEpsilonV = delegate.addVertex(epsilonV)
    val hyperedges = new mutable.ArrayBuffer[HyperEdge[H,E]](hyperEdgeInfo.size)
    for(heInfo: (H, Seq[(PackedVertex[V],E)]) <- hyperEdgeInfo) {
      val(h, edgeInfo) = heInfo
      for( (srcV, e) <- edgeInfo) assert(srcV != sink, "This meta-edge would create a cycle")
      val he = delegate.addHyperEdge(h, edgeInfo, meEpsilonV)
      hyperedges += he
    }

    // associate this epsilon vertex with its metaedge
    val me = new MetaEdge[M,H,E](meEpsilonV, m, hyperedges)
    metaEdgesByEpsilon += meEpsilonV -> me

    // delay adding the meta-edges to the delegate builder
    // until we know we've accumulated all of them
    metaEdgeSinks.getOrElseUpdate(sink, {new mutable.ArrayBuffer[PackedVertex[V]]}) += meEpsilonV
    
    return me
  }

  def build() = {
    // add single hyperedge that represents all incoming meta-edges
    // for all non-epsilon vertices
    val epsilonEdges = new mutable.HashSet[HyperEdge[H,E]]
    for( (sink, epsilonVertices) <- metaEdgeSinks) {
      val epsilonParents = for(v <- epsilonVertices) yield (v, epsilonE)
      val he = delegate.addHyperEdge(epsilonH, epsilonParents, sink)
      epsilonEdges += he
    }

    new MetaHyperDag[V,M,H,E](delegate.build, metaEdgesByEpsilon, epsilonEdges, phantomVertices)
  }
}
