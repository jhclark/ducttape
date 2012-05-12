package ducttape.util

import ducttape.workflow.Branch
import collection._
import ducttape.workflow.BranchPoint
import scala.math.Ordering
import ducttape.workflow.Realization

object PrefixTreeMap {
  // TODO: Refactor as builder and immutable version which is pre-sorted
  private[util] class TrieNode[K,SortK,V] {
    var value: Option[V] = None
    val children = new mutable.HashMap[K,TrieNode[K,SortK,V]]
    
    override def toString() = "(" + value + " " + children.map { case (k,v) => k+":"+v }.mkString + ")"
  }
}

/**
 * A varation on a prefix tree (trie). Stores structured keys (*sets* of K objects).
 * User can query using *supersets* and we will return the value associated with
 * the largest matching (maximal) subset (if any). (What about sets having the same size?)
 */
class PrefixTreeMap[K,SortK,V](sorter: K => SortK)
                              (mapping: Iterable[(Iterable[K], V)])
                              (implicit ordering: Ordering[SortK]) {
  
  import PrefixTreeMap.TrieNode
  
  private[util] val trieRoot = new TrieNode[K,SortK,V]
  
  {
    // TODO: Can we guarantee this before hand? Or make it otherwise faster?
    // sort seqs by branch point names
    val sortedMapping: Iterable[(Seq[K], V)] = mapping.map { case (keySet, value) => {
      (keySet.toSeq.sortBy(sorter), value)
    }}
    def insert(remaining: Seq[K], value: V, curNode: TrieNode[K,SortK,V]) {
      remaining.headOption match {
        case None => curNode.value = Some(value)
        case Some(head) => {
          val sortK = sorter(head)
          val nextNode = curNode.children.getOrElseUpdate(head, {new TrieNode})
          insert(remaining.drop(1), value, nextNode)
        }
      }
    }
    for( (keySet, value) <- sortedMapping) {
      insert(keySet, value, trieRoot)
    }
  }
  
  def get(superset: Iterable[K]): Option[V] = {
    
    // return the maximal matching subset of the query within our local trie
    def query(matchedNode: TrieNode[K,SortK,V], remainingQuery: Seq[K], prevMatch: Option[V]): Option[V] = {
      remainingQuery.headOption match {
        case None => prevMatch // terminate recursion
        case Some(nextQueryElem) => {
          matchedNode.children.get(nextQueryElem) match {
            case None => query(matchedNode, remainingQuery.drop(1), prevMatch) // skip this query entry
            case Some(nextNode) => query(nextNode, remainingQuery.drop(1), nextNode.value) // better match!
          }
        }
      }
    }
    
    // TODO: Pre-sort? Or would that take more time since the common case is one element?
    query(this.trieRoot, superset.toSeq.sortBy(sorter), None)
  }
  
  def apply(superset: Iterable[K]): V = get(superset) match {
    case Some(v) => v
    case None => throw new NoSuchElementException(superset.toString + " :: " + trieRoot.toString)
  }
  def contains(superset: Iterable[K]): Boolean = get(superset).isDefined
  
  override def toString() = trieRoot.toString
}

class PrefixTreeSet[K,SortK](sorter: K => SortK)
                            (elements: Iterable[Iterable[K]])
                            (implicit ordering: Ordering[SortK]) {
  
  private[util] val delegate = new PrefixTreeMap[K,SortK,Null](sorter)(elements.map{(_, null)})
  
  def apply(superset: Iterable[K]): Boolean = delegate.contains(superset)
}

object BranchPrefixTreeMap {
  def sorter(branch: Branch): String = branch.branchPoint.name
}

// can map from an active branch map Map[String,Branch]
// to a value, such as the set of Specs associated with a particular branch
class BranchPrefixTreeMap[V](mapping: Iterable[(Seq[Branch], V)])
  extends PrefixTreeMap[Branch,String,V](BranchPrefixTreeMap.sorter)(mapping) {
  
  def get(real: Realization): Option[V] = super.get(real.branches)
  def apply(real: Realization): V = super.apply(real.branches)
  // get the only element in the map
  def only(): V = trieRoot.children.values.toSeq match {
    case Seq(oneChild) => oneChild.value.get
    case _ => throw new RuntimeException("Expected a single element")
  }
}