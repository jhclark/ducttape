package ducttape.hyperdag

/** A generalization of the iterator concept to allow parallel traversal
 *  of structured collections (e.g. DAGs). Callers can use a producer-consumer
 *  pattern to accomplish parallel traversal by calling take/complete.
 *  Implementations must be threadsafe and might take the form of an
 *  agenda-based traversal algorithm. */
trait Walker[+A] extends Iterable[A] {
  private val self = this

  /** Get the next traversable item. Returns None when there are no more elements */
  def take(): Option[A]

  /** Callers must use this method to notify walker that caller is done with each
   *  item so that walker can traverse its dependends */
  def complete(item: A)

  /** Get a synchronous iterator (not appropriate for multi-threaded consumers) */
  def iterator() = new Iterator[A] {
    var nextItem: Option[A] = None

    override def hasNext(): Boolean = {
      if(nextItem == None) nextItem = self.take
      nextItem != None
    }

    override def next(): A = {
      val hazNext = hasNext
      require(hazNext, "No more items. Call hasNext() first.")
      val result: A = nextItem.get
      nextItem = None
      self.complete(result)
      result
    }
  }
}
