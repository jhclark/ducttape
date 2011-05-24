package ducttape.hyperdag

/**
 * must be threadsafe
 */
trait Walker[A] extends Iterable[A] {

  private val self = this

  /**
   * returns None when there are no more elements
   */
  def take(): Option[A]

  /**
   * notify walker that caller is done with the item so that we know we
   * can traverse its dependends
   */
  def complete(item: A)

  /**
   * get a synchronous iterator (not appropriate for multi-threaded consumers)
   */
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
