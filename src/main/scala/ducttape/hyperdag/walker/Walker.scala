package ducttape.hyperdag.walker

/** A generalization of the iterator concept to allow parallel traversal
 *  of structured collections (e.g. DAGs). Callers can use a producer-consumer
 *  pattern to accomplish parallel traversal by calling take/complete.
 *  Implementations must be threadsafe and might take the form of an
 *  agenda-based traversal algorithm. */
trait Walker[A] extends Iterable[A] { // TODO: Should this be a TraversableOnce?
  private val self = this

  /** Get the next traversable item. Returns None when there are no more elements */
  private[hyperdag] def take(): Option[A]

  /** Callers must use this method to notify walker that caller is done with each
   *  item so that walker can traverse its dependends
   *  continue: False indicates that no dependents of the specified item should be traversed */
  private[hyperdag] def complete(item: A, continue: Boolean = true)

  /** Get a synchronous iterator (not appropriate for multi-threaded consumers) */
  def iterator() = new Iterator[A] {
    var nextItem: Option[A] = None

    override def hasNext: Boolean = {
      if(nextItem == None) nextItem = self.take
      nextItem != None
    }

    override def next: A = {
      val hazNext = hasNext
      require(hazNext, "No more items. Call hasNext() first.")
      val result: A = nextItem.get
      nextItem = None
      self.complete(result)
      result
    }
  }

  // TODO: Add a .par(j) method that returns a parallel walker
  // j = numCores (as in make -j)
  def foreach[U](j: Int, f: A => U) {
    import java.util.concurrent._
    import collection.JavaConversions._

    val pool = Executors.newFixedThreadPool(j)
    val tasks: Seq[Callable[Unit]] = (0 until j).map(i => new Callable[Unit] {
      override def call {
        var running = true
        while(running) {
          take match {
            case Some(a) => {
              var success = true
              try {
                f(a)
              } catch {
                case t: Throwable => {
                  success = false
                  throw t
                }
              } finally {
                // mark as complete, but don't run any dependencies
                // TODO: Keep a list of tasks that failed?
                //System.err.println("UNSUCCESSFUL, NOT CONTINUING: " + a)
                complete(a, continue=success)
              }
            }
            case None => {
              running = false
            }
          }
        }
        //if(j>1) System.err.println("Worker thread %d of %d joined".format(i+1, j))
      }
    })
    // start running tasks in thread pool
    // wait a few years or until all tasks complete
    val futures = pool.invokeAll(tasks, Long.MaxValue, TimeUnit.MILLISECONDS)
    pool.shutdown
    // call get on each future so that we propagate any exceptions
    futures.foreach(_.get)
  }
}
