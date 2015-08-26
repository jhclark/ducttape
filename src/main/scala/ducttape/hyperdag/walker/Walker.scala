// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.hyperdag.walker

import grizzled.slf4j.Logging

/** A generalization of the iterator concept to allow parallel traversal
 *  of structured collections (e.g. DAGs). Callers can use a producer-consumer
 *  pattern to accomplish parallel traversal by calling take/complete.
 *  Implementations must be threadsafe and might take the form of an
 *  agenda-based traversal algorithm. */
trait Walker[A] extends Iterable[A] with Logging { // TODO: Should this be a TraversableOnce?
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

    // TODO: Write as tail recursion so that breaking out of the loop isn't as complicated
    val pool = Executors.newFixedThreadPool(j)
    
    // kill the entire thread pool if there's an internal failure within the walker
    def catchAndKillPool[T](func: => T):T = {
      try {
        func
      } catch {
          case ie: InterruptedException => throw ie
          case t: Throwable => {
            pool.shutdownNow() // may hang forever otherwise
            throw t
          }
        }
    }
    
    val tasks: Seq[Callable[Unit]] = (0 until j).map(i => new Callable[Unit] {
      override def call {
        var running = true
        while (running) {
          catchAndKillPool { take() } match {
            case Some(a) => {
              var success = true
              try {
                debug("Executing callback for %s".format(a))
                f(a)
              } catch {
                // catch exceptions happening within the callback
                case t: Throwable => {
                  success = false
                  // this won't kill off the main thread until we
                  // call get() on the Future object below
                  throw t
                }
              } finally {
                // mark as complete, but don't run any dependencies
                // TODO: Keep a list of tasks that failed?
                debug("UNSUCCESSFUL, NOT CONTINUING: " + a)
                catchAndKillPool {
                  complete(a, continue=success)
                }
              }
            }
            case None => {
              running = false
            }
          }
        }
        trace("Worker thread %d of %d joined".format(i+1, j))
      }
    })
    // start running tasks in thread pool
    // wait a few years or until all tasks complete
    val futures = pool.invokeAll(tasks, Long.MaxValue, TimeUnit.MILLISECONDS)
    pool.shutdown()
    
    // call get on each future so that we propagate any exceptions
    futures.foreach(_.get)
  }
}
