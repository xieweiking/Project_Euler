package common

import scala.actors.{ Future, Futures }
import scala.actors.Futures.future

final class ConcurrentTasks[T](timeout: Long) {

    @volatile
    private[this] var futures: List[Future[T]] = Nil
    @volatile
    private[this] var results: List[T] = Nil

    def this(b0: => T, t: Long) = {
        this(t)
        fork(b0)
    }

    def fork(body: => T) = synchronized {
        futures = future(body) :: futures
        this
    }

    def join = synchronized {
        results = for (f <- Futures.awaitAll(timeout, futures: _*) if f.isDefined) yield f.get.asInstanceOf[T]
        futures = Nil
        results
    }

    def join[R](calc: List[T] => R): R = calc(if (results == Nil) join else results)

}

final object ConcurrentTasks {

    def apply[T](timeout: Long) = new ConcurrentTasks[T](timeout)

    def apply[T](body: => T, timeout: Long) = new ConcurrentTasks(body, timeout)

}
