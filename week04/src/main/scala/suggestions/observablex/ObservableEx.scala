package suggestions
package observablex

import scala.concurrent.{Future, ExecutionContext}
import scala.util._
import scala.util.Success
import scala.util.Failure
import java.lang.Throwable
import rx.lang.scala.Observable
import rx.lang.scala.Scheduler
<<<<<<< HEAD
import rx.lang.scala.subjects.ReplaySubject
=======
>>>>>>> master

object ObservableEx {

  /** Returns an observable stream of values produced by the given future.
   * If the future fails, the observable will fail as well.
   *
   * @param f future whose values end up in the resulting observable
   * @return an observable completed after producing the value of the future, or with an exception
   */
<<<<<<< HEAD
  def apply[T](f: Future[T])(implicit execContext: ExecutionContext): Observable[T] = {
    val subject = ReplaySubject[T]()

    f.onComplete {
      case Success(value) => subject.onNext(value); subject.onCompleted()
      case Failure(throwable) => subject.onError(throwable)
    }

    f.onFailure {
      case throwable => subject.onError(throwable)
    }

    subject
  }
=======
  def apply[T](f: Future[T])(implicit execContext: ExecutionContext): Observable[T] = ???
>>>>>>> master

}