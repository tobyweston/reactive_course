package suggestions

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import scala.concurrent.{ExecutionContext, Future}
import suggestions.observablex.ObservableEx
import ExecutionContext.Implicits.global

@RunWith(classOf[JUnitRunner])
class ObservableExTest extends FunSuite {

  test("Future result is passed to observer") {
    val future = Future(5)
    assert(ObservableEx(future).toBlockingObservable.toList === List(5))
  }

  test("Exception is propagated by observer") {
    val exception = new Throwable("Failure!")
    val future = Future.failed(exception)
    try {
      ObservableEx(future).toBlockingObservable.toList
      assert(false)
    } catch {
      case t: Throwable => assert(t.getCause === exception)
    }
  }
}
