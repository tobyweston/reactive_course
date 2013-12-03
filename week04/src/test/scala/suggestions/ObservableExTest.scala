package suggestions

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import scala.concurrent.{ExecutionContext, Future}
import suggestions.observablex.ObservableEx
import ExecutionContext.Implicits.global

@RunWith(classOf[JUnitRunner])
class ObservableExTest extends FunSuite {

  test("Future should give result on Observable") {
    val f = Future(5)
    assert(ObservableEx(f).toBlockingObservable.toList === List(5))
  }

  test("Future may throw on Observable") {
    val exception = new Throwable("Failure!")
    val f = Future.failed(exception)
    try {
      ObservableEx(f).toBlockingObservable.toList
      assert(false)
    } catch {
      case t: Throwable => assert(t === exception)
    }
  }
}
