package nodescala



import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) === 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("List[Future[T]] to Future[List[T]") {
    val odd = Future.always(135)
    val even = Future.always(246)
    val all = Future.all(List(odd, even))

    assert(Await.result(all, 1 second) === List(135, 246))
  }

  test("List[Future[T]] to future[List[T]] with failure") {
    val exception = new Throwable("Failure!")
    val good = Future.always("Success!")
    val bad = Future.failed(exception)
    val all = Future.all(List(good, bad))

    try {
      Await.result(all, 1 second)
      assert(false)
    } catch {
      case t: Throwable => assert(t === exception)
    }
  }

  test("List[Future[T]] to first Future[T]") {
    val any = Future.any(List(Future.never, Future.always(10), Future.never))
    assert(Await.result(any, 1 second) === 10)
  }

  test("List[Future[T]] to first Future[T] with failure") {
    val exception = new Throwable("Failure!")
    val any = Future.any(List(Future.never, Future.failed(exception), Future.never))
    try {
      Await.result(any, 1 second)
      assert(false)
    } catch {
      case t: Throwable => assert(t === exception)
    }
  }

  test("Future completes after delay") {
    val delay = Future.delay(3 seconds)
    assert(Await.result(delay, 4 seconds) === ())
  }

  test("Future not complete before delay") {
    val delay = Future.delay(3 seconds)
    try {
      Await.result(delay, 1 second)
      assert(false)
    } catch {
      case t: Throwable =>
    }
  }

  test("Future.now returns result if completed successfully ") {
    assert(Future.always(3).now === 3)
  }

  test("Future.now throws exception if completed with failure") {
    val exception = new Throwable("Failure!")
    try {
      Future.failed(exception).now
    } catch {
      case t: Throwable => assert(t === exception)
    }
  }

  test("Future.now throws exception if not completed") {
    try {
      Future.never.now
    } catch {
      case t: NoSuchElementException =>
    }
  }

  test("Future.continueWith returns result of function") {
    val result = Future.always(5).continueWith(_.now + 2)
    assert(Await.result(result, 1 seconds) === 7)
  }

  test("Future.continueWith returns failure if completed with failure") {
    val exception = new Throwable("Failure!")
    val result = Future.failed(exception).continueWith(_ => ())

    try {
      Await.result(result, 1 seconds)
      assert(false)
    } catch {
      case t: Throwable => assert(t === exception)
    }
  }

  test("Future.continueWith returns failure if function throws") {
    val exception = new Throwable("Failure!")
    val result = Future.always(5).continueWith(_ => throw exception)

    try {
      Await.result(result, 1 seconds)
      assert(false)
    } catch {
      case t: Throwable => assert(t === exception)
    }
  }

  test("Future.continue returns result of function") {
    val result = Future.always(5).continue(_.get + 2)
    assert(Await.result(result, 1 seconds) === 7)
  }

  test("Future.continue returns failure if completed with failure") {
    val exception = new Throwable("Failure!")
    val result = Future.failed(exception).continue(_ => ())

    try {
      Await.result(result, 1 seconds)
      assert(false)
    } catch {
      case t: Throwable => assert(t === exception)
    }
  }

  test("Future.continue returns failure if function throws") {
    val exception = new Throwable("Failure!")
    val result = Future.always(5).continue(_ => throw exception)

    try {
      Await.result(result, 1 seconds)
      assert(false)
    } catch {
      case t: Throwable => assert(t === exception)
    }
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




