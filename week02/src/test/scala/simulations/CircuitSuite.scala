package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)

    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    in2.setSignal(false)
    run
    assert(out.getSignal === true, "or 2")

    in1.setSignal(false)
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 3")

    in1.setSignal(true)
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 4")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)

    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    in2.setSignal(false)
    run
    assert(out.getSignal === true, "or 2")

    in1.setSignal(false)
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 3")

    in1.setSignal(true)
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 4")
  }

  test("demux example, no control") {
    val in, out = new Wire
    demux(in, Nil, List(out))

    in.setSignal(false)
    run
    assert(out.getSignal === false, "demux 1")

    in.setSignal(true)
    run
    assert(out.getSignal === true, "demux 2")
  }

  test("demux example, 1 control") {
    val in, c, out1, out2 = new Wire
    demux(in, List(c), List(out1, out2))

    in.setSignal(false)
    c.setSignal(false)
    run
    assert(out1.getSignal === false, "demux 1")
    assert(out2.getSignal === false, "demux 1")

    in.setSignal(false)
    c.setSignal(true)
    run
    assert(out1.getSignal === false, "demux 2")
    assert(out2.getSignal === false, "demux 2")

    in.setSignal(true)
    c.setSignal(false)
    run
    assert(out1.getSignal === true, "demux 3")
    assert(out2.getSignal === false, "demux 3")

    in.setSignal(true)
    c.setSignal(true)
    run
    assert(out1.getSignal === false, "demux 4")
    assert(out2.getSignal === true, "demux 4")
  }

  //
  // to complete with tests for orGate, demux, ...
  //

}
