package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
<<<<<<< HEAD

=======
  
>>>>>>> origin/master
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
<<<<<<< HEAD
          "  " + currentTime + ": " + name + " -> " + wire.getSignal)
=======
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
>>>>>>> origin/master
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
<<<<<<< HEAD
      afterDelay(InverterDelay) {
        output.setSignal(!inputSig)
      }
=======
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
>>>>>>> origin/master
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
<<<<<<< HEAD
      afterDelay(AndGateDelay) {
        output.setSignal(a1Sig & a2Sig)
      }
=======
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
>>>>>>> origin/master
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
<<<<<<< HEAD
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) {
        output.setSignal(a1Sig || a2Sig)
      }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val b, c, d = new Wire
    inverter(a1, b)
    inverter(a2, c)
    andGate(b, c, d)
    inverter(d, output)
  }

  def passThrough(input: Wire, output: Wire) {
    def passThroughAction() {
      val inputSig = input.getSignal
      afterDelay(0) {
        output.setSignal(inputSig)
      }
    }
    input addAction passThroughAction
  }

  def demuxSingleWire(in: Wire, c: Wire, out0: Wire, out1: Wire) {
    val inv = new Wire
    inverter(c, inv)
    andGate(in, inv, out0)
    andGate(in, c, out1)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]): Unit = c match {
    case Nil => passThrough(in, out.head)
    case x :: Nil => demuxSingleWire(in, x, out.tail.head, out.head)
    case x :: xs => {
      val o1, o2 = new Wire
      demuxSingleWire(in, x, o1, o2)
      demux(o1, xs, out.drop(out.size / 2))
      demux(o2, xs, out.take(out.size / 2))
    }
=======
    ???
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    ???
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    ???
>>>>>>> origin/master
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
