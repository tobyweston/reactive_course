package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt
  def upTo(i: Int) = 1 + randomBelow(i)

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val incubationTime: Int = 6
    val dieTime: Int = 14
    val immuneTime: Int = 16
    val healTime: Int = 18

    val prevalenceRate: Double = 0.01
    val transRate: Double = 0.4
    val dieRate: Double = 0.25

    // to complete: additional parameters of simulation
  }

  import SimConfig._

  val initialInfected = (population * prevalenceRate).toInt

  val persons: List[Person] = (for(id <- 1 to population) yield new Person(id)).toList

  class Person (val id: Int) {
    var infected = id <= initialInfected
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //

    def move(): Unit = {

      // choose random direction
      // apply transformation on row, col
      // remember to wrap at 0 or 8

      if(!dead) {
        afterDelay(upTo(5)) { move() }
      }
    }

    move()
  }
}
