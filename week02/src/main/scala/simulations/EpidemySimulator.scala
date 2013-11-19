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

  def moves = List((1, 0), (-1, 0), (0, 1), (0, -1))

  def in(room: (Int, Int)): Person => Boolean = room match {
    case (r, c) => { p => p.row == r && p.col == c }
  }

  def peopleIn(room: (Int, Int)) = persons match {
    case null => Nil
    case people => people.filter(in(room))
  }

  def visiblyInfectious(room: (Int, Int)): Boolean = {
    peopleIn(room).count(p => p.sick || p.dead) > 0
  }

  def infectious(room: (Int, Int)): Boolean = {
    peopleIn(room).count(p => p.infected || p.sick || p.dead) > 0
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //

    def next = moves.map { case (x, y) => (
      ((row + x) % roomRows + roomRows) % roomRows,
      ((col + y) % roomColumns + roomColumns) % roomColumns
    )}

    def moveTo(room: (Int, Int)) = room match {
      case (r, c) => { row = r; col = c }
    }

    def move(): Unit = {
      if(!dead) {
        val possibleMoves = next.filterNot(visiblyInfectious)
        if (!possibleMoves.isEmpty) moveTo(possibleMoves(randomBelow(possibleMoves.length)))
        if(becomesInfected) infect()
        afterDelay(upTo(5)) { move() }
      }
    }

    def becomesInfected = !infected && !immune && infectious(row, col) && random <= transRate

    def infect() = {
      infected = true
      afterDelay(incubationTime)  { sick = true }
      for(delay <- dieTime until immuneTime) {
        afterDelay(delay) { if (sick && !dead && random <= dieRate) dead = true }
      }
      afterDelay(immuneTime) { if(!dead) { immune = true; sick = false } }
      afterDelay(healTime) { if(!dead) { infected = false; immune = false } }
    }

    if(id <= initialInfected) infect()
    move()
  }
}
