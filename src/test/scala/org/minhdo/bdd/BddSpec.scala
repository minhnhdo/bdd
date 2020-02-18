package org.minhdo.bdd

import org.scalatest._

class BddSpec extends FlatSpec with Matchers {
  "A BDD object" should "cache True" in {
    val b = new Bdd
    b.constant(true) shouldEqual b.constant(true)
  }

  "A BDD object" should "cache False" in {
    val b = new Bdd
    b.constant(false) shouldEqual b.constant(false)
  }

  "A BDD object" should "create different variables" in {
    val b = new Bdd
    b.variable() shouldNot equal(b.variable())
  }

  "A BDD object" should "cache repeated constructions" in {
    val b = new Bdd
    val v1 = b.variable()
    val v2 = b.variable()
    (v1 & v2) shouldEqual (v1 & v2)
    (v1 | v2) shouldEqual (v1 | v2)
    (v1 ^ v2) shouldEqual (v1 ^ v2)
    (~v1) shouldEqual (~v1)
  }

  "A BDD object" should "compute tautologies efficiently" in {
    val b = new Bdd
    val v1 = b.variable()
    val v2 = b.variable()
    (v1 | ~v1) shouldEqual b.constant(true)
    ((v1 & v2) | ~v1 | ~v2) shouldEqual b.constant(true)
  }

  "A BDD object" should "compute contradictions efficiently" in {
    val b = new Bdd
    val v1 = b.variable()
    val v2 = b.variable()
    (v1 & ~v1) shouldEqual b.constant(false)
    ((v1 & v2) & (~v1 | ~v2)) shouldEqual b.constant(false)
  }
}
