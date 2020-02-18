package org.minhdo.bdd

sealed abstract class Node(private[bdd] val id: Long, private[bdd] val bdd: Bdd) {
  def &(that: Node): Node = ite(this, that, bdd.constant(false))
  def |(that: Node): Node = ite(this, bdd.constant(true), that)
  def ^(that: Node): Node = ite(this, ~that, that)
  def unary_~(): Node = ite(this, bdd.constant(false), bdd.constant(true))

  private[this] def substitute(f: Node, v: Variable, vIsTrue: Boolean): Node = f match {
    case _: True => f
    case _: False => f
    case u: Variable => if (u eq v) {
      bdd.constant(vIsTrue)
    } else {
      u
    }
    case Ite(u, t, e) => if (u.eq(v) && vIsTrue) {
      substitute(t, v, vIsTrue)
    } else if (u.eq(v) /* && !vIsTrue */) {
      substitute(e, v, vIsTrue)
    } else {
      Ite(v, substitute(t, v, vIsTrue), substitute(e, v, vIsTrue), bdd)
    }
  }

  private[this] def ite(cond: Node, thenExpr: Node, elseExpr: Node): Node = cond match {
    case _: True => thenExpr
    case _: False => elseExpr
    case _ => bdd.getOrComputeEntry(cond, thenExpr, elseExpr, {
      val v = bdd.topVar(cond, thenExpr, elseExpr)
      val t = ite(
        substitute(cond, v, vIsTrue = true),
        substitute(thenExpr, v, vIsTrue = true),
        substitute(elseExpr, v, vIsTrue = true))
      val e = ite(
        substitute(cond, v, vIsTrue = false),
        substitute(thenExpr, v, vIsTrue = false),
        substitute(elseExpr, v, vIsTrue = false))
      if (t eq e) {
        return t
      }
      Ite(v, t, e, bdd)
    })
  }
}

// The following classes are not defined as case classes
// because reference equality is required for performance

private[bdd] class True(override private[bdd] val bdd: Bdd) extends Node(bdd.nextId(), bdd) {
  override def hashCode(): Int = id.toInt
  override def toString: String = "True"
}

private[bdd] class False(override private[bdd] val bdd: Bdd) extends Node(bdd.nextId(), bdd) {
  override def hashCode(): Int = id.toInt
  override def toString: String = "False"
}

private[bdd] class Variable(override private[bdd] val bdd: Bdd) extends Node(bdd.nextId(), bdd) {
  override def equals(other: Any): Boolean = other match {
    case that: Variable => id == that.id
    case _ => false
  }

  override def hashCode(): Int = id.toInt

  override def toString: String = "b" + id
}

private[bdd] class Ite(private[bdd] val cond: Variable, private[bdd] val thenExpr: Node,
                       private[bdd] val elseExpr: Node, override private[bdd] val bdd: Bdd)
  extends Node(bdd.nextId(), bdd) {
  bdd.cacheTopVar(this)

  override def equals(other: Any): Boolean = other match {
    case Ite(oCond, oThen, oElse) =>
      cond.id == oCond.id && thenExpr.id == oThen.id && elseExpr.id == oElse.id
    case _ => false
  }

  override def hashCode(): Int =
    41 * cond.id.toInt + 41*41 * thenExpr.id.toInt + 41*41*41 * elseExpr.id.toInt
}

private[bdd] object Ite {
  private[bdd] def apply(cond: Variable, thenExpr: Node, elseExpr: Node, bdd: Bdd): Ite = {
    val result = new Ite(cond, thenExpr, elseExpr, bdd)
    bdd.cacheTopVar(result)
    bdd.remember(result)
  }

  private[bdd] def unapply(ite: Ite): Option[(Variable, Node, Node)] = if (ite ne null) {
    Some((ite.cond, ite.thenExpr, ite.elseExpr))
  } else {
    None
  }
}
