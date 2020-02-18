package org.minhdo.bdd

import java.util.concurrent.atomic.AtomicLong

import scala.collection.mutable
import scala.ref.WeakReference

class Bdd {
  private[this] val atomicLong = new AtomicLong

  private[this] val uniqueTable = new mutable.WeakHashMap[Ite, WeakReference[Ite]]
  private[this] val computedTable = new mutable.WeakHashMap[(Node, Node, Node), WeakReference[Node]]
  private[this] val topVar = new mutable.WeakHashMap[Node, WeakReference[Variable]]

  private[this] val TRUE = new True(this)
  private[this] val FALSE = new False(this)

  private[bdd] def nextId(): Long = atomicLong.incrementAndGet()

  private[bdd] def remember(ite: Ite): Ite =
    uniqueTable.getOrElseUpdate(ite, WeakReference(ite)).get.get

  private[bdd] def getOrComputeEntry(cond: Node, thenExpr: Node, elseExpr: Node,
                                     compute: => Node): Node = {
    computedTable.getOrElseUpdate((cond, thenExpr, elseExpr), WeakReference(compute)).get.get
  }

  private[this] def computeTopVar(v: Variable, f: Node): Variable =
    topVar.get(f).flatMap(_.get).map(vf => if (vf.id < v.id) { vf } else { v }).getOrElse(v)

  private[bdd] def topVar(f: Node, g: Node, h: Node): Variable = f match {
    case v: Variable =>
      val vg = computeTopVar(v, g)
      val vh = computeTopVar(v, h)
      if (vg.id < vh.id) {
        vg
      } else {
        vh
      }
    case _ => Array(
      topVar.get(f).flatMap(_.get),
      topVar.get(g).flatMap(_.get),
      topVar.get(h).flatMap(_.get))
        .flatten
        .minBy(_.id)
  }

  private[bdd] def cacheTopVar(ite: Ite): Unit = topVar.getOrElseUpdate(ite, ite match {
    case Ite(v, t, e) => WeakReference(topVar(v, t, e))
  })

  def constant(b: Boolean): Node = if (b) TRUE else FALSE

  def variable(): Node = new Variable(this)
}
