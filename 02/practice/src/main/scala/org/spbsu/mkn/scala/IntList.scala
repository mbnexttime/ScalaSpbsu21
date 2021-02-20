package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

sealed trait IntList {
  def head: Int

  def tail: IntList

  def drop(n: Int): IntList

  def take(n: Int): IntList

  def map(f: Int => Int): IntList

  def ::(elem: Int): IntList = new ::(elem,this)
}

object IntNil extends IntList {
  override def drop(n: Int): IntList = {
    if ( n > 0) undef
    else IntNil
  }

  override def head: Int = IntList.undef

  override def map(f: Int => Int): IntList = IntNil

  override def take(n: Int): IntList = {
    if (n > 0) undef
    else IntNil
  }

  override def tail: IntList = IntList.undef
}

case class ::(override val head: Int, override val tail: IntList) extends IntList {
  override def map(f: Int => Int): IntList = f(head) :: tail.map(f)

  override def drop(n: Int): IntList = {
    n match {
      case i if i <= 0 => this
      case _ => tail.drop(n - 1)
    }
  }

  override def take(n: Int): IntList = {
    n match {
      case i if i <= 0 => IntNil
      case _ => head :: tail.take(n - 1)
    }
  }
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq(seq: Seq[Int]): IntList = {
    seq.foldRight(IntNil: IntList)((e, list) => e :: list)
  }

  def sum(intList: IntList): Int = {
    if (intList == IntNil) undef
    foldLeft(intList, 0)((s, e) => s + e)
  }

  def size(intList: IntList): Int = {
    foldLeft(intList, 0)((s, _) => s + 1)
  }

  @scala.annotation.tailrec
  def foldLeft[B](list: IntList, ini: B)(f: Function2[B, Int, B]): B = {
    list match {
      case (x :: xs) => foldLeft(xs, f(ini, x))(f)
      case _ => ini
    }
  }

  // extra task: implement sum using foldLeft
  // def foldLeft(???)(???): ??? = ???
}
