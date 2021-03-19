package org.spbsu.mkn.scala

sealed trait MyGenericList[+A] {
  def head: A

  def tail: MyGenericList[A]

  def drop(n: Int): MyGenericList[A]

  def take(n: Int): MyGenericList[A]

  def map[B](f: A => B): MyGenericList[B]

  def ::[B >: A](elem: B): MyGenericList[B] = new ::(elem, this)
}

case class ::[+A](override val head: A, override val tail: MyGenericList[A]) extends MyGenericList[A] {

  override def drop(n: Int): MyGenericList[A] = {
    n match {
      case i if i <= 0 => this
      case _ => tail.drop(n - 1)
    }
  }

  override def take(n: Int): MyGenericList[A] = {
    n match {
      case i if i <= 0 => MyNil.asInstanceOf[MyGenericList[A]]
      case _ => head :: tail.take(n - 1)
    }
  }

  override def map[B](f: A => B): MyGenericList[B] = {
    f(head) :: tail.map(f)
  }
}

case object MyNil extends MyGenericList[Nothing] {
  override def head: Nothing = MyGenericList.undef

  override def tail: MyGenericList[Nothing] = MyGenericList.undef

  override def drop(n: Int): MyGenericList[Nothing] = {
    if (n > 0) {
      MyGenericList.undef
    } else {
      MyNil
    }
  }

  override def take(n: Int): MyGenericList[Nothing] = {
    if (n > 0) {
      MyGenericList.undef
    } else {
      MyNil
    }
  }

  override def map[B](f: Nothing => B): MyGenericList[Nothing] = MyNil
}

object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq[A](seq: Seq[A]): MyGenericList[A] = {
    seq.foldRight(MyNil: MyGenericList[A])((e, list) => e :: list)
  }

  def size[A](list: MyGenericList[A]): Int = {
    foldLeft(list, 0)((s, _) => s + 1)
  }

  def sum[A: Numeric](list: MyGenericList[A]): A = {
    if (list == MyNil) {
      undef
    } else {
      foldLeft(list, Numeric[A].zero)((s, e) => Numeric[A].plus(s, e))
    }
  }

  @scala.annotation.tailrec
  def foldLeft[A, B](list: MyGenericList[A], ini: B)(f: (B, A) => B): B = {
    list match {
      case x :: xs => foldLeft(xs, f(ini, x))(f)
      case _ => ini
    }
  }
}
