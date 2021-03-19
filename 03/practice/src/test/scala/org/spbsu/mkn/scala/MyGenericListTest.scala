package org.spbsu.mkn.scala

import org.scalatest.funsuite.AnyFunSuite
import org.spbsu.mkn.scala
import org.spbsu.mkn.scala.MyGenericList.{fromSeq, size, sort, sum}

class MyGenericListTest extends AnyFunSuite {
  test("sort") {
    assert(sort(fromSeq(Seq(1, 2, 3))).head == 1)
    assert(sort(fromSeq(Seq(4,5,5,5,5,5,0,0))).head == 0)
    assert(size(sort(fromSeq(Seq(1, 2, 3)))) == 3)
  }



}