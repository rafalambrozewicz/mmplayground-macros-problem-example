package com.softwaremill.mmplayground

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SortOfMainTest extends AnyFlatSpec with Matchers {

//  "zero" should "be zero" in {
//    assert(0 == 0)
//  }

//  "nice" should "compile properly if Wrapper class is not nested" in {
//    case class Wrapper(i: Option[Int])
//
//    val a = Endpoint().n[Wrapper]
//  }

  "nice" should "compile properly if Wrapper class is nested" in {
    object OuterWrapper {
      case class InnerWrapper(i: Option[Int])
    }

    val a = Endpoint().n[OuterWrapper.InnerWrapper]
  }
}