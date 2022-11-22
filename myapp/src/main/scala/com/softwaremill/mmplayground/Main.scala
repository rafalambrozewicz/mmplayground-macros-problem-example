package com.softwaremill.mmplayground

object Main {

  // test #0
  Endpoint().n[Wrapper.MyCaseClass]

  object Wrapper {
    case class MyCaseClass(y: Option[Int])
    class MyClass
  }

  // test #1
  Endpoint().n[MyCaseClass2]

  case class MyCaseClass2(y: Option[Int])

  def main(args: Array[String]): Unit = {

    // test #2
    val a = Endpoint().n[Wrapper.MyCaseClass]

    // test #3
    val b = Endpoint().n[MyCaseClass2]

    println("hello macros!")
  }
}

class MyClass {

  object things {
    case class YetAnotherMyCaseClass(x: Option[Int])
    // test #4
    val c = Endpoint().n[YetAnotherMyCaseClass]
  }
}
