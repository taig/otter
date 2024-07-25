package io.taig.otter

import cats.Id

object Playground:
  import io.taig.otter.Types.*


  extension [A <: Matchable](self: Data.Optional[A]) inline def getOrElse[B >: A](b: => B): B =
    self match
      case Data.Null => b
      case a: A => a
    

  val a: Data.Object[Data.Array[?]] = Data.Object.of("foo" -> Data.Array.Empty)
  val b: Data.Optional[Data.Object[Data.Number]] = Data.Null
  val c: Data.Object[Data.String] = Data.Object.one("asdf", Data.String("asdf"))
  
  // val q: Data.Object[Data.Array[?] | Data.Number | Data.String] = a ++ b.getOrElse(Data.Object.Empty) ++ c
  // val w: Data.Object[Data.Object[?] | Data.String] = Data.Object.one("value", a) ++ c

  val x: Codec[Data.Optional[Data.Object[Data.Array[?]]], String] = ???
  val y: Codec[Data.Object[Data.Array[?]], String] = ???

  val v = Branch.merged("type", "mybranch", x)
  val w = Branch.merged("type", "mybranch", y)
