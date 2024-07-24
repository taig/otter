package io.taig.otter

import cats.Id

object Playground:
  import io.taig.otter.Types.*

  // val x: Sum[Id, Data.Primitive, String] = ???
  // val y: Sum[Data.Optional, Data.Array[Data.Primitive], Vector[String]] = ???

  // val x: Data.Optional[Data.Object[Data.Array[?]]] & Data.Optional[Data.Object[Data.Primitive]] =
  //   Data.Object.of("number" -> (Data.Number(3): Data.Primitive))
  // val y: Data.Optional[Data.Object[Data.Array[Data.Primitive] & Data.Primitive]] = x
