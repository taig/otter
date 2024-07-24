package io.taig.otter

import cats.Eq

enum Discriminator:
  case Nested(identifier: String, value: String)
  case Merged(identifier: String)
  case Keyed

object Discriminator:
  object Nested:
    val Default: Discriminator.Nested = Nested(identifier = "type", value = "value")

  object Merged:
    val Default: Discriminator.Merged = Merged(identifier = "type")

  val Default: Discriminator = Nested.Default

  given Eq[Discriminator] = Eq.fromUniversalEquals

sealed abstract class Discriminator2[+A <: Data]

object Discriminator2:
  final case class Keyed[A <: Data]() extends Discriminator2[Data.Object[A]]
  final case class Nested[A <: Data]() extends Discriminator2[Data.Object[Data.String | A]]
  final case class Merged[F[a <: Data] <: Data.Object[a], A <: Data]()
      extends Discriminator2[Data.Object[Data.String | A]]
  final case class Yolo[A <: Data]() extends Discriminator2[A]

object Test:
  val x: Data.Object[Data.String] = ???
  val y: Data.Object[Data.Array[?]] = ???

  val d: Discriminator2[Data.Object[Data.String | Data.Array[?]]] = Discriminator2.Merged[Data.Object, Data.Array[?]]()
