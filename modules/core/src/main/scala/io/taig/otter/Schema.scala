package io.taig.otter

import io.taig.otter.Data.Optional

abstract class Schema[A]:
  type Self[+o <: Data, a] <: Schema.Of[o, a]

  type Of <: Data

  def imap[B](f: A => B)(g: B => A): Self[Of, A]

  def optional: Self[Data.Optional[Of], Option[A]]

  def encode(a: A): Of

object Schema:
  type Of[+O <: Data, A] = Schema[A] { type Of <: O }

sealed abstract class Collection2[A] extends Schema[A]:
  override type Self[+o <: Data, a] = Collection2.Of[o, a]

object Collection2:
  type Of[+O <: Data, A] = Collection2[A] { type Of <: O }

  def apply[A](schema: Schema[A]): Collection2.Of[Data.Array[schema.Of], Vector[A]] = new Collection2[Vector[A]]:
    override type Of = Data.Array[schema.Of]
    override def imap[B](f: Vector[A] => B)(g: B => Vector[A]): Collection2.Of[Of, Vector[A]] = ???
    override def optional: Collection2.Of[Optional[Data.Array[schema.Of]], Option[Vector[A]]] = ???
    override def encode(a: Vector[A]): Data.Array[schema.Of] = ???

sealed abstract class Product2[A] extends Schema[A]:
  override type Self[+o <: Data, a] <: Product2.Of[o, a]

object Product2:
  type Of[+O <: Data, A] = Product2[A] { type Of <: O }

sealed abstract class Record2[A] extends Product2[A]:
  final override type Self[+o <: Data, a] = Record2.Of[o, a]

object Record2:
  type Of[+O <: Data, A] = Record2[A] { type Of <: O }

  def apply[A](schema: Schema[A]): Record2.Of[Data.Object[schema.Of], A] = new Record2[A]:
    self =>
    type Of = Data.Object[schema.Of]
    override def imap[B](f: A => B)(g: B => A): Record2.Of[Of, A] = ???
    override def optional: Record2.Of[Optional[Of], Option[A]] = ???
    override def encode(a: A): self.Of = ???

object Test:
  val x: Record2.Of[Data.Primitive, String] = ???
  val y: Product2.Of[Data, String] = x
