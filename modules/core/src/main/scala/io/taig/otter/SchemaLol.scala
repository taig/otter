package io.taig.otter

sealed abstract class SchemaLol[+A]:
  self =>
  type Self[+a] <: SchemaLol[a]
  type Of <: SchemaLol[?]

  def description(f: Option[String] => Option[String]): Self[A] = ???

  def example: Option[A]
  def example[B >: A](f: Option[A] => Option[B]): Self[B]
  final def example[B >: A](value: Option[B]): Self[B] = example(_ => value)
  final def example[B >: A](value: B): Self[B] = example(Some(value))

  final def orElse[B](schema: SchemaLol[B]): SchemaLol.Of[self.Of | schema.Of, Either[A, B]] = ???

object SchemaLol:
  type Of[+A <: SchemaLol[?], +B] <: SchemaLol[B] { type Of <: A }

  sealed abstract class Value[+A] extends SchemaLol[A] {
    self =>
    override type Self[+a] <: SchemaLol.Value[a]
    override type Of <: SchemaLol.Value[?]
  }

  final case class Primitive[+A](example: Option[A]) extends Value[A] {
    override type Self[+a] = Primitive[a]
    override type Of <: Primitive[A]
    override def example[B >: A](f: Option[A] => Option[B]): Primitive[B] = copy(example = f(example))
  }
  sealed abstract class Enumeration[+A] extends Value[A] {
    final override type Self[+a] = Enumeration[a]
    override type Of <: Enumeration[A]

    override def example: Option[A] = ???
  }
  sealed abstract class Record[+A] extends SchemaLol[A] {
    final override type Self[+a] = Record[a]
    override type Of <: Record[A]

    override def example: Option[A] = ???
  }

object Yolo {
  val a: SchemaLol.Primitive[String] = ???
  val b: SchemaLol.Enumeration[Int] = ???
  val c: SchemaLol.Record[Long] = ???

//  val aaa: SchemaLol.Primitive[String] | SchemaLol.Enumeration[Int] = ???
//  val bbb: SchemaLol.Value[?] = aaa

  val x: SchemaLol.Of[SchemaLol.Primitive[String] | SchemaLol.Enumeration[Int], Either[String, Int]] = a.orElse(b)
  val y: SchemaLol.Of[SchemaLol.Value[String | Int], Either[String, Int]] = x
  val z: SchemaLol.Of[SchemaLol.Value[?], Either[String, Int]] = x

  def encode[A](schema: SchemaLol.Of[SchemaLol.Value[?], A]) = schema match {
    case _: SchemaLol.Primitive[?]   => ???
    case _: SchemaLol.Enumeration[?] => ???
  }

  encode(z)
}
