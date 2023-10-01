package io.taig.otter

import io.taig.otter.Yolo.Record.Of

object Yolo {
  abstract class Schema[A]
  abstract class Value[A] extends Schema[A]
  abstract class Primitive[A] extends Value[A]
  abstract class Enumeration[A] extends Value[A]

  abstract class Record[A] extends Schema[A] {
    self =>
    type Of <: Schema[?]

    def orElse[B](schema: Schema[B]): Record.Of[self.Of | schema.type, A | B]
  }

  object Record:
    type Of[A <: Schema[?], B] = Record[B] { type Of <: A }

  val a: Primitive[String] = ???
  val b: Enumeration[Int] = ???

  val c: Record.Of[Primitive[String], String] = ???
  val d: Record.Of[Primitive[String] | Enumeration[Int], String | Int] = c.orElse(b)
  val e: Record.Of[Value[?], String | Int] = d
}
