package io.taig.otter.json.circe

import io.taig.otter as Base
import io.taig.otter.Plain.*
import io.circe.Json
import cats.syntax.all.*

object JsonCollectionEncoder:
  def apply[A](data: Base.Collection[Parent.Writer[?], A], a: A): Vector[Json] = data match
    // case Base.Collection.Root(schema) =>
    //   val x: Parent.Writer[?] = schema
    //   ???
    case data: Base.Collection.Root[Base.Writer[AsSchema, Base.Optional, Base.Schema, ?, *], ?] => ???
    // case data: Base.Collection[Base.Writer[AsSchema, Base.Optional, Base.Schema, ?, ?], ?]      => ???

  def apply1[A](data: Base.Collection.Root[Parent.Writer, A], a: Vector[A]): Vector[Json] = ???
