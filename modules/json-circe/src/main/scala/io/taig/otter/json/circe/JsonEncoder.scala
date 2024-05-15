package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter as Base
import io.taig.otter.Plain.*
import io.taig.otter.Codec
import io.taig.otter.Fix

object JsonEncoder: // extends Encoder[Plain.Schema.Writer, Json]:
  // def apply[A](schema: Base.Schema[Base.Codec[Id, ?, *], A], a: A): Json = schema match
  //   case Base.Schema.Root(schema)   => apply(schema, a)
  //   case Base.Schema.Optional(self) => a.map(apply(self, _)).getOrElse(Json.Null)

  // def apply[A](schema: Base.Codec[Id, ?, A], a: A): Json = schema match
  //   case schema: Base.Collection[?, ?, A] => ???
  //   case schema: Base.Primitive[A]        => ???
  //   case schema: Base.Tuple[Id, ?, A]     => apply(schema, a)

  def apply0[A](schema: ParentA[A], a: A): Json = ??? // apply00(schema.unfix, a)

  def apply00[A, B](schema: Base.Schema[Base.Codec[ParentA[B], *], A], a: A): Json = ???

  def apply1[A, B](schema: Base.Codec[ParentA[A], B], b: B): Json = schema match
    case schema: Base.Tuple[ParentA[A], B] => ??? // apply2(schema, b)
    // case Base.Tuple.One(schema) => ??? // apply(schema, a)

  def apply2[A](tuple: Base.Tuple[Fix[[a] =>> Base.Schema[Codec[a, *], ?]], A], a: A): Json = tuple match
    case Base.Tuple.Empty => Json.arr()
    // case schema: Base.Tuple.One[Fix, Codec, A] => apply0(schema.schema, a)
    case Base.Tuple.One(schema) => apply0(schema, a)
