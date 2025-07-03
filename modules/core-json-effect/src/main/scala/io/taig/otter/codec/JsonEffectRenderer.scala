package io.taig.otter.codec

import cats.Applicative
import cats.Order
import cats.arrow.FunctionK
import io.taig.otter.Effect
import io.taig.otter.Json
import io.taig.otter.Key

final class JsonEffectRenderer[S[_]: Applicative, A: Order](renderer: Renderer[Json, S[A]])(lift: Effect[A] => A)
    extends Renderer[Json, S[Effect[A]]]:
  val field: Renderer[Json.Field, S[(String, A)]] =
    FieldRenderer[Key, Json, S, A](printer = KeyPrinter.Unquoted, renderer)
      .mapK[Json.Field](FunctionK.liftFunction(_.self))

  val fromJson = [A] =>
    (json: Json[A]) =>
      json match
        case Json.Collection(self)  => self
        case Json.Constant(self)    => self
        case Json.Dictionary(self)  => self.leftMapK(FunctionK.liftFunction[Key, Json](_.translate[Json]))
        case Json.Enumeration(self) => self
        case Json.Nullable(self)    => self
        case Json.Primitive(self)   => self
        case Json.Record(self)      => self
        case Json.Tuple(self)       => self
        case Json.Union(self)       => self

  val base = EffectRenderer[Json, Json.Primitive, Json.Field, S, A](
    renderer = renderer,
    printer = JsonPrimitivePrinter.Quoted,
    field
  )(lift).mapK[Json](FunctionK.lift(fromJson))

  override def render[B](schema: Json[B]): S[Effect[A]] = base.render(schema)
