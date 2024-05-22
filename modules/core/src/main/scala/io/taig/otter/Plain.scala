package io.taig.otter

import io.taig.otter as Base
import io.taig.otter.Isomorphic.Root
import io.taig.otter.Schema.Required.Root
import io.taig.otter.Schema.Optional
import io.taig.otter.Primitive.Root
import io.taig.otter.Enumeration.Root
import cats.arrow.FunctionK
import cats.data.Kleisli
import cats.Id

object Plain extends Dsl:
  final override type AsSchema[A] = A
  final override type AsCollection[A] = A
  final override type AsPrimitive[A] = A
  final override type AsTuple[A] = A

  // override def primitive[A](tpe: Type[A]): Primitive.Required[A] =
  //   Base.Schema.Required.Root(Base.Primitive.Root(tpe))

  val x: Schema[String] = ???

  val z: Schema[Option[String]] = x.mapF(_.optional)

  z match
    case Isomorphic.Root(fa) =>
      fa match
        case Schema.Optional(fa) =>
          fa match
            case Schema.Required.Root(fa) =>
              fa match
                case Enumeration.Root(schema) =>
                  schema match
                    case Isomorphic.Root(fa) =>
                      fa match
                        case Schema.Required.Root(fa) =>
                          fa match
                            case Primitive.Root(tpe) =>
                              tpe
