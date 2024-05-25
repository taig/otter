package io.taig.otter

import io.taig.otter as Base

object Plain extends Dsl:
  final override type AsSchema[A] = A
  final override type AsCollection[A] = A
  final override type AsPrimitive[A] = A
  final override type AsTuple[A] = A

  // override def primitive[A](tpe: Type[A]): Primitive.Required[A] =
  //   Base.Schema.Required.Root(Base.Primitive.Root(tpe))

  val x: Schema[Vector[Vector[String]]] = ???

  x match
    case Base.Isomorphic.Root(schema) =>
      schema match
        case Base.Schema.Required.Root(schema) =>
          schema match
            case Base.Collection.Root(schema) =>
              schema match
                case Base.Isomorphic.Root(schema) =>
                  schema match
                    case Base.Schema.Required.Root(schema) =>
                      schema
