package io.taig.otter

import cats.data.NonEmptyList
import cats.syntax.all.*
import io.taig.otter as Base

object Playground:
  import Plain.*
  import Plain.given

  // val _: Collection.Of[Primitive.Required[String], NonEmptyList[String]] = string.collection(nonEmptyList)

  // val todo: Schema.Writer[NonEmptyList[String] | Long] =
  //   string.asWriter.collection.contramap[NonEmptyList[String]](_.toList.toVector)
  //   ???

  // string.collection.transform(nonEmptySet, todo)

  // val unionA: Union[Either[String, Int]] = string.orElse(int)
  // val unionB: Union[String | Int] = string | int

  type MySchema[A] = Base.Schema[Int, ?, A]

  def render[A](schema: MySchema[A]): String = schema match
    case Base.Primitive.Required.Root(m, tpe) => ???
    case Base.Union.OrElse(m, left, right)    => render(left)