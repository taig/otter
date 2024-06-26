package io.taig.otter

import cats.data.NonEmptyList

object Playground:
  import Plain.*
  import Plain.given

  val x: Primitive.Required[String] = string

  val y: Schema[Option[String]] = x.optional

  val _: Collection.Of[Primitive.Required[String], NonEmptyList[String]] = string.collection(nonEmptyList)

  val todo: Schema.Writer[NonEmptyList[String] | Long] =
    string.asWriter.collection.contramap[NonEmptyList[String]](_.toList.toVector)
    ???

  string.collection.transform(nonEmptySet, todo)

  val unionA: Union[Either[String, Int]] = string.orElse(int)
  val unionB: Union[String | Int] = string | int
