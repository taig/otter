package io.taig.otter.http

import io.taig.otter as Base
import cats.syntax.all.*

object Playground:
  import Dsl.*
  import Dsl.given

  val x: Schema[String] = string
  string.as(3)
  string.imap(???)(???)
  string.optional
  string.ivalidate(???)(???)
  val y: Primitive.Required.Reader.Any = string.validate(???)
  string.optional
  string.tpe
  val a: Primitive.Required.Writer[String] = string.asWriter
  string.asReader
  val _: Primitive.Required.Reader.Any = string.as(???)
  val col: Collection.Writer[Vector[String]] = a.collection
  col.transform(list)
  col.transform(nonEmptyList)

  val _: Union[Either[String, Int]] = string.union :+ int
  val _: Union.Of[Primitive.Any, Either[String, Int]] = string :+ int
  val _: Union[Either[String, Either[Int, Long]]] = string +: int +: long
  val _: Union.Of[Union.Any | Primitive.Any, Either[Either[String, Int], Long]] = string :+ int :+ long
  val _: Union[String | Int] = string.union | int

  val _: Union[Either[String, Int]] = string or int
  val _: Union[String | Int | Long] = string | int | long
