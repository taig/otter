package io.taig.otter

import io.taig.otter.Plain.*
import munit.FunSuite

final class SyntaxTest extends FunSuite:
  test("collection"):
    val _: Collection.Of[Primitive.Required[String], Vector[String]] = string.collection
    val _: Collection.Of[Primitive.Required[?], Vector[String]] = string.collection
    val _: Collection.Of[Primitive[String], Vector[String]] = string.collection
    val _: Collection.Of[Primitive[?], Vector[String]] = string.collection
    val _: Collection.Of[Schema[String], Vector[String]] = string.collection
    val _: Collection.Of[Schema[?], Vector[String]] = string.collection

  test("union"):
    val _: Union[Either[String, Int]] = string.union :+ int
    val _: Union.Of[Primitive.Any, Either[String, Int]] = string :+ int
    val _: Union[Either[String, Either[Int, Long]]] = string +: int +: long
    val _: Union.Of[Union.Any | Primitive.Any, Either[Either[String, Int], Long]] = string :+ int :+ long
    val _: Union[String | Int] = string.union | int
    val _: Union[Either[String, Int]] = string or int
    val _: Union[String | Int | Long] = string | int | long
