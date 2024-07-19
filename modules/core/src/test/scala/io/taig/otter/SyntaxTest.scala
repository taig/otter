package io.taig.otter

import io.taig.otter.Dsl.*
import munit.FunSuite

final class SyntaxTest extends FunSuite:
  test("collection"):
    val _: Collection.Of[Primitive.Required[String], Vector[String]] = string.toCollection
    val _: Collection.Of[Primitive.Required[?], Vector[String]] = string.toCollection
    val _: Collection.Of[Primitive[String], Vector[String]] = string.toCollection
    val _: Collection.Of[Primitive[?], Vector[String]] = string.toCollection
    val _: Collection.Of[Codec[String], Vector[String]] = string.toCollection
    val _: Collection.Of[Codec[?], Vector[String]] = string.toCollection

  test("union"):
    val _: Union[Either[String, Int]] = string :+ int
    val _: Union.Of[Primitive[?], Either[String, Int]] = string :+ int
    val _: Union[Either[String, Either[Int, Long]]] = string +: int +: long
    val _: Union.Of[Primitive[?], Either[Either[String, Int], Long]] = string :+ int :+ long
    val _: Union[String | Int] = string | int
    val _: Union[String | Int | Long] = string | int | long

  test("product"):
    val _: Product[(String, Int)] = string :* int
    val _: Product[(String, Int, Long)] = string :* int :* long
    val _: Product[(Int, String)] = int *: string
    val _: Product[(Int, String, Long)] = int *: string *: long
    val _: Product.Of[Primitive[?] | Collection[?], (Int, Vector[String])] = int :* string.toCollection
    val _: Product.Of[Primitive[?] | Collection[?], (Int, Vector[String])] = int :* string.toCollection
    val _: Product.Of[Primitive.Required[?], (Int, String, Long)] = int *: string *: long
    val _: Product.Of[Primitive[?], (Int, String, Long)] = int *: string *: long
    val _: Product.Of[Codec[?], (Int, String, Long)] = int *: string *: long
