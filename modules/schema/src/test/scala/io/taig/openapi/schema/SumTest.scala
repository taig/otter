package io.taig.openapi.schema

import io.taig.openapi.OpenApi
import io.taig.openapi.schema.schemas.*
import io.taig.openapi
import munit.FunSuite

final class SumTest extends FunSuite:
  enum Foo:
    case Bar(name: String)

  val bar: Product[String, Foo.Bar] = field("name", string).toProduct.as[Foo.Bar]
  val foo: Sum[String, Foo] = branch("bar", bar).toSum.as[Foo]

  enum Animal:
    case Bird(name: String)
    case Cat(lives: Int)
    case Dog(goodBoy: Boolean)

  val bird: Product[String, Animal.Bird] = field("name", string).toProduct.as[Animal.Bird]
  val cat: Product[String, Animal.Cat] = field("lives", int).toProduct.as[Animal.Cat]
  val dog: Product[String, Animal.Dog] = field("goodBoy", boolean).toProduct.as[Animal.Dog]
  val animal: Sum[String, Animal] = (
    branch("bird", bird) :+
      branch("cat", cat) :+
      branch("dog", dog)
  ).as[Animal]

  test("as: enum 1") {
    assertEquals(
      obtained = foo.encode(Foo.Bar("foobar")),
      expected = OpenApi.obj(
        "type" -> OpenApi.fromString("bar"),
        "value" -> OpenApi.obj("name" -> OpenApi.fromString("foobar"))
      )
    )
  }
