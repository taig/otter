package io.taig.otter.schema

import cats.syntax.all.*
import munit.FunSuite

final class SumTest extends FunSuite:
  enum Foo:
    case Bar(name: String)

  val bar: Product[String, Foo.Bar] = field("name", string).to
  val foo: Coproduct[String, Foo] = branch("bar", bar).to

  enum Animal:
    case Bird(name: String)
    case Cat(lives: Int)
    case Dog(goodBoy: Boolean)

  val bird: Product[String, Animal.Bird] = field("name", string).to
  val cat: Product[String, Animal.Cat] = field("lives", int).to
  val dog: Product[String, Animal.Dog] = field("goodBoy", boolean).to
  val animal: Coproduct[String, Animal] = (
    branch("bird", bird) :+
      branch("cat", cat) :+
      branch("dog", dog)
  ).to

  test("decode: nested discriminator") {
    assertEquals(
      obtained = animal
        .withNestedDiscriminator(identifier = "type", value = "value")
        .decode(OpenApi.obj("type" := "cat", "value" := OpenApi.obj("lives" := 7))),
      expected = Animal.Cat(lives = 7).valid
    )
  }

  test("decode: nested discriminator (identifier missing)") {
    assertEquals(
      obtained = animal
        .withNestedDiscriminator(identifier = "type", value = "value")
        .decode(OpenApi.obj("value" := OpenApi.obj("lives" := 7))),
      expected = Violations.oneNec(History.Root / "type", Constraint.required.toViolation(OpenApi.Null)).invalid
    )
  }

  test("decode: nested discriminator (identifier unknown)") {
    val names = OpenApi.Array(Vector("bird", "cat", "dog").map(_.asOpenApi))
    assertEquals(
      obtained = animal
        .withNestedDiscriminator(identifier = "type", value = "value")
        .decode(OpenApi.obj("type" := "tiger", "value" := OpenApi.obj("lives" := 7))),
      expected = Violations.rootNec(Constraint.collection.oneOf(names).toViolation(OpenApi.fromString("tiger"))).invalid
    )
  }

  test("decode: nested discriminator (value missing)") {
    assertEquals(
      obtained = animal
        .withNestedDiscriminator(identifier = "type", value = "value")
        .decode(OpenApi.obj("type" := "cat")),
      expected = Violations.oneNec(History.Root / "value", Constraint.required.toViolation(OpenApi.Null)).invalid
    )
  }

  test("decode: nested discriminator (value error)") {
    assertEquals(
      obtained = animal
        .withNestedDiscriminator(identifier = "type", value = "value")
        .decode(OpenApi.obj("type" := "cat", "value" := OpenApi.obj())),
      expected =
        Violations.oneNec(History.Root / "value" / "lives", Constraint.required.toViolation(OpenApi.Null)).invalid
    )
  }

  test("decode: merged discriminator") {
    assertEquals(
      obtained = animal
        .withMergedDiscriminator(identifier = "type")
        .decode(
          OpenApi.obj(
            "type" := "cat",
            "lives" := 7
          )
        ),
      expected = Animal.Cat(lives = 7).valid
    )
  }

  test("decode: keyed discriminator") {
    assertEquals(
      obtained = animal.withKeyedDiscriminator
        .decode(OpenApi.obj("cat" := OpenApi.obj("lives" := 7))),
      expected = Animal.Cat(lives = 7).valid
    )
  }

  test("decode: none discriminator") {
    assertEquals(
      obtained = animal.withoutDiscriminator.decode(OpenApi.obj("lives" := 7)),
      expected = Animal.Cat(lives = 7).valid
    )
  }

  test("decode: none discriminator (error)") {
    assertEquals(
      obtained = animal.withoutDiscriminator.decode(OpenApi.obj()),
      expected = Violations
        .ofNec(
          History.Root / "bird" / "name" -> Constraint.required.toViolation(OpenApi.Null),
          History.Root / "cat" / "lives" -> Constraint.required.toViolation(OpenApi.Null),
          History.Root / "dog" / "goodBoy" -> Constraint.required.toViolation(OpenApi.Null)
        )
        .invalid
    )
  }

  test("encode: nested discriminator") {
    val cat = Animal.Cat(lives = 7)

    assertEquals(
      obtained = animal.withNestedDiscriminator(identifier = "type", value = "value").encode(cat),
      expected = OpenApi.obj("type" := "cat", "value" := OpenApi.obj("lives" := 7))
    )
  }

  test("encode: merged discriminator") {
    val cat = Animal.Cat(lives = 7)

    assertEquals(
      obtained = animal.withMergedDiscriminator(identifier = "type").encode(cat),
      expected = OpenApi.obj("type" := "cat", "lives" := 7)
    )
  }

  test("encode: merged discriminator (key conflict)") {
    val cat = Animal.Cat(lives = 7)

    assertEquals(
      obtained = animal.withMergedDiscriminator(identifier = "lives").encode(cat),
      expected = OpenApi.Object.Empty
    )
  }

  test("encode: merged discriminator (no object)") {
    val bar: Primitive[Foo.Bar] = string.imap[Foo.Bar](Foo.Bar.apply)(_.name)
    val foo: Coproduct[String, Foo] = branch("bar", bar).toSum.to[Foo]

    assertEquals(
      obtained = foo.withMergedDiscriminator(identifier = "type").encode(Foo.Bar("foobar")),
      expected = OpenApi.Object.Empty
    )
  }

  test("encode: keyed discriminator") {
    val cat = Animal.Cat(lives = 7)

    assertEquals(
      obtained = animal.withKeyedDiscriminator.encode(cat),
      expected = OpenApi.obj("cat" := OpenApi.obj("lives" := 7))
    )
  }

  test("encode: none discriminator") {
    val cat = Animal.Cat(lives = 7)

    assertEquals(
      obtained = animal.withoutDiscriminator.encode(cat),
      expected = OpenApi.obj("lives" := 7)
    )
  }
