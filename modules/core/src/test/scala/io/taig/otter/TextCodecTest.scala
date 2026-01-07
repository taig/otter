// package io.taig.otter

// import io.taig.otter.codec.TextParser
// import io.taig.otter.codec.TextPrinter
// import io.taig.otter.component.TextComponent.*
// import zio.*
// import zio.test.*

// object TextCodecTest extends ZIOSpecDefault:
//   enum Animal:
//     case Bird, Cat, Dog

//   object Animal:
//     val text: Text.Enumeration[Animal] = enumeration(string):
//       case Bird => "bird"
//       case Cat  => "cat"
//       case Dog  => "dog"

//   override def spec: Spec[TestEnvironment & Scope, Any] = suite("TextCodecTest")(
//     test("primitive"):
//       val input = TextPrinter.encode(string, "foobar")
//       val result = TextParser.decode(string, input)

//       assertTrue(result.toEither.is(_.right) == input)
//     ,
//     test("constant"):
//       val schema = constant(string, "foo")
//       val input = TextPrinter.encode(schema, "bar")
//       val result = TextParser.decode(schema, input)

//       assertTrue(
//         input == "foo",
//         result.toEither.is(_.right) == input
//       )
//     ,
//     test("enumeration"):
//       val input = TextPrinter.encode(Animal.text, Animal.Cat)
//       val result = TextParser.decode(Animal.text, input)

//       assertTrue(result.toEither.is(_.right) == Animal.Cat)
//   )
