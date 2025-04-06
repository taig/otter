// package io.taig.otter

// import munit.FunSuite
// import cats.data.State
// import cats.syntax.all.*
// import io.taig.otter.JsonDsl.key.*
// import scala.collection.immutable.ListMap

// final class JsonKeyZodRendererTest extends OtterSuite:
//   val renderer = JsonKeyZodRenderer

//   test("constant"):
//     assertEq(
//       obtained = renderer(string).runA(ListMap.empty).value,
//       expected = Expression.Inline("z.string()")
//     )

//   test("primitive"):
//     assertEq(
//       obtained = renderer(string).runA(ListMap.empty).value,
//       expected = Expression.Inline("z.string()")
//     )
