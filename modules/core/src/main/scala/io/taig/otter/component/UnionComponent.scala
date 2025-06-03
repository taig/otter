// package io.taig.otter.component

// import io.taig.otter.operation.UnionSchemaInvariant

// import scala.annotation.targetName

// trait UnionComponent[Self[_], Value[_]](using self: UnionSchemaInvariant[Self, Value]):
//   extension [A](self: Self[A])
//     @targetName("union :+ schema")
//     final def :+[B](schema: Value[B]): Self[Either[A, B]] = self.orElse(schema.toUnion)

//   extension [A](self: Value[A])
//     @targetName("schema :+ schema")
//     final def :+[B](schema: Value[B]): Self[Either[A, B]] = self.toUnion.orElse(schema.toUnion)

//     @targetName("schema +: schema")
//     final def +:[B](schema: Value[B]): Self[Either[A, B]] = self.toUnion.orElse(schema.toUnion)

//     final def toUnion: Self[A] = this.self.lift(self)

//   extension [A <: Matchable](self: Self[A])
//     @targetName("union | schema")
//     final inline def |[B <: Matchable](schema: Value[B]): Self[A | B] = self.or(schema.toUnion)

//   extension [A <: Matchable](self: Value[A])
//     @targetName("schema | schema")
//     final inline def |[B <: Matchable](schema: Value[B]): Self[A | B] = self.toUnion.or(schema.toUnion)
