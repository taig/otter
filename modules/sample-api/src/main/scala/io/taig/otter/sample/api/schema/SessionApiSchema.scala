// package io.taig.otter.sample.api.schema

// import cats.syntax.all.*
// import io.taig.otter.sample.api.Dsl.*

// import java.util.UUID

// opaque type SessionApiSchema = UUID

// object SessionApiSchema:
//   extension (self: SessionApiSchema) def toUUID: UUID = self

//   def apply(value: UUID): SessionApiSchema = value

//   def codec(prefix: String): Primitive[SessionApiSchema] = parser("session") { value =>
//     Option
//       .when(value.startsWith(prefix) && value.length > prefix.length + 1)(value.substring(prefix.length))
//       .flatMap: value =>
//         try UUID.fromString(value).some
//         catch { case _: IllegalArgumentException => none }
//   }(uuid => prefix + uuid.show)

//   val codec: Primitive[SessionApiSchema] = codec(prefix = "")
