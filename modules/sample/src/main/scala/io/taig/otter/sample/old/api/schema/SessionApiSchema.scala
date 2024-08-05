// package io.taig.otter.sample.api.schema

// import java.util.UUID
// import io.taig.otter.sample.Dsl.*
// import cats.syntax.all.*

// opaque type SessionApiSchema = UUID

// object SessionApiSchema:
//   def codec(prefix: String): Primitive.Required[SessionApiSchema] = parser("session") { value =>
//     Option
//       .when(value.startsWith(prefix) && value.length > prefix.length + 1)(value.substring(prefix.length + 1))
//       .flatMap(value =>
//         try UUID.fromString(value).some
//         catch { case _: IllegalArgumentException => none }
//       )
//   }(uuid => s"Bearer $uuid")

//   val codec: Primitive.Required[SessionApiSchema] = codec(prefix = "")
