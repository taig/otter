// package io.taig.otter.sample.api

// import io.taig.otter.http.Header
// import io.taig.otter.sample.api.schema.SessionApiSchema
// import io.taig.otter.sample.Dsl.*
// import org.typelevel.ci.*

// object headers:
//   val session: Header[SessionApiSchema] =
//     header.authorization(SessionApiSchema.codec(prefix = "Bearer "))
