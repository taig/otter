// package io.taig.otter.sample

// import io.taig.otter.http.json as HttpJson
// import io.taig.otter.openapi as Openapi
// import io.taig.otter.sample.api.endpoints.AuthenticatedEndpoint
// import io.taig.otter.sample.api.*
// import org.typelevel.ci.*
// import java.util.regex.Pattern
// import cats.syntax.all.*

// object Dsl extends HttpJson.Dsl, Openapi.Dsl:
//   val email: Primitive.Required[CIString] = cistring(matches = Pattern.compile(".+@.+", Pattern.CASE_INSENSITIVE).some)

//   extension [I, O](self: Endpoint[I, O])
//     def role[R <: Role](role: R): AuthenticatedEndpoint[R, I, O] = AuthenticatedEndpoint(
//       role,
//       self
//         .modifyRequest(request => (headers.session.optional *: request).to[Authentication[I]])
//         .modifyResponse(_.modifyResults(Authentication.codec.orElse))
//     )
