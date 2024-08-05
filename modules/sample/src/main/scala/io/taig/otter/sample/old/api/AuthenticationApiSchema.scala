// package io.taig.otter.sample.api

// import io.taig.otter.http.Results
// import io.taig.otter.sample.Dsl.*

// import scala.util.control.NoStackTrace
// import io.taig.otter.sample.data.Session

// final case class AuthenticationApiSchema[A](session: Option[Session], payload: A)

// object AuthenticationApiSchema:
//   enum Error extends NoStackTrace:
//     case UserUnknown
//     case Forbidden

//   val codec: Results[Authentication.Error] =
//     val userUnknown: Codec[Authentication.Error.UserUnknown.type] =
//       error("userUnknown", singleton(Authentication.Error.UserUnknown))
//     val permissionDenied: Codec[Authentication.Error.Forbidden.type] =
//       error("permissionDenied", singleton(Authentication.Error.Forbidden))

//     (result(code.unauthorized, json.output(userUnknown)) :+ result(code.forbidden, json.output(permissionDenied))).to
