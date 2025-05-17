// package io.taig.otter.http

// import io.taig.otter.http.header.MediaType
// import io.taig.otter.http.header.Parameters

// trait HttpFormDataDsl:
//   final def formData[A](codec: => FormData[A]): Body[FormData, A] = BodyDsl.body(
//     mediaType = MediaType(
//       tpe = MediaType.Type(primary = "application", secondary = "x-www-form-urlencoded"),
//       parameters = Parameters.Empty
//     ),
//     codec
//   )

// object HttpFormDataDsl extends HttpFormDataDsl
