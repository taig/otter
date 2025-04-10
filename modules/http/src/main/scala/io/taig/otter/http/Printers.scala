// package io.taig.otter.http

// import cats.Show
// import cats.syntax.all.*
// import io.taig.otter.http.header.MediaRange
// import io.taig.otter.http.header.MediaType
// import io.taig.otter.http.header.Parameter
// import io.taig.otter.http.header.Parameters
// import io.taig.otter.http.header.Weighted

// private[http] object Printers:

//   // def apply(formData: FormData): String = formData.toVector
//   //   .map:
//   //     case (key, Some(value)) => s"$key=$value"
//   //     case (key, None)        => key
//   //   .mkString("&")

//   def error(name: String): String = s"Error: $name"
