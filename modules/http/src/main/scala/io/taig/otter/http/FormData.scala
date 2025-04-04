// package io.taig.otter.http

// import cats.Show
// import cats.syntax.all.*

// opaque type FormData = Vector[(String, Option[String])]

// object FormData:
//   extension (self: FormData)
//     inline def toVector: Vector[(String, Option[String])] = self
//     def get(key: String): Vector[Option[String]] = self.collect { case (`key`, value) => value }

//   def apply(values: Vector[(String, Option[String])]): FormData = values

//   // TODO proper parser
//   // TODO URL encoding / decoding (?)
//   def parse(value: String): FormData = value
//     .split('&')
//     .toVector
//     .map: value =>
//       value.split("=", 2) match
//         case Array(key, value) => (key, value.some)
//         case _                 => (value, none)

//   given Show[FormData] = Printers(_)
