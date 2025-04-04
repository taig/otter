// package io.taig.otter.http.header

// import cats.Show
// import io.taig.otter.http.Printers

// final case class MediaRange(tpe: MediaRange.Type, parameters: Parameters):
//   override def toString: String = Printers(this)

// object MediaRange:
//   enum Type:
//     case Secondary(primary: String, secondary: String)
//     case Primary(primary: String)
//     case Any

//     override def toString: String = Printers(this)

//   object Type:
//     given Show[MediaRange.Type] = Show.fromToString

//   val Any: MediaRange = MediaRange(Type.Any, Parameters.Empty)

//   given Show[MediaRange] = Show.fromToString
