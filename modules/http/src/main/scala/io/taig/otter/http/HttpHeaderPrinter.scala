// package io.taig.otter.http

// import cats.data.Chain
// import cats.syntax.all.*
// import io.taig.otter.*

// final class HttpHeaderPrinter(explode: Boolean) extends Printer[Http.Header]:
//   override def apply[A](codec: Http.Header[A], a: A): String = codec match
//     case codec: Http.Header.Array[A]  => array(apply(codec, a))
//     case codec: Http.Header.Object[A] => obj(values = apply(codec, a))
//     case codec: Http.Header.Value[A]  => HttpHeaderValuePrinter(codec, a)

//   def apply[A](codec: Http.Header.Array[A], a: A): Chain[String] = codec match
//     case Http.Header.Array.Collection(self) =>
//       Chain.fromSeq(CollectionPrinter(printer = HttpHeaderValuePrinter)(codec = self, a))
//     case Http.Header.Array.Tuple(self) =>
//       TuplePrinter(printer = HttpHeaderValuePrinter)(codec = self, a)

//   def apply[A](codec: Http.Header.Object[A], a: A): Chain[(String, String)] = codec match
//     case Http.Header.Object.Dictionary(self) =>
//       Chain.fromSeq(DictionaryPrinter(printer = HttpHeaderValuePrinter)(codec = self, a))
//     case Http.Header.Object.Record(self) => ??? // RecordPrinter(printer = HttpHeaderValuePrinter)(codec = self, a)

//   def array(values: Chain[String]): String = values.map(escape(_, ",")).mkString_(",")

//   def obj(values: Chain[(String, String)]): String =
//     if explode then
//       val characters = List("=", ",")
//       values.map((name, value) => s"${escape(name, characters)}=${escape(value, characters)}").mkString_(",")
//     else values.map((name, value) => s"${escape(name, ",")},${escape(value, ",")}").mkString_(",")

// object HttpHeaderPrinter:
//   def apply(explode: Boolean): Printer[Http.Header] = new HttpHeaderPrinter(explode)
