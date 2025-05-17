// package io.taig.otter.http

// import cats.data.Chain
// import cats.syntax.all.*
// import io.taig.otter.*

// final class HttpQueryPrinter(explode: Boolean, style: Query.Style):
//   def apply[A](name: String, codec: Http.Query[A], a: A): Chain[(String, Option[String])] = codec match
//     case codec: Http.Query.Array[?] =>
//       array(name, values = apply(codec, a)).map(_.map(_.some))
//     case codec: Http.Query.Object[?] =>
//       obj(name, values = apply(codec, a)).map(_.map(_.some))
//     case codec: Http.Query.Optional[?] => apply(name, codec, a)
//     case codec: Http.Query.Value[?]    => Chain.one((name, HttpQueryValuePrinter(codec, a).some))

//   def apply[A](codec: Http.Query.Array[A], a: A): Chain[String] = codec match
//     case Http.Query.Array.Collection(self) =>
//       Chain.fromSeq(CollectionPrinter(printer = HttpQueryValuePrinter)(codec = self, a))
//     case Http.Query.Array.Tuple(self) => TuplePrinter(printer = HttpQueryValuePrinter)(codec = self, a)

//   def apply[A](codec: Http.Query.Object[A], a: A): Chain[(String, String)] = codec match
//     case Http.Query.Object.Dictionary(self) =>
//       Chain.fromSeq(DictionaryPrinter(printer = HttpQueryValuePrinter)(codec = self, a))
//     case Http.Query.Object.Record(self) => ??? // RecordPrinter(printer = HttpQueryValuePrinter)(codec = self, a)

//   def apply[A](name: String, codec: Http.Query.Optional[A], a: A): Chain[(String, Option[String])] =
//     apply(name, codec = codec.self, a)

//   def apply[A](name: String, codec: Nullable[Http.Query, A], a: A): Chain[(String, Option[String])] = codec match
//     case Nullable.Default(codec, _, _) => apply(name, codec = codec.value, a)
//     case Nullable.Modify(self, _, g)   => apply(name, codec = self, g(a))
//     case Nullable.Root(codec, _)       => a.fold(Chain.one((name, none)))(apply(name, codec = codec.value, _))

//   def array[A](name: String, values: Chain[String]): Chain[(String, String)] = (explode, style) match
//     case (true, _)                       => values.tupleLeft(name)
//     case (_, Query.Style.Form)           => Chain.one((name, values.map(escape(_, ",")).mkString_(",")))
//     case (_, Query.Style.SpaceDelimited) => Chain.one((name, values.map(escape(_, " ")).mkString_(" ")))
//     case (_, Query.Style.PipeDelimited)  => Chain.one((name, values.map(escape(_, "|")).mkString_("|")))

//   def obj[A](name: String, values: Chain[(String, String)]): Chain[(String, String)] = (explode, style) match
//     case (false, Query.Style.Form) =>
//       Chain.one((name, values.map((key, value) => s"${escape(key, ",")},${escape(value, ",")}").mkString_(",")))
//     case (_, _) => values
