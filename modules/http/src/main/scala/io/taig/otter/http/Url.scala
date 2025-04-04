// package io.taig.otter.http

// import cats.syntax.all.*
// import io.taig.otter.Codec
// import io.taig.otter.Merge

// sealed abstract class Url[A]:
//   self =>

//   def path: Path[?]

//   def queries: Queries[?]

//   final def matches(url: Http.Url): Boolean =
//     self.path.matches(url.path) && self.queries.matches(url.queries)

//   final def imap[B](f: A => B)(g: B => A): Url[B] = new Url[B]:
//     export self.{path, queries}
//     override def decodeWithRemainders(values: Http.Url): Codec.Result[(Http.Url, B)] =
//       self.decodeWithRemainders(values).map(_.map(f))
//     override def encode(b: B): Http.Url = self.encode(g(b))

//   final def zip[B](url: Url[B]): Url[(A, B)] = new Url[(A, B)]:
//     override def path: Path[?] = self.path.zip(url.path)
//     override def queries: Queries[?] = self.queries.zip(url.queries)
//     override def decodeWithRemainders(values: Http.Url): Codec.Result[(Http.Url, (A, B))] =
//       self
//         .decodeWithRemainders(values)
//         .andThen:
//           case (values, a) =>
//             url.decodeWithRemainders(values).map(_.tupleLeft(a))
//     override def encode(ab: (A, B)): Http.Url = self.encode(ab._1) ++ url.encode(ab._2)

//   final def zip[B](path: Path[B]): Url[(A, B)] = zip(path.toUrl)

//   final def /[B](path: Path[B])(using merge: Merge[A, B]): Url[merge.Out] = zip(path).imap(merge.apply)(merge.unapply)
//   final def /(segment: String): Url[A] = zip(Segment.Static(segment).toPath).imap { case (a, _) => a }(a => (a, ()))
//   final def /[B](segment: Segment.Parameter[B])(using merge: Merge[A, B]): Url[merge.Out] = /(segment.toPath)

//   final def zip[B](queries: Queries[B]): Url[(A, B)] = zip(queries.toUrl)
//   final def &[B](queries: Queries[B])(using merge: Merge[A, B]): Url[merge.Out] =
//     zip(queries).imap(merge.apply)(merge.unapply)
//   final def &[B](query: Query[B])(using merge: Merge[A, B]): Url[merge.Out] = &(query.toQueries)

//   final def decode(values: Http.Url): Codec.Result[A] =
//     decodeWithRemainders(values).map { case (_, a) => a }

//   protected def decodeWithRemainders(values: Http.Url): Codec.Result[(Http.Url, A)]

//   def encode(a: A): Http.Url

// object Url:
//   val Empty: Url[Unit] = new Url[Unit]:
//     override def path: Path[?] = Path.Empty
//     override def queries: Queries[?] = Queries.Empty
//     override def decodeWithRemainders(values: Http.Url): Codec.Result[(Http.Url, Unit)] =
//       (values, ()).valid
//     override def encode(a: Unit): Http.Url = Http.Url.Empty

//   def apply[A, B](path: Path[A], queries: Queries[B]): Url[(A, B)] =
//     val _path = path
//     val _queries = queries

//     new Url[(A, B)]:
//       override def path: Path[A] = _path
//       override def queries: Queries[B] = _queries
//       override def decodeWithRemainders(values: Http.Url): Codec.Result[(Http.Url, (A, B))] =
//         val (remainders, b) = queries.decode(values.queries)
//         (path.decodeWithRemainders(values.path), b)
//           .mapN { case ((path, a), b) => (Http.Url(path, remainders), (a, b)) }
//       override def encode(ab: (A, B)): Http.Url = Http.Url(path = path.encode(ab._1), queries = queries.encode(ab._2))

//   def apply[A](path: Path[A]): Url[A] = Url(path, Queries.Empty).imap { case (a, _) => a } { case a => (a, ()) }

//   def apply[A](queries: Queries[A]): Url[A] = Url(Path.Empty, queries).imap { case (_, a) => a } { case a => ((), a) }
