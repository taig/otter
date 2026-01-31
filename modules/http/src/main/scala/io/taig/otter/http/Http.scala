// package io.taig.otter.http

// import io.taig.otter.syntax.all.*
// import io.taig.otter.http as Self
// import io.taig.otter.Annotation
// import io.taig.otter.http.operation.SegmentOperation
// import cats.Functor
// import io.taig.otter.Annotated
// import io.taig.otter.http.operation.QueryOperation
// import io.taig.otter.http.operation.QueriesOperation
// import cats.Contravariant
// import cats.Invariant
// import cats.Apply
// import cats.ContravariantSemigroupal
// import cats.InvariantSemigroupal
// import Self.operation.QueriesableOperation

// object Http:
//   final case class Path[A](self: Annotation[Self.Path[Http.Segment, A]]) extends Http.Path.Read[A], Http.Path.Write[A]

//   object Path:
//     sealed trait Read[+A]:
//       def self: Annotation[Self.Path.Read[Http.Segment.Read, A]]

//     sealed trait Write[-A]:
//       def self: Annotation[Self.Path.Write[Http.Segment.Write, A]]

//   final case class Query[A](self: Annotation[Self.Query[Http.Query.Parameter, A]])
//       extends Http.Query.Read[A],
//         Http.Query.Write[A]

//   object Query:
//     sealed trait Read[+A]:
//       def self: Annotation[Self.Query.Read[Http.Query.Parameter.Read, A]]

//     object Read:
//       def apply[A](annotation: Annotation[Self.Query.Read[Http.Query.Parameter.Read, A]]) = new Read[A]:
//         override def self: Annotation[Self.Query.Read[Http.Query.Parameter.Read, A]] = annotation

//       given Functor[Http.Query.Read] = Functor[[a] =>> Annotation[Self.Query.Read[Http.Query.Parameter.Read, a]]]
//         .imapK([_] => Read(_))([_] => _.self)

//       given [A] => Annotated[Http.Query.Read[A]] =
//         Annotated[Annotation[Self.Query.Read[Http.Query.Parameter.Read, A]]].imap(Read.apply)(_.self)

//       given query: QueryOperation.Read[Http.Query.Read, Http.Query.Parameter.Read] = QueryOperation
//         .Read[[a] =>> Annotation[Self.Query.Read[Http.Query.Parameter.Read, a]], Http.Query.Parameter.Read]
//         .imapK([_] => Read(_))([_] => _.self)

//       given queriesable: QueriesableOperation.Read[Http.Query.Read, Http.Queries.Read] =
//         QueriesableOperation.Read.derived

//     sealed trait Write[-A]:
//       def self: Annotation[Self.Query.Write[Http.Query.Parameter.Write, A]]

//     object Write:
//       def apply[A](annotation: Annotation[Self.Query.Write[Http.Query.Parameter.Write, A]]) = new Write[A]:
//         override def self: Annotation[Self.Query.Write[Http.Query.Parameter.Write, A]] = annotation

//       given Contravariant[Http.Query.Write] =
//         Contravariant[[a] =>> Annotation[Self.Query.Write[Http.Query.Parameter.Write, a]]]
//           .imapK([_] => Write(_))([_] => _.self)

//       given [A] => Annotated[Http.Query.Write[A]] =
//         Annotated[Annotation[Self.Query.Write[Http.Query.Parameter.Write, A]]].imap(Write.apply)(_.self)

//       given query: QueryOperation.Write[Http.Query.Write, Http.Query.Parameter.Write] = QueryOperation
//         .Write[[a] =>> Annotation[Self.Query.Write[Http.Query.Parameter.Write, a]], Http.Query.Parameter.Write]
//         .imapK([_] => Write(_))([_] => _.self)

//       given queriesable: QueriesableOperation.Write[Http.Query.Write, Http.Queries.Write] =
//         QueriesableOperation.Write.derived

//     given Invariant[Query] = Invariant[[a] =>> Annotation[Self.Query[Http.Query.Parameter, a]]]
//       .imapK([_] => Query(_))([_] => _.self)

//     given [A] => Annotated[Http.Query[A]] =
//       Annotated[Annotation[Self.Query[Http.Query.Parameter, A]]].imap(Query.apply)(_.self)

//     given QueryOperation[Http.Query, Http.Query.Parameter] = QueryOperation[
//       [a] =>> Annotation[Self.Query[Http.Query.Parameter, a]],
//       Http.Query.Parameter
//     ].imapK([_] => Query(_))([_] => _.self)

//     given QueriesableOperation[Http.Query, Http.Queries] = QueriesableOperation.derived

//     export Self.QueryParameter as Parameter

//   final case class Queries[A](self: Annotation[Self.Queries[Http.Query, A]])
//       extends Http.Queries.Read[A],
//         Http.Queries.Write[A]

//   object Queries:
//     sealed trait Read[+A]:
//       def self: Annotation[Self.Queries.Read[Http.Query.Read, A]]

//     object Read:
//       def apply[A](annotation: Annotation[Self.Queries.Read[Http.Query.Read, A]]) = new Read[A]:
//         override def self: Annotation[Self.Queries.Read[Http.Query.Read, A]] = annotation

//       given Apply[Http.Queries.Read] = Apply[[a] =>> Annotation[Self.Queries.Read[Http.Query.Read, a]]]
//         .imapK([_] => Read(_))([_] => _.self)

//       given [A] => Annotated[Http.Queries.Read[A]] =
//         Annotated[Annotation[Self.Queries.Read[Http.Query.Read, A]]].imap(Read.apply)(_.self)

//       given QueriesOperation.Read[Http.Queries.Read, Http.Query.Read] = QueriesOperation
//         .Read[[a] =>> Annotation[Self.Queries.Read[Http.Query.Read, a]], Http.Query.Read]
//         .imapK([_] => Read(_))([_] => _.self)

//     sealed trait Write[-A]:
//       def self: Annotation[Self.Queries.Write[Http.Query.Write, A]]

//     object Write:
//       def apply[A](annotation: Annotation[Self.Queries.Write[Http.Query.Write, A]]) = new Write[A]:
//         override def self: Annotation[Self.Queries.Write[Http.Query.Write, A]] = annotation

//       given ContravariantSemigroupal[Http.Queries.Write] =
//         ContravariantSemigroupal[[a] =>> Annotation[Self.Queries.Write[Http.Query.Write, a]]]
//           .imapK([_] => Write(_))([_] => _.self)

//       given [A] => Annotated[Http.Queries.Write[A]] =
//         Annotated[Annotation[Self.Queries.Write[Http.Query.Write, A]]].imap(Write.apply)(_.self)

//       given QueriesOperation.Write[Http.Queries.Write, Http.Query.Write] = QueriesOperation
//         .Write[[a] =>> Annotation[Self.Queries.Write[Http.Query.Write, a]], Http.Query.Write]
//         .imapK([_] => Write(_))([_] => _.self)

//     given InvariantSemigroupal[Http.Queries] =
//       InvariantSemigroupal[[a] =>> Annotation[Self.Queries[Http.Query, a]]]
//         .imapK([_] => Queries(_))([_] => _.self)

//     given [A] => Annotated[Http.Queries[A]] =
//       Annotated[Annotation[Self.Queries[Http.Query, A]]].imap(Queries.apply)(_.self)

//     given QueriesOperation[Http.Queries, Http.Query] =
//       QueriesOperation[[a] =>> Annotation[Self.Queries[Http.Query, a]], Http.Query]
//         .imapK([_] => Queries(_))([_] => _.self)
