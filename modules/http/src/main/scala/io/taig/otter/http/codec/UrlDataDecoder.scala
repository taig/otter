package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Constraint
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Url
import io.taig.otter.http.Url.Data

object UrlDataDecoder extends Decoder.Remaining[Url, Url.Data]:
  override def decode[A](schema: Url[A], value: Data): Validated[Violations, A] =
    decodeRemaining(schema, value).andThen: (data, a) =>
      Validated
        .cond(
          test = data.path.isEmpty,
          a,
          Violation.fromConstraint(
            constraint = Constraint.Generic.Equals(reference = "/"),
            actual = "/" + data.path.mkString_("/")
          )
        )
        .leftMap(Violations.rootNec)

  override def decodeRemaining[A](schema: Url[A], value: Data): Validated[Violations, (Data, A)] =
    decodeRemaining(schema = schema.value, value)

  def decodeRemaining[A](schema: Url.Value[A], value: Data): Validated[Violations, (Data, A)] = schema match
    case Url.Value.Empty               => (value, ()).valid
    case Url.Value.Modify(self, f, _)  => decodeRemaining(schema = self, value).map(_.map(f))
    case Url.Value.Root(path, queries) =>
      PathDataDecoder
        .decodeRemaining(schema = path, value = value.path)
        .andThen: (path, a) =>
          QueriesDataDecoder
            .decodeRemaining(schema = queries, value = value.queries)
            .map((queries, b) => (Url.Data(path, queries), (a, b)))
    case Url.Value.Zip(left, right) =>
      decodeRemaining(schema = left, value).andThen: (value, a) =>
        decodeRemaining(schema = right, value).map((value, b) => (value, (a, b)))
