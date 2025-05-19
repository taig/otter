package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Url
import io.taig.otter.http.Url.Data

object UrlDataDecoder extends Decoder.Remainding[Url, Url.Data]:
  override def decode[A](schema: Url[A], value: Data): Validated[Violations, A] =
    decodeRemainding(schema, value).andThen: (data, a) =>
      Validated.cond(
        test = data.path.isEmpty,
        a,
        Violations.rootNec(Violation.equal(reference = "/", actual = "/" + data.path.mkString_("/")))
      )

  override def decodeRemainding[A](schema: Url[A], value: Data): Validated[Violations, (Data, A)] = schema match
    case Url.Empty              => (value, ()).valid
    case Url.Modify(self, f, _) => decodeRemainding(schema = self, value).map(_.map(f))
    case Url.Root(path, queries) =>
      PathDataDecoder
        .decodeRemainding(schema = path, value = value.path)
        .andThen: (path, a) =>
          QueriesDataDecoder
            .decodeRemainding(schema = queries, value = value.queries)
            .map((queries, b) => (Url.Data(path, queries), (a, b)))
    case Url.Zip(left, right) =>
      decodeRemainding(schema = left, value).andThen: (value, a) =>
        decodeRemainding(schema = right, value).map((value, b) => (value, (a, b)))
