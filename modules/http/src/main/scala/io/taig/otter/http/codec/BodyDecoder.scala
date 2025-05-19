package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.http.Body
import io.taig.otter.http.HttpError.MediaTypeUnsupported
import io.taig.otter.http.HttpError.ValidationViolations
import io.taig.otter.http.header.MediaType
import org.typelevel.ci.*

import java.nio.charset.Charset

final class BodyDecoder[-S[_]](decoder: PayloadDecoder[S]):
  def decode[A](
      schema: Body[S, A],
      contentType: Option[MediaType],
      bytes: Array[Byte]
  ): Either[MediaTypeUnsupported | ValidationViolations, A] = schema match
    case Body.Modify(self, f, _) => decode(schema = self, contentType, bytes).map(f)
    case Body.Root(mediaType, schema) =>
      contentType match
        case Some(contentType) if contentType === mediaType =>
          val charset = contentType.parameters
            .get(ci"charset")
            .headOption
            .flatMap(value =>
              try Charset.forName(value).some
              catch { case _: IllegalArgumentException => none }
            )

          decoder.decode(schema = schema.value, charset, bytes).toEither.leftMap(ValidationViolations.apply)
        case Some(_) => MediaTypeUnsupported.asLeft
        case None =>
          decoder.decode(schema = schema.value, charset = none, bytes).toEither.leftMap(ValidationViolations.apply)
