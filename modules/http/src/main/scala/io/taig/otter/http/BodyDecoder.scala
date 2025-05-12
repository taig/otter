package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.http.HttpError.MediaTypeUnsupported
import io.taig.otter.http.HttpError.ValidationViolations
import io.taig.otter.http.header.MediaType
import org.typelevel.ci.*

import java.nio.charset.Charset

final class BodyDecoder[S[_]](decoder: PayloadDecoder[S]):
  def apply[A](
      codec: Body[S, A],
      contentType: Option[MediaType],
      bytes: Array[Byte]
  ): Either[MediaTypeUnsupported | ValidationViolations, A] = codec match
    case Body.Modify(self, f, _) => apply(codec = self, contentType, bytes).map(f)
    case Body.Root(mediaType, codec) =>
      contentType match
        case Some(contentType) if contentType === mediaType =>
          val charset = contentType.parameters
            .get(ci"charset")
            .headOption
            .flatMap(value =>
              try Charset.forName(value).some
              catch { case _: IllegalArgumentException => none }
            )

          decoder(codec = codec.value, charset, bytes).toEither.leftMap(ValidationViolations.apply)
        case Some(_) => MediaTypeUnsupported.asLeft
        case None    => decoder(codec = codec.value, charset = none, bytes).toEither.leftMap(ValidationViolations.apply)
