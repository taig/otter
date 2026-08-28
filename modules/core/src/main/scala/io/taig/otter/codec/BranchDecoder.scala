package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Branch
import io.taig.otter.Violations

final class BranchDecoder[F[-_, +_], T](decoder: Decoder[F, T]) extends Decoder[[w, r] =>> Branch[F, w, r], T]:
  override def decode[R](schema: Branch[F, Nothing, R], value: T): Validated[Violations, R] = (schema: @unchecked) match
    case schema: Branch.Modify[F, ?, ?, ?, R] => decode(schema.self, value).map(schema.f)
    case schema: Branch.Root[F, ?, R]         => decoder.decode(schema.reference.value, value).leftMap(schema.name /: _)
