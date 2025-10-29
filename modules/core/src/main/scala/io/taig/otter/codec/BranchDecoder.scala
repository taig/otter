package io.taig.otter.codec

import cats.data.Validated

import io.taig.otter.Violations
import io.taig.otter.Branch
import cats.syntax.all.*

final class BranchDecoder[-S[_], T](decoder: Decoder[S, T]) extends Decoder[Branch[S, *], T]:
  override def decode[A](schema: Branch[S, A], value: T): Validated[Violations, A] = schema match
    case Branch.Modify(self, f, _) => decode(schema = self, value).map(f)
    case Branch.Root(name, schema) => decoder.decode(schema = schema.value, value).leftMap(name /: _)

object BranchDecoder:
  def apply[S[_], T](decoder: Decoder[S, T]): Decoder[Branch[S, *], T] = new BranchDecoder(decoder)
