package io.taig.otter

import cats.syntax.all.*
import cats.data.Validated

final class CollectionParser[S[_]](parser: Parser[S]):
  def apply[A](codec: Collection[S, A], values: List[String]): Validated[Violations, A] = codec match
    case Collection.Indexed(codec, _, _, _, _) =>
      values.toVector.zipWithIndex
        .traverse((value, index) => parser(codec = codec.value, value).leftMap(index /: _))
    case Collection.Linked(codec, _, _, _, _) =>
      values.zipWithIndex
        .traverse((value, index) => parser(codec = codec.value, value).leftMap(index /: _))
    case Collection.Modify(self, f, _) => apply(codec = self, values).map(f)
