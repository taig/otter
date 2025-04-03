package io.taig.otter

trait BranchInvariant[Self[_], Union[_]] extends CodecInvariant[Self]:
  given union: UnionInvariant[Union, Self]

  extension [A](self: Self[A])
    final def :+[B](branch: Self[B]): Union[Either[A, B]] = self.toUnion.orElse(branch.toUnion)
    final def +:[B](branch: Self[B]): Union[Either[B, A]] = branch.toUnion.orElse(self.toUnion)
    final def toUnion: Union[A] = union.one(self)
