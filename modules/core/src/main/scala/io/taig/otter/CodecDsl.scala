package io.taig.otter

import cats.~>

trait CodecDsl[Self[_]]:
  extension [A](self: Self[A])
    def metadata: Metadata
    def modifyMetadata(f: Metadata => Metadata): Self[A]
    def mapK[S[a] >: Self[a], T[_]](fK: S ~> T): T[A]
