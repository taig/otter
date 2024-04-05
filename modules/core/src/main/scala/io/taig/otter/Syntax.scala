package io.taig.otter

import io.taig.otter as Plain

trait Syntax extends Types:
  extension [S[a] <: Schema[a], A](self: S[A]) def imap[B](f: A => B)(g: B => A): S[B]
