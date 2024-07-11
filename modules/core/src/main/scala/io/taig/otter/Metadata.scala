package io.taig.otter

import cats.syntax.all.*

opaque type Metadata[A] = Map[String, Metadata.Value[A]]

object Metadata:
  opaque type Key[A] = String

  enum Value[+A]:
    case Attached(value: A)
    case Detached(value: Any)

  extension [A](self: Metadata[A])
    def imap[B](f: A => B)(g: B => A): Metadata[B] = (self: Map[String, Metadata.Value[A]]).fmap:
      case Value.Attached(value) => Value.Attached(f(value))
      case Value.Detached(value) => Value.Detached(value)

    inline def put(key: Key[A], value: A): Metadata[A] = self.updated(key, Value.Attached(value))
    inline def put[B](key: Key[B], value: B): Metadata[A] = self.updated(key, Value.Detached(value))
