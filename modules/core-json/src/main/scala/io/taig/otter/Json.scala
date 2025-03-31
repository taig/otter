package io.taig.otter

import io.taig.otter as Self
import cats.Invariant

final case class Json[A](
    self: Collection[Json, A] | Constant[Json, A] | Dictionary[Json.Key, Json, A] | Enumeration[A] | Optional[Json, A] |
      Primitive[A] | Record[Json, A] | Tuple[Json, A] | Union[Json, A]
) extends AnyVal

object Json:
  final case class Key[A](self: Constant[Json.Key, A] | Enumeration[A] | Primitive[A] | Union.Untagged[Json.Key, A])
      extends AnyVal

  object Key:
    given Invariant[Json.Key] = new Invariant[Json.Key]:
      override def imap[A, B](fa: Json.Key[A])(f: A => B)(g: B => A): Json.Key[B] = Json.Key:
        fa.self match
          case codec: Constant[Json.Key, A]       => codec.imap(f)(g)
          case codec: Enumeration[A]              => codec.imap(f)(g)
          case codec: Primitive[A]                => codec.imap(f)(g)
          case codec: Union.Untagged[Json.Key, A] => codec.imap(f)(g)

  given Invariant[Json] = new Invariant[Json]:
    override def imap[A, B](fa: Json[A])(f: A => B)(g: B => A): Json[B] = Json:
      fa.self match
        case codec: Collection[Json, A]           => codec.imap(f)(g)
        case codec: Constant[Json, A]             => codec.imap(f)(g)
        case codec: Dictionary[Json.Key, Json, A] => codec.imap(f)(g)
        case codec: Enumeration[A]                => codec.imap(f)(g)
        case codec: Optional[Json, A]             => codec.imap(f)(g)
        case codec: Primitive[A]                  => codec.imap(f)(g)
        case codec: Record[Json, A]               => codec.imap(f)(g)
        case codec: Tuple[Json, A]                => codec.imap(f)(g)
        case codec: Union[Json, A]                => codec.imap(f)(g)
