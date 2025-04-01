package io.taig.otter

import io.taig.otter as Self
import cats.Invariant

final case class Typescript[A](
    self: Collection[Typescript, A] | Constant[Primitive, A] | Dictionary[Typescript.Key, Typescript, A] |
      Enumeration[A] | Optional[Typescript, A] | Primitive[A] | Record[Typescript.Key, Typescript, A] |
      Tuple[Typescript, A] | Union[Typescript.Key, Typescript, A]
) extends AnyVal

object Typescript:
  type Key[A] = Primitive.String[A]

  given Invariant[Typescript] = new Invariant[Typescript]:
    override def imap[A, B](fa: Typescript[A])(f: A => B)(g: B => A): Typescript[B] = Typescript:
      fa.self match
        case codec: Collection[Typescript, A]                 => codec.imap(f)(g)
        case codec: Constant[Primitive, A]                    => codec.imap(f)(g)
        case codec: Dictionary[Typescript.Key, Typescript, A] => codec.imap(f)(g)
        case codec: Enumeration[A]                            => codec.imap(f)(g)
        case codec: Optional[Typescript, A]                   => codec.imap(f)(g)
        case codec: Primitive[A]                              => codec.imap(f)(g)
        case codec: Record[Typescript.Key, Typescript, A]     => codec.imap(f)(g)
        case codec: Tuple[Typescript, A]                      => codec.imap(f)(g)
        case codec: Union[Typescript.Key, Typescript, A]      => codec.imap(f)(g)
