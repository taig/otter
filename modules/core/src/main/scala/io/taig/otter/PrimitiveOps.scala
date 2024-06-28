package io.taig.otter

import io.taig.otter.validation.Validation

trait PrimitiveOps[Self[_], Optional[_], Writer[_], Collection[_, _], Union[_, _]]
    extends SchemaOps[[_, a] =>> Self[a], [_, a] =>> Optional[a], Collection, Union],
      ValidationOps[[_, a] =>> Self[a], [a] =>> Constraint.Primitive[(Writer[a], a)], [a] =>> (Writer[a], a)]:
  extension [A](self: Self[A]) def tpe: Type[?]

object PrimitiveOps:
  trait Isomorphic[Self[_], Optional[_], Writer[_], Collection[_, _], Union[_, _]]
      extends PrimitiveOps[Self, Optional, Writer, Collection, Union],
        SchemaOps.Isomorphic[[_, a] =>> Self[a], [_, a] =>> Optional[a], Collection, Union],
        ValidationOps.Isomorphic[[_,
        a] =>> Self[a], [a] =>> Constraint.Primitive[(Writer[a], a)], [a] =>> (Writer[a], a)]:
    extension [A, B](self: Self[B])
      override def imap[C](f: B => C)(g: C => B): Self[C] =
        self.ivalidate(Validation.lift(f))(g)

  trait Reader[Self[_], Optional[_], Writer[_], Collection[_, _], Union[_, _]]
      extends PrimitiveOps[Self, Optional, Writer, Collection, Union],
        SchemaOps.Reader[[_, a] =>> Self[a], [_, a] =>> Optional[a], Collection, Union],
        ValidationOps.Reader[[_, a] =>> Self[a], [a] =>> Constraint.Primitive[(Writer[a], a)], [a] =>> (Writer[a], a)]

  trait Writer[Self[_], Optional[_], Writer[_], Collection[_, _], Union[_, _]]
      extends PrimitiveOps[Self, Optional, Writer, Collection, Union],
        SchemaOps.Writer[[_, a] =>> Self[a], [_, a] =>> Optional[a], Collection, Union],
        ValidationOps.Writer[[_, a] =>> Self[a], [a] =>> Constraint.Primitive[(Writer[a], a)], [a] =>> (Writer[a], a)]
