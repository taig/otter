package io.taig.otter.http

import io.taig.otter as Base
import io.taig.otter.Type
import cats.syntax.all.*
import cats.Comonad

trait Dsl extends Base.Dsl:
  override object container extends Base.Container:
    override type Schema[+A] = Annotation[Metadata, A]
    override type Collection[+A] = Annotation[Metadata, A]
    override type Primitive[+A] = Annotation[Metadata.Primitive, A]
    override type Tuple[+A] = Annotation[Metadata, A]
    override type Union[+A] = Annotation[Metadata, A]

  given Comonad[container.Schema] with
    override def coflatMap[A, B](fa: container.Schema[A])(f: container.Schema[A] => B): container.Schema[B] =
      fa.copy(self = f(fa))
    override def extract[A](x: container.Schema[A]): A = x.self
    override def map[A, B](fa: container.Schema[A])(f: A => B): container.Schema[B] = fa.copy(self = f(fa.self))

  given Comonad[container.Primitive] with
    override def coflatMap[A, B](fa: container.Primitive[A])(f: container.Primitive[A] => B): container.Primitive[B] =
      fa.copy(self = f(fa))
    override def extract[A](x: container.Primitive[A]): A = x.self
    override def map[A, B](fa: container.Primitive[A])(f: A => B): container.Primitive[B] = fa.copy(self = f(fa.self))

  override def primitive[A](tpe: Type[A]): Primitive.Required[A] = Annotation(
    metadata = Metadata.Primitive(name = none),
    self = Base.Primitive.Required.Root(tpe)
  )

object Dsl extends Dsl
