package io.taig.otter.openapi

import cats.Applicative
import cats.Comonad
import io.taig.otter as Base
import io.taig.otter.ApplicativeComonad
import alleycats.Extract
import alleycats.Pure
import cats.Functor
import cats.Invariant

trait Dsl extends Base.Dsl:
  override object container extends Base.Metadata:
    override type Schema[+A] = Annotation[Metadata[A], A]
    override type Collection[+A] = Annotation[Metadata.Collection[A], A]
    override type Primitive[+A] = Annotation[Metadata.Primitive[A], A]
    override type Product[+A] = Annotation[Metadata.Product[A], A]
    override type Union[+A] = Annotation[Metadata.Union[A], A]

  type MySchema[A, B, C] = Annotation[Metadata[A], Base.Schema[[a] =>> Annotation[Metadata[a], a], A, B, C]]

  // val x: MySchema[?, ?, String] = ???

  // given xxx: Invariant[MySchema] = new Invariant[MySchema]:
  //   override def imap[A, B](fa: MySchema[A])(f: A => B)(g: B => A): MySchema[B] =
  //     fa.copy(metadata = fa.metadata.imap(f)(g), self = fa.self.imap(f)(g))

  override given schemaApplicativeComonad: ApplicativeComonad[container.Schema] =
    new Applicative[container.Schema] with Comonad[container.Schema]:
      override def pure[A](x: A): container.Schema[A] = Annotation(metadata = Metadata.Default, self = x)
      override def ap[A, B](ff: container.Schema[A => B])(fa: container.Schema[A]): container.Schema[B] =
        fa.copy(metadata = ???, self = ff.self(fa.self))
      override def coflatMap[A, B](fa: container.Schema[A])(f: container.Schema[A] => B): container.Schema[B] =
        fa.copy(metadata = ???, self = f(fa))
      override def extract[A](x: container.Schema[A]): A = x.self

  override given collectionApplicativeComonad: ApplicativeComonad[container.Collection] =
    new Applicative[container.Collection] with Comonad[container.Collection]:
      override def pure[A](x: A): container.Collection[A] = Annotation(metadata = Metadata.Collection.Default, self = x)
      override def ap[A, B](ff: container.Collection[A => B])(fa: container.Collection[A]): container.Collection[B] =
        fa.copy(metadata = ???, self = ff.self(fa.self))
      override def coflatMap[A, B](fa: container.Collection[A])(
          f: container.Collection[A] => B
      ): container.Collection[B] =
        fa.copy(metadata = ???, self = f(fa))
      override def extract[A](x: container.Collection[A]): A = x.self

  override given primitiveApplicativeComonad: ApplicativeComonad[container.Primitive] =
    new Applicative[container.Primitive] with Comonad[container.Primitive]:
      override def pure[A](x: A): container.Primitive[A] = Annotation(metadata = Metadata.Primitive.Default, self = x)
      override def ap[A, B](ff: container.Primitive[A => B])(fa: container.Primitive[A]): container.Primitive[B] =
        fa.copy(metadata = ???, self = ff.self(fa.self))
      override def coflatMap[A, B](fa: container.Primitive[A])(f: container.Primitive[A] => B): container.Primitive[B] =
        fa.copy(metadata = ???, self = f(fa))
      override def extract[A](x: container.Primitive[A]): A = x.self

  override given unionApplicativeComonad: ApplicativeComonad[container.Union] =
    new Applicative[container.Union] with Comonad[container.Union]:
      override def pure[A](x: A): container.Union[A] = Annotation(metadata = Metadata.Union.Default, self = x)
      override def ap[A, B](ff: container.Union[A => B])(fa: container.Union[A]): container.Union[B] =
        fa.copy(metadata = ???, self = ff.self(fa.self))
      override def coflatMap[A, B](fa: container.Union[A])(f: container.Union[A] => B): container.Union[B] =
        fa.copy(metadata = ???, self = f(fa))
      override def extract[A](x: container.Union[A]): A = x.self

object Dsl extends Dsl
