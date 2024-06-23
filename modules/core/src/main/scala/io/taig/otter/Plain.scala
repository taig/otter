package io.taig.otter

import io.taig.otter as Base

sealed trait MyMetadata

object MyMetadata:
  final case class Collection() extends MyMetadata

trait Plain extends Dsl:
  override object metadata extends Metadata:
    override type Schema = MyMetadata
    override type Collection = MyMetadata.Collection
    override type Enumeration = MyMetadata
    override type Primitive = MyMetadata

  // override given asSchema: ApplicativeComonad[AsSchema] with
  //   override def ap[A, B](ff: AsSchema[A => B])(fa: AsSchema[A]): AsSchema[B] = ff(fa)
  //   override def pure[A](x: A): AsSchema[A] = x
  //   override def coflatMap[A, B](fa: AsSchema[A])(f: AsSchema[A] => B): AsSchema[B] = f(fa)
  //   override def extract[A](x: AsSchema[A]): A = x

  // override given asCollection: ApplicativeComonad[AsCollection] with
  //   override def ap[A, B](ff: AsCollection[A => B])(fa: AsCollection[A]): AsCollection[B] = ff(fa)
  //   override def pure[A](x: A): AsCollection[A] = x
  //   override def coflatMap[A, B](fa: AsCollection[A])(f: AsCollection[A] => B): AsCollection[B] = f(fa)
  //   override def extract[A](x: AsCollection[A]): A = x

  // override given asTuple: ApplicativeComonad[AsTuple] with
  //   override def ap[A, B](ff: AsTuple[A => B])(fa: AsTuple[A]): AsTuple[B] = ff(fa)
  //   override def pure[A](x: A): AsTuple[A] = x
  //   override def coflatMap[A, B](fa: AsTuple[A])(f: AsTuple[A] => B): AsTuple[B] = f(fa)
  //   override def extract[A](x: AsTuple[A]): A = x

  // override given asPrimitive: ApplicativeComonad[AsPrimitive] with
  //   override def ap[A, B](ff: AsPrimitive[A => B])(fa: AsPrimitive[A]): AsPrimitive[B] = ff(fa)
  //   override def pure[A](x: A): AsPrimitive[A] = x
  //   override def coflatMap[A, B](fa: AsPrimitive[A])(f: AsPrimitive[A] => B): AsPrimitive[B] = f(fa)
  //   override def extract[A](x: AsPrimitive[A]): A = x

object Plain extends Plain
