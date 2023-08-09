//package io.taig.otter.http
//
//import cats.data.NonEmptyChain
//import cats.syntax.all.*
//import io.taig.otter.schema.+
//
//sealed abstract class Results[A]:
//  def toNonEmptyChain: NonEmptyChain[Result[?]]
//
//object Results:
//  final private[otter] case class Root[A](result: Result[A]) extends Results[A]:
//    override def toNonEmptyChain: NonEmptyChain[Result[?]] = NonEmptyChain.one(result)
//
//  final private[otter] case class OrElse[A, B](left: Results[A], right: Results[B]) extends Results[A + B]:
//    override def toNonEmptyChain: NonEmptyChain[Result[?]] = left.toNonEmptyChain.combine(right.toNonEmptyChain)
//
//  final private[otter] case class Modify[A, B](self: Results[A], f: A => B, g: B => A) extends Results[B]:
//    export self.toNonEmptyChain
//
//  def apply[A](result: Result[A]): Results[A] = Root(result)
