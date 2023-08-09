//package io.taig.otter.http
//
//import cats.data.Chain
//
//sealed abstract class Headers[A]:
//  def toChain: Chain[Header[?]]
//
//object Headers:
//  private[otter] case object Root extends Headers[Unit]:
//    override def toChain: Chain[Header[?]] = Chain.empty
//
//  private[otter] case class One[A](header: Header[A]) extends Headers[A]:
//    override def toChain: Chain[Header[A]] = Chain.one(header)
//
//  private[otter] case class Zip[A, B](left: Headers[A], right: Headers[B]) extends Headers[(A, B)]:
//    override def toChain: Chain[Header[?]] = left.toChain ++ right.toChain
//
//  val Empty: Headers[Unit] = Root
