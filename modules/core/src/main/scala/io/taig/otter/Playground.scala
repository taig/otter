package io.taig.otter

import io.taig.otter.Dsl.*
import io.taig.otter.Evidence.Product.Aux
import scala.deriving.Mirror

abstract class MyWrapper[A]:
  def imap[B](f: A => B)(g: B => A): MyWrapper[B]
  def to[B](using evidence: Evidence.Product.Aux[B, A]): MyWrapper[B] =
    imap(evidence.from)(evidence.to)

abstract class MyChildWrapper[A] extends MyWrapper[A]:
  override def imap[B](f: A => B)(g: B => A): MyChildWrapper[B]
  final override def to[B](using evidence: Aux[B, A]): MyChildWrapper[B] =
    imap(evidence.from)(evidence.to)

object Playground:

  val x = record(field("reference", long)).to[Constraint.Collection.MaxItems]

  sum
    .nested {
      branch("maxItems", record(field("reference", long)).to[Constraint.Collection.MaxItems]) :+
        branch("maxItems", record(field("reference", long)).to[Constraint.Collection.MinItems]) :+
        branch("uniqueItems", singleton(Constraint.Collection.UniqueItems))
    }
    .to[Constraint.Collection]
