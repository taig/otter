package io.taig.otter

import cats.Eq
import cats.data.Chain
import cats.syntax.all.*

object syntax:
  extension [A: Eq, B](self: Chain[(A, B)])
    private[otter] def all(key: A): Chain[B] = self.collect { case (reference, value) if key === reference => value }
    private[otter] def first(key: A): Option[B] = self.collectFirst {
      case (reference, value) if key === reference => value
    }
    private[otter] def removeAll(key: A): Chain[(A, B)] = self.filter:
      case (reference, _) if key === reference => false
      case _                                   => true
    private[otter] def removeFirst(key: A): Chain[(A, B)] =
      var removed = false
      val result = List.newBuilder[(A, B)]
      self.iterator.foreach {
        case (reference, _) if key == reference && !removed => removed = true; ()
        case entry                                          => result += entry
      }
      Chain.fromSeq(result.result())
    private[otter] def allWithRemainders(key: A): (Chain[B], Chain[(A, B)]) = (all(key), removeAll(key))
    private[otter] def firstWithRemainders(key: A): Option[(B, Chain[(A, B)])] = first(key).tupleRight(removeFirst(key))
