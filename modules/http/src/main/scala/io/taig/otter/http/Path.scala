// package io.taig.otter.http

// import cats.InvariantSemigroupal
// import cats.data.{Chain, Validated}
// import cats.syntax.all.*
// import io.taig.otter.validation.Violations
// import io.taig.otter.Evidence

// sealed abstract class Path[A]:
//   self =>
//   def toChain: Chain[Segment[?]]

//   final def imap[B](f: A => B)(g: B => A): Path[B] = new Path[B]:
//     export self.{matchesWithRemainders, toChain}
//     override def decodeWithRemainders(remainders: Http.Path): Validated[Violations, (Http.Path, B)] =
//       self.decodeWithRemainders(remainders).map(_.map(f))
//     override def encode(b: B): Http.Path = self.encode(g(b))

//   final infix def product[B](path: Path[B]): Path[(A, B)] = new Path[(A, B)]:
//     override def toChain: Chain[Segment[?]] = self.toChain ++ path.toChain
//     override def matchesWithRemainders(remainders: Http.Path): Option[Http.Path] = self
//       .matchesWithRemainders(remainders)
//       .flatMap(path.matchesWithRemainders(_).orElse(path.matchesWithRemainders(remainders)))
//     override def decodeWithRemainders(remainders: Http.Path): Validated[Violations, (Http.Path, (A, B))] =
//       self.decodeWithRemainders(remainders).andThen { case (remainders, a) =>
//         path.decodeWithRemainders(remainders).map(_.tupleLeft(a))
//       }
//     override def encode(ab: (A, B)): Http.Path = self.encode(ab._1) ++ path.encode(ab._2)

//   final infix def zip[B](path: Path[B])(using evidence: Evidence.Merge[A, B]): Path[evidence.Out] =
//     product(path).imap(evidence.apply)(evidence.unapply)
//   final def /[B](path: Path[B])(using evidence: Evidence.Merge[A, B]): Path[evidence.Out] = zip(path)
//   final def /[B](segment: Segment[B])(using evidence: Evidence.Merge[A, B]): Path[evidence.Out] = /(segment.toPath)
//   final def /(static: String): Path[A] = /(Segment.Static(static))

//   final def matches(path: Http.Path): Boolean = matchesWithRemainders(path).exists(_.isEmpty)
//   def matchesWithRemainders(remainders: Http.Path): Option[Http.Path]

//   def decodeWithRemainders(remainders: Http.Path): Validated[Violations, (Http.Path, A)]
//   def encode(a: A): Http.Path

//   final def print: String = toChain match
//     case Chain.nil => "/"
//     case segments  => segments.map(_.print).mkString_("/", "/", "")

//   final def toUrl: Url[A] = Url(this)

// object Path:
//   val Root: Path[Unit] = new Path[Unit]:
//     override def toChain: Chain[Segment[?]] = Chain.empty
//     override def matchesWithRemainders(remainders: Http.Path): Option[Http.Path] = remainders.some
//     override def decodeWithRemainders(remainders: Http.Path): Validated[Violations, (Http.Path, Unit)] =
//       (remainders, ()).valid
//     override def encode(a: Unit): Http.Path = Chain.empty

//   def apply[A](segment: Segment[A]): Path[A] = new Path[A]:
//     override def toChain: Chain[Segment[?]] = Chain.one(segment)
//     // TODO test this, I don't think this will work
//     override def matchesWithRemainders(remainders: Http.Path): Option[Http.Path] = remainders.uncons match
//       case Some((head, tail)) => Option.when(segment.matches(head))(tail)
//       case None               => None
//     override def decodeWithRemainders(remainders: Http.Path): Validated[Violations, (Http.Path, A)] = remainders.uncons
//       .match
// //        case Some((head, tail)) if segment.isOptional =>
// //          segment.decode(head).tupleLeft(tail).findValid(segment.decode(none).tupleLeft(remainders))
//         case Some((head, tail)) => segment.decode(head).tupleLeft(tail)
//         case None               => ??? // segment.decode(none).tupleLeft(remainders)
//       .leftMap(_.modifyHistory(segment.name /: _))
//     override def encode(a: A): Http.Path = Chain.one(segment.encode(a))

//   given InvariantSemigroupal[Path] with
//     override def imap[A, B](fa: Path[A])(f: A => B)(g: B => A): Path[B] = fa.imap(f)(g)
//     override def product[A, B](fa: Path[A], fb: Path[B]): Path[(A, B)] = fa.zip(fb)
