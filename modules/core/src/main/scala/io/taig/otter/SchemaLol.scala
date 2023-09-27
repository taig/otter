//package io.taig.otter
//
//import cats.data.Chain
//
//sealed abstract class SchemaLol[+A]:
//  self =>
//  type Self[+a] <: SchemaLol[a]
//  type Of <: SchemaLol[?]
//
//  def description: Option[String]
//  def description(f: Option[String] => Option[String]): Self[A]
//
//  final def orElse[B](schema: SchemaLol[B]): SchemaLol.Of[self.Of | schema.Of, Either[A, B]] = ???
//
//object SchemaLol:
//  type Of[+A <: SchemaLol[?], +B] <: SchemaLol[B] { type Of <: A }
//
//  sealed abstract class Value[+A] extends SchemaLol[A] {
//    self =>
//    override type Self[+a] <: SchemaLol.Value[a]
//    override type Of <: SchemaLol.Value[?]
//  }
//
//  sealed abstract class Primitive[+A] extends Value[A] {
//    override type Self[+a] = Primitive[a]
//    override type Of <: Primitive[A]
//  }
//  object Primitive {
//    final case class Root[A](description: Option[String]) extends Primitive[A] {
//      override def description(f: Option[String] => Option[String]): Primitive[A] = ???
//    }
//  }
//
//  sealed abstract class Enumeration[+A] extends Value[A] {
//    final override type Self[+a] = Enumeration[a]
//    override type Of <: Enumeration[A]
//
//  }
//  sealed abstract class Record[+A] extends SchemaLol[A] {
//    final override type Self[+a] = Record[a]
//    override type Of <: Record[A]
//
//  }
//  sealed abstract class Collection[+A] extends SchemaLol[A] {
//    final override type Self[+a] = Collection[a]
//    final override type Of <: Collection[A]
//  }
//  object Collection {
////    final case class Root[A](example: Option[Chain[A]]) extends SchemaLol[Chain[A]] {
////      override def description: Property[Option[String]] = ???
////      override def modifyDescription(f: Property[Option[String]] => Property[Option[String]]): Self[Chain[A]] = ???
////      override def modifyExample[B](f: Option[Chain[A]] => Option[B]): Self[B] = ???
////      override def modifyDefault[B](f: Option[Chain[A]] => Option[B]): Self[B] = ???
////    }
//  }
//
//object Yolo {
//  val a: SchemaLol.Primitive[String] = ???
//  val b: SchemaLol.Enumeration[Int] = ???
//  val c: SchemaLol.Record[Long] = ???
//
////  val aaa: SchemaLol.Primitive[String] | SchemaLol.Enumeration[Int] = ???
////  val bbb: SchemaLol.Value[?] = aaa
//
//  val x: SchemaLol.Of[SchemaLol.Primitive[String] | SchemaLol.Enumeration[Int], Either[String, Int]] = a.orElse(b)
//  val y: SchemaLol.Of[SchemaLol.Value[String | Int], Either[String, Int]] = x
//  val z: SchemaLol.Of[SchemaLol.Value[?], Either[String, Int]] = x
//}
