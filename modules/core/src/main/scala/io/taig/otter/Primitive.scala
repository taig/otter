// package io.taig.otter

// sealed abstract class Primitive[A]
//     extends Primitive.Read[A]
//     with Primitive.Write[A]
//     with Primitive.Operation[Primitive, Primitive.Read, Primitive.Write, A]:
//   override def imap[B](f: A => B)(g: B => A): Primitive[B] = copy(asRead.map(f), asWrite.contramap(g))
//   override def optional: Primitive[Option[A]] = copy(asRead.optional, asWrite.optional)

//   private def copy[B](read: Primitive.Read[B], write: Primitive.Write[B]): Primitive[B] = new Primitive[B]:
//     override def asRead = read
//     override def asWrite = write
//     override def tpe: Type[?] = read.tpe

// object Primitive:
//   trait Operation[+S[_], +RS[a] >: S[a], +WS[a] >: S[a], A]
//       extends Schema.Operation[S, S, RS, RS, WS, WS, A]
//       with Primitive.Common

//   trait Common:
//     def tpe: Type[?]

//   sealed trait Required[A]
//       extends Primitive[A]
//       with Primitive.Required.Read[A]
//       with Primitive.Required.Write[A]
//       with Primitive.Required.Operation[
//         Primitive.Required,
//         Primitive,
//         Primitive.Required.Read,
//         Primitive.Read,
//         Primitive.Required.Write,
//         Primitive.Write,
//         A
//       ]:
//     override def imap[B](f: A => B)(g: B => A): Primitive.Required[B] = ???

//   object Required:
//     trait Operation[+S[_], +O[_], +WS[_], +WO[a] >: O[a], +RS[_], +RO[a] >: O[a], A]
//         extends Schema.Operation[S, O, WS, WO, RS, RO, A]
//         with Primitive.Common

//     sealed trait Read[A]
//         extends Primitive.Read[A]
//         with Primitive.Required.Read.Operation[Primitive.Required.Read, Primitive.Read, A]:
//       override def map[B](f: A => B): Primitive.Required.Read[B] = ???

//     object Read:
//       trait Operation[+S[_], +O[_], A] extends Schema.Operation.Read[S, O, A] with Primitive.Common

//     sealed trait Write[A]
//         extends Primitive.Write[A]
//         with Primitive.Required.Write.Operation[Primitive.Required.Write, Primitive.Write, A]:
//       override def contramap[B](f: B => A): Primitive.Required.Write[B] = ???

//     object Write:
//       trait Operation[+S[_], +O[_], A] extends Schema.Operation.Write[S, O, A] with Primitive.Common

//   sealed trait Read[A] extends Primitive.Read.Operation[Primitive.Read, A]:
//     override def map[B](f: A => B): Primitive.Read[B] = Primitive.Read.Modify(this, f)
//     override def optional: Primitive.Read[Option[A]] = Primitive.Read.Optional(this)

//   object Read:
//     trait Operation[+S[_], A] extends Schema.Operation.Read[S, S, A] with Primitive.Common

//     final case class Modify[A, B](schema: Primitive.Read[A], f: A => B) extends Primitive.Read[B]:
//       export schema.tpe

//     final case class Optional[A](schema: Primitive.Read[A]) extends Primitive.Read[Option[A]]:
//       export schema.tpe

//     final case class Root[A](tpe: Type[A]) extends Primitive.Read[A]

//     def apply[A](tpe: Type[A]): Primitive.Read[A] = Root(tpe)

//   sealed trait Write[A] extends Primitive.Write.Operation[Primitive.Write, A]:
//     override def contramap[B](f: B => A): Primitive.Write[B] = Primitive.Write.Modify(this, f)
//     override def optional: Primitive.Write[Option[A]] = Primitive.Write.Optional(this)

//   object Write:
//     trait Operation[+S[_], A] extends Schema.Operation.Write[S, S, A] with Primitive.Common

//     final case class Modify[A, B](schema: Primitive.Write[A], f: B => A) extends Primitive.Write[B]:
//       export schema.tpe

//     final case class Optional[A](schema: Primitive.Write[A]) extends Primitive.Write[Option[A]]:
//       export schema.tpe

//     final case class Root[A](tpe: Type[A]) extends Primitive.Write[A]

//     def apply[A](tpe: Type[A]): Primitive.Write[A] = Root(tpe)

//   def apply[A](tpe: Type[A]): Primitive[A] =
//     val read = Read(tpe)
//     val write = Write(tpe)

//     new Primitive[A]:
//       override def asRead: Primitive.Read[A] = read
//       override def asWrite: Primitive.Write[A] = write
//       override def tpe: Type[?] = read.tpe

// object Playground:
//   @main
//   def run = {
//     val a: Primitive[String] = Primitive.apply(Type.String)
//     val b = a.optional
//     b.asRead match
//       case Primitive.Read.Optional(_) => println("optional fuck yeah")
//       case x                          => println(x)
//     // val b: Primitive.Required.Read[String] = a
//     // val c: Primitive.Required.Write[String] = a
//     // val d: Primitive.Read[String] = a
//     // val e: Primitive.Write[String] = a
//     // val f: Primitive.Read[String] = b
//     // val g: Primitive.Write[String] = c
//     // val h: Primitive[String] = a
//   }
