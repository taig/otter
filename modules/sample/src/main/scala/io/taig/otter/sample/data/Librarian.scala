// package io.taig.otter.sample.data

// import cats.Eq
// import cats.syntax.all.*
// import io.taig.otter.validation.{validations, Validation}
// import org.typelevel.ci.*

// import java.util.regex.Pattern

// final case class Librarian(
//     reference: Librarian.Reference,
//     email: Librarian.Email,
//     password: Librarian.Password,
//     session: Option[Session]
// ):
//   def toSummary: Librarian.Summary = Librarian.Summary(reference, email)

// object Librarian:
//   opaque type Reference = CIString
//   object Reference:
//     val Length = 8
//     extension (self: Librarian.Reference) def toCIString: CIString = self
//     def unsafeFromCIString(value: CIString): Librarian.Reference = value
//     val validation: Validation[CIString, Librarian.Reference] = validations.length[CIString](Length, _.length).tap

//   opaque type Email = CIString
//   object Email:
//     extension (self: Librarian.Email) def toCIString: CIString = self
//     def unsafeFromCIString(value: CIString): Librarian.Email = value
//     val validation: Validation[CIString, Librarian.Email] =
//       val pattern = Pattern.compile(".+@.+")
//       Validation.ask[CIString].map(_.toString).andThen(validations.matches(pattern)).tap

//     given (using eq: Eq[CIString]): Eq[Librarian.Email] = eq

//   opaque type Password = String
//   object Password:
//     def unsafeFromString(value: String): Librarian.Password = value
//     val validation: Validation[String, Librarian.Password] = validations.minLength(6).tap

//   final case class Login(email: CIString, password: String)

//   final case class Create(email: Librarian.Email, password: Librarian.Password):
//     def toLogin: Librarian.Login = Login(email.toCIString, password.toString)

//   object Create:
//     val Default: Librarian.Create = Librarian.Create(
//       Librarian.Email.unsafeFromCIString(ci"me@otter.org"),
//       Librarian.Password.unsafeFromString("password")
//     )

//   final case class Summary(reference: Librarian.Reference, email: Librarian.Email)
