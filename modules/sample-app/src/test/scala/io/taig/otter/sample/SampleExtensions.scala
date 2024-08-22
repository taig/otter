// package io.taig.otter.sample

// import cats.data.Validated
// import cats.effect.IO
// import io.taig.otter.munit.OtterExtensions
// import io.taig.otter.sample.api.Authentication
// import io.taig.otter.validation.Violations
// import mouse.all.*

// trait SampleExtensions extends OtterExtensions:
//   extension [O](self: IO[Validated[Violations, Either[Authentication.Error, O]]])
//     def toAuthenticated: IO[O] = self.toValid
//       .leftMapIn(new IllegalStateException("Expected Success, got Authentication.Error", _))
//       .rethrow

//     def toUnauthenticated: IO[Authentication.Error] = self.toValid.swapIn
//       .leftMapIn(_ => new IllegalStateException("Expected Authentication.Error, got Success"))
//       .rethrow

//   extension [E <: Matchable, O](self: IO[Validated[Violations, Either[Authentication.Error, Either[E, O]]]])
//     def toSuccess: IO[O] = self.toAuthenticated.leftMapIn {
//       case throwable: Throwable => new IllegalStateException("Expected Success, got Error", throwable)
//       case _                    => new IllegalStateException("Expected Success, got Error")
//     }.rethrow

//     def toError: IO[E] = self.toAuthenticated.swapIn
//       .leftMapIn(_ => new IllegalStateException("Expected Error, got Success"))
//       .rethrow
