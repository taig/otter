// package io.taig.otter.http.codec

// import cats.data.Validated
// import cats.syntax.all.*
// import io.taig.otter.Violation
// import io.taig.otter.Violations

// object SegmentParser:
//   def apply[A](segment: Segment[A], value: String): Validated[Violations, A] = segment match
//     case Segment.Static(name, _) =>
//       Validated.cond(
//         test = value === name,
//         (),
//         Violations.rootNec(Violation.equal(reference = name, actual = value))
//       )
//     case Segment.Parameter(name, codec, metadata) =>
//       val explode = metadata.get(HttpKeys.explode).getOrElse(false) // TODO proper default
//       val style = metadata
//         .get(HttpKeys.style)
//         .collect { case style: Header.Style => style }
//         .getOrElse(Header.Style.Simple)

//       HttpParameterParser(explode, style)(name, codec = codec.value, value)
//     case Segment.Modify(self, f, _) => apply(segment = self, value).map(f)
