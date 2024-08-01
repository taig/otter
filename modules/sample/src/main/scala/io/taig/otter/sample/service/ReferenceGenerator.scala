// package io.taig.otter.sample.service

// import cats.effect.IO
// import com.aventrix.jnanoid.jnanoid.NanoIdUtils
// import org.typelevel.ci.CIString

// final class ReferenceGenerator(alphabet: Array[Char]):
//   def generate(size: Int): IO[CIString] =
//     IO(CIString(NanoIdUtils.randomNanoId(NanoIdUtils.DEFAULT_NUMBER_GENERATOR, alphabet, size)))

// object ReferenceGenerator:
//   def apply(): ReferenceGenerator = new ReferenceGenerator("123456789abcdefghijkmnopqrstuvwxyz".toCharArray)
