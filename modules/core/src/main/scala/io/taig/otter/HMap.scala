package io.taig.otter

import scala.collection.immutable.HashMap

opaque type HMap[+T <: Singleton] = HashMap[String, Any]

object HMap:
  opaque type Key[A] = String

  object Key:
    def apply[A](value: String): HMap.Key[A] = value

  extension [T <: Singleton](hmap: HMap[T])
    def apply[A <: T & Key[B] & Singleton, B](key: A): B = hmap.apply(key).asInstanceOf[B]

    def get[A <: Key[B] & Singleton, B](key: A): Option[B] = hmap.get(key).asInstanceOf[Option[B]]

    def update[A <: T & Key[B] & Singleton, B](key: A, f: B => B): HMap[T] = put(key, f(apply[A, B](key)))

    def put[A <: Key[B] & Singleton, B](key: A, value: B): HMap[key.type | T] = hmap.updated(key, value)

  val Empty: HMap[Nothing] = HashMap.empty

// object Playground:
//   @main
//   def apply =
//     val format: HMap.Key[String] = HMap.Key("format")
//     val name: HMap.Key[String] = HMap.Key("name")
//     val description: HMap.Key[String] = HMap.Key("description")

//     val x: HMap[Nothing] = HMap.Empty

//     // x.apply(format)
//     val y = x.put(format, "lol").put(description, "foo")
//     println(y.apply(format))
//     // println(y.apply(name))
//     println(y.apply(description))
//     // println(y.apply(name))
