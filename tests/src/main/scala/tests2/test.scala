package magnolia.tests.sandbox

import estrapade.{TestApp, test}
import magnolia.{Param, _}
import magnolia.tests.{Destination, Item, OffRoad, Path}

import scala.language.experimental.macros



case class Prefix(names:Seq[String]) {
  def add(name:String): Prefix = this.copy(names :+ name)

  def toStrRep:String = names.mkString(".")
}

object Prefix {
  def empty:Prefix = Prefix(Seq.empty)
}



trait TC[A] {
  def toMapWithPrefix(value: A, prefix: Prefix = Prefix.empty): Map[String, String]
}

object TCMagnolia {
  type Typeclass[T] = TC[T]


  def combine[T](ctx: CaseClass[Typeclass, T]): TC[T] = (obj,prefix) => {
    ctx.parameters
      .flatMap(param => {
        param.typeclass.toMapWithPrefix(param.dereference(obj), prefix.add(param.label))
      }).toMap
  }
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): TC[T] =
    (obj, prefix) => ctx.dispatch(obj) { sub => sub.typeclass.toMapWithPrefix(sub.cast(obj), prefix)}

  implicit def gen[T]: TC[T] = macro Magnolia.gen[T]
}

object TestSubTypeclass extends TestApp {

  def tests(): Unit = {

    def instance[T](f:T => String):TC[T] = (obj,prefix) => Map(prefix.toStrRep -> f(obj))

    implicit val str: TC[String] = instance(identity)
    implicit val int: TC[Int] = instance(_.toString)

    val path = OffRoad(Some(Destination(1)))

    test("construct a TC product instance with coproduit of typeclass on a sealed trait") {

      TCMagnolia.gen[Path[Int]].toMapWithPrefix(value = path)

    }.assert(_ == Map("path.value.value" -> "1"))

    test("derived type classe") {
      //Impl for option

      implicit def opt[T](implicit T: TC[T]): TC[Option[T]] = (obj,prefix) => {
        obj match {
          case None => Map.empty[String,String]
          case Some(t) => T.toMapWithPrefix(t, prefix)
        }
      }

      implicit def destination[T](implicit T:TC[T]): TC[Destination[T]] = (obj,prefix) => {
        T.toMapWithPrefix(obj.value,prefix.add("destination"))
      }

      TCMagnolia.gen[Path[Int]].toMapWithPrefix(path)

    }.assert(_ == Map("path.destination" -> "1"))

    test("construct a TC product instance with coproduct of typeclass") {

      TCMagnolia.gen[Item].toMapWithPrefix(Item("a", 1, 1))

    }.assert(_ == Map("name" -> "a", "quantity" -> "1", "price" -> "1"))

  }
}
