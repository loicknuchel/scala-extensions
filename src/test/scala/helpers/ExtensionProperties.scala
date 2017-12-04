package helpers

import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Properties
import helpers.Extensions._

import scala.util.Random

object ExtensionProperties extends Properties("Extension") {
  property("SeqExtension.duplicates should return nothing if input is distinct") = forAll { (l: List[Int]) =>
    val d = l.distinct
    d.duplicates == Seq()
    Random.shuffle(d ++ d).duplicates == d
  }
}
