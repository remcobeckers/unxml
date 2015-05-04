package org.rembo.unxml.performance

import org.scalatest._

import scala.concurrent._
import scala.concurrent.duration._

import org.rembo.unxml.blocking.UnXml
import org.rembo.unxml.blocking.XmlBlockingTypes._

import scala.util.Random

class BlockingPerformanceTest extends WordSpecLike with Matchers with OptionValues with Inside with Inspectors {

  val N = 100000

  import scala.concurrent.ExecutionContext.Implicits.global

  def logExecutionTime[T](descriptor: String*)(f: ⇒ Future[T])(implicit executor: ExecutionContext): Future[T] = {
    val start = System.nanoTime
    f.andThen {
      case _ ⇒
        val durationInMillis = (System.nanoTime - start) / (1000 * 1000)
        println(s"${descriptor.mkString(".")}=${durationInMillis}ms")
    }
  }

  def genPersons(persons: Int): Iterable[Char] = {
    val start = "<root><persons>"
    val end = "</persons></root>"

    val hobbies = Set("Running", "Darting", "Drinking", "Bowling", "Cycling", "Hanging out")
    def randomHobbies = hobbies.filter(_ ⇒ Random.nextBoolean()).map { hobby ⇒
      s"<hobby>$hobby</hobby>"
    }.mkString("")

    (Iterator.apply[String](start) ++ Iterator.tabulate[String](persons) { n ⇒
      s"""<person>
         |     <name>Person $n</name>
         |     <age>${n / 100}</age>
         |     <hobbies>
         |       $randomHobbies
         |     </hobbies>
         |   </person>
      """.stripMargin
    } ++ Iterator.apply[String](end)).flatMap(_.map(c => c)).toIterable
  }

  "The streaming XML reader" should {
    "be fast with a lot of data" in {
      case class Person(name: String, age: Int, hobbies: Set[Hobby])
      case class Hobby(description: String)

      implicit val hobbyReader = XmlPath.empty.read[String].as(Hobby)
      implicit val personReader = (
          (XmlPath \ "name").read[String] and
            (XmlPath \ "age").read[Int] and
                      (XmlPath \ "hobbies").readAll[Set, String]("hobby", 10).map(_.map(Hobby))
        ).as(Person)

      val parseResult = Future {
        UnXml.traversableFromXml[Person](XmlPath \ "root" \ "persons" \ "person", scala.io.Source.fromIterable(genPersons(N))).foldLeft(0) {
          case (acc, XmlSuccess(r)) ⇒ acc + 1
          case (acc, e)             ⇒ acc
        }
      }

      val result = Await.result(logExecutionTime("parsing.time")(parseResult), 5.minutes)
      result shouldBe N
    }
  }
}
