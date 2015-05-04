package org.rembo.unxml.performance

import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest._

import scala.concurrent._
import scala.concurrent.duration._

import org.rembo.unxml.streaming.UnXml
import org.rembo.unxml.streaming.XmlStreamingTypes._

import scala.util.Random

//TODO: Error handling seems incorrect in case of empty set of hobbies for readAll (and errors make stream crash)
class StreamingPerformanceTest extends TestKit(ActorSystem()) with WordSpecLike with Matchers with OptionValues with Inside with Inspectors {

  val N = 10000

  implicit val mat = ActorFlowMaterializer()

  import system.dispatcher

  def logExecutionTime[T](descriptor: String*)(f: ⇒ Future[T])(implicit executor: ExecutionContext): Future[T] = {
    val start = System.nanoTime
    f.andThen {
      case _ ⇒
        val durationInMillis = (System.nanoTime - start) / (1000 * 1000)
        println(s"${descriptor.mkString(".")}=${durationInMillis}ms")
    }
  }

  def genPersons(persons: Int): Iterator[ByteString] = {
    val start = "<root><persons>"
    val end = "</persons></root>"

    val hobbies = Set("Darting", "Drinking", "Bowling", "Cycling", "Hanging out")
    def randomHobbies = (hobbies.filter(_ ⇒ Random.nextBoolean()) + "Running").map { hobby ⇒
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
    } ++ Iterator.apply[String](end)).flatMap(_.grouped(30)).map(ByteString.apply(_))
  }

  "The streaming XML reader" should {
    "be fast with a lot of data" in {
      case class Person(name: String, age: Int, hobbies: Set[Hobby])
      case class Hobby(description: String)

      implicit val hobbyReader = XmlPath.empty.read[String].as(Hobby)
      val personReader = (
        (XmlPath \ "root" \ "persons" \ "person").read(
          (XmlPath \ "name").read[String] and
            (XmlPath \ "age").read[Int] and
            (XmlPath \ "hobbies").readAll[Set, String]("hobby", 10).map(_.map(Hobby))
        ).as(Person)
      )

      //TODO: readAll seems to have some non-termination issue and does not parse empty lists correctly...

      val byteSource = Source(() ⇒ genPersons(N))
      val resultSource = UnXml.fromXml(byteSource)(personReader)

      val foldSink = Sink.fold[Int, XmlResult[Person]](0) {
        case (acc, XmlSuccess(r)) ⇒ acc + 1
        case (acc, e)             ⇒ acc
      }

      val result = Await.result(logExecutionTime("parsing.time")(resultSource.runWith(foldSink)), 5.minutes)
      result shouldBe N
    }
  }
}
