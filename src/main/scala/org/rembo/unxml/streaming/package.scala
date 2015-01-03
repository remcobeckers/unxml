package org.rembo.unxml
package streaming

import scala.language.higherKinds
import scala.annotation.implicitNotFound
import scala.collection.generic

import akka.stream.FlattenStrategy
import akka.stream.scaladsl._

import org.rembo.unxml.streaming.transformers.XmlTransformer.EndDocument
import org.rembo.unxml.streaming.transformers.{ XmlPathFilter, XmlStringReader, XmlTransformer }

object XmlStreamingTypes extends XmlBaseTypes {
  import XmlTransformer.XmlEvent

  type XmlFlow[T] = Flow[XmlEvent, XmlResult[T]]

  @implicitNotFound(msg = "An implicit XmlReads for ${T} is required.")
  case class XmlReads[T](flow: XmlFlow[T]) {
    def reads(source: Flow[XmlEvent, XmlEvent]): Flow[XmlEvent, XmlResult[T]] = source.via(flow)
    def reads(source: Source[XmlEvent]): Source[XmlResult[T]] = source.via(flow)

    def map[R](f: T ⇒ R): XmlReads[R] = XmlReads(flow.map(_.map(v ⇒ f(v))))
    def flatMap[R](f: T ⇒ XmlResult[R]): XmlReads[R] = XmlReads(flow.map(_.flatMap(v ⇒ f(v))))

    def mapResult[R](f: XmlResult[T] ⇒ XmlResult[R]): XmlReads[R] = XmlReads(flow.map(v ⇒ f(v)))
  }

  object XmlReads {
    def apply[T](f: String ⇒ XmlResult[T]): XmlReads[T] =
      XmlReads(Flow[XmlEvent].transform(() ⇒ new XmlStringReader)).flatMap(f)

    private[streaming] def combine[L, R](left: XmlReads[L], right: XmlReads[R]): XmlFlow[(L, R)] =
      Flow() { implicit b ⇒
        import FlowGraphImplicits._
        val unzip = Unzip[XmlEvent, XmlEvent]
        val zip = Zip[XmlResult[L], XmlResult[R]]

        val source = UndefinedSource[XmlEvent]
        val sink = UndefinedSink[XmlResult[(L, R)]]

        source ~> Flow[XmlEvent].map(v ⇒ (v, v)) ~> unzip.in
        unzip.left ~> left.flow ~> zip.left
        unzip.right ~> right.flow ~> zip.right

        zip.out ~> Flow[(XmlResult[L], XmlResult[R])].map {
          case (lr, rr) ⇒
            lr.flatMap { l1 ⇒
              rr.map { r ⇒
                (l1, r)
              }
            }
        } ~> sink

        (source, sink)
      }
  }

  implicit class XmlPathOps(path: XmlPath) {

    def apply(source: Source[XmlEvent]): Source[XmlEvent] = {
      source.transform(() ⇒ new XmlPathFilter(path))
    }

    def read[T](implicit reads: XmlReads[T]): XmlReads[T] =
      XmlReads(reads.reads(Flow[XmlEvent].transform(() ⇒ new XmlPathFilter(path)))).mapResult(_.addErrorPathPrefix(path))

  }

  implicit class ElemPathOps(path: ElemPath) {
    def readAll[F[_], T](child: String, maxSize: Int)(implicit reads: XmlReads[T], bf: generic.CanBuildFrom[F[_], T, F[T]]): XmlReads[F[T]] = {
      val childReads = (XmlPath \ child).read[T].mapResult(_.addErrorPathPrefix(path \ child))
      XmlReads[F[T]] {
        Flow[XmlEvent].transform(() ⇒ new XmlPathFilter(path)).splitWhen {
          case EndDocument ⇒ true
          case _           ⇒ false
        }.map { childrenStream ⇒
          childrenStream.via(childReads.flow).grouped(maxSize).take(1).map {
            XmlResult.sequence(_).map { values ⇒
              val builder = bf()
              values.foreach(v ⇒ builder += v)
              builder.result()
            }
          }
        }.flatten(FlattenStrategy.concat)
      }
    }
  }
}
