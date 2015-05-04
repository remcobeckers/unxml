package org.rembo.unxml
package streaming

import scala.language.higherKinds
import scala.annotation.implicitNotFound
import scala.collection.generic

import akka.stream._
import akka.stream.scaladsl._

import org.rembo.unxml.streaming.transformers.XmlTransformer.EndDocument
import org.rembo.unxml.streaming.transformers.{ XmlPathFilter, XmlStringReader, XmlTransformer }

object XmlStreamingTypes extends XmlBaseTypes with DefaultXmlReads {
  import XmlTransformer.XmlEvent

  type XmlFlow[T] = Flow[XmlEvent, XmlResult[T], Unit]

  @implicitNotFound(msg = "An implicit XmlReads for ${T} is required.")
  case class XmlReads[T](flow: XmlFlow[T]) {
    def reads(source: Flow[XmlEvent, XmlEvent, Unit]): Flow[XmlEvent, XmlResult[T], Unit] = source.via(flow)
    def reads(source: Source[XmlEvent, Unit]): Source[XmlResult[T], Unit] = source.via(flow)

    def map[R](f: T ⇒ R): XmlReads[R] = XmlReads(flow.map(_.map(v ⇒ f(v))))
    def flatMap[R](f: T ⇒ XmlResult[R]): XmlReads[R] = XmlReads(flow.map(_.flatMap(v ⇒ f(v))))

    def mapResult[R](f: XmlResult[T] ⇒ XmlResult[R]): XmlReads[R] = XmlReads(flow.map(v ⇒ f(v)))
  }

  object XmlReads {
    def apply[T](f: String ⇒ XmlResult[T]): XmlReads[T] =
      XmlReads(Flow[XmlEvent].transform(() ⇒ new XmlStringReader)).flatMap(f)

    private[streaming] def combine[L, R](left: XmlReads[L], right: XmlReads[R]): XmlFlow[(L, R)] =
      Flow() { implicit b ⇒
        import FlowGraph.Implicits._

        val unzip = b.add(Unzip[XmlEvent, XmlEvent])
        val zip = b.add(Zip[XmlResult[L], XmlResult[R]])

        val in = b.add(Flow[XmlEvent].map(v ⇒ (v, v)))

        in ~> unzip.in
        unzip.out0 ~> left.flow ~> zip.in0
        unzip.out1 ~> right.flow ~> zip.in1

        val out = b.add(Flow[(XmlResult[L], XmlResult[R])].map {
          case (lr, rr) ⇒
            lr.flatMap { l1 ⇒
              rr.map { r ⇒
                (l1, r)
              }
            }
        })

        zip.out ~> out

        (in.inlet, out.outlet)
      }
  }

  implicit class XmlPathOps(path: XmlPath) {

    def apply(source: Source[XmlEvent, Unit]): Source[XmlEvent, Unit] = {
      source.transform(() ⇒ new XmlPathFilter(path))
    }

    def read[T](implicit reads: XmlReads[T]): XmlReads[T] =
      XmlReads {
        reads.reads(Flow[XmlEvent].transform(() ⇒ new XmlPathFilter(path))) map (_.addErrorPathPrefix(path))
      }

    def readOptional[T](implicit reads: XmlReads[T]): XmlReads[Option[T]] =
      path.read[T].mapResult {
        case XmlSuccess(r)                                           ⇒ XmlSuccess(Some(r))
        case XmlError(_, notFoundPath, true) if notFoundPath == path ⇒ XmlSuccess(None)
        case error: XmlError                                         ⇒ error.addErrorPathPrefix(path)
      }
  }

  implicit class ElemPathOps(path: ElemPath) {
    def readAll[F[_], T](child: String, maxSize: Int)(implicit reads: XmlReads[T], bf: generic.CanBuildFrom[F[_], T, F[T]]): XmlReads[F[T]] =
      readAllWith(child, maxSize)(reads)

    def readAllWith[F[_], T](child: String, maxSize: Int)(reads: XmlReads[T])(implicit bf: generic.CanBuildFrom[F[_], T, F[T]]): XmlReads[F[T]] = {
      val childReads = (XmlPath \ child).read(reads).mapResult(_.addErrorPathPrefix(path \ child))
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
