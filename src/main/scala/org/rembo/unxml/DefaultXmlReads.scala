package org.rembo.unxml

import collection.generic
import xml.NodeSeq
import scala.language.higherKinds

trait DefaultXmlReads {

  implicit val readString: XmlRead[String] = XmlRead(n ⇒ XmlResult(n.text))
  implicit val readInt: XmlRead[Int] = XmlRead(n ⇒ XmlResult(n.text.toInt))
  implicit val readDouble: XmlRead[Double] = XmlRead(n ⇒ XmlResult(n.text.toDouble))
  implicit val readBoolean: XmlRead[Boolean] = XmlRead(n ⇒ XmlResult(n.text.toBoolean))

  //TODO: Accumulate all errors instead of stopping after the first
  implicit def traversableReads[F[_], A](implicit bf: generic.CanBuildFrom[F[_], A, F[A]], ra: XmlRead[A]) = XmlRead[F[A]] { nodeSeq ⇒
    nodeSeq.zipWithIndex.foldLeft(XmlSuccess(Vector.empty): XmlResult[Vector[A]]) {
      case (r @ XmlError(_, _), _) ⇒ r
      case (acc, (elt, index)) ⇒ (acc, ra.read(elt)) match {
        case (XmlSuccess(results), XmlSuccess(v)) ⇒ XmlSuccess(results :+ v)
        case (_, XmlError(error, path))           ⇒ XmlError(error, path \ s"[$index]")
        case (e @ XmlError(_, _), XmlSuccess(v))  ⇒ e
      }
    }.map { results ⇒
      val builder = bf()
      results.foreach(v ⇒ builder += v)
      builder.result()
    }
  }

  implicit val readXmlPath = XmlRead[XmlPath](nodeSeq ⇒ XmlResult((new XmlPath(List.empty) { override def apply(drop: NodeSeq) = nodeSeq })))
}
