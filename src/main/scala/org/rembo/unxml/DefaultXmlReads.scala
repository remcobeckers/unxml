package org.rembo.unxml

import collection.generic
import xml.NodeSeq
import scala.language.higherKinds

trait DefaultXmlReads {

  implicit val readString: XmlRead[String] = XmlRead(n ⇒ n.text)
  implicit val readInt: XmlRead[Int] = XmlRead(n ⇒ n.text.toInt)
  implicit val readDouble: XmlRead[Double] = XmlRead(n ⇒ n.text.toDouble)
  implicit val readBoolean: XmlRead[Boolean] = XmlRead(n ⇒ n.text.toBoolean)
  implicit def traversableReads[F[_], A](implicit bf: generic.CanBuildFrom[F[_], A, F[A]], ra: XmlRead[A]) = XmlRead[F[A]] { nodeSeq ⇒
    nodeSeq.map(ra.read(_)).foldLeft(bf()) { (acc, v) ⇒ acc += v }.result()
  }
  implicit val readXmlPath = XmlRead[XmlPath](nodeSeq ⇒ new XmlPath { def apply(drop: NodeSeq) = nodeSeq })
}
