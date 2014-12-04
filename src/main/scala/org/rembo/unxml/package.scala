package org.rembo.unxml

import annotation._
import scala.util.control.NonFatal
import scala.xml.NodeSeq

object XmlRead {
  def apply[T](f: NodeSeq ⇒ XmlResult[T]) = new XmlRead[T]() {
    def read(nodeSeq: NodeSeq) = f(nodeSeq)
  }
}

@implicitNotFound(msg = "An implicit XmlRead for ${T} is required.")
trait XmlRead[T] {
  def read(nodeSeq: NodeSeq): XmlResult[T]

  def map[R](f: T ⇒ R): XmlRead[R] = XmlRead { node ⇒ read(node).map(v ⇒ f(v)) }
  def flatMap[R](f: T ⇒ XmlRead[R]): XmlRead[R] = XmlRead { node ⇒ read(node).flatMap(t ⇒ f(t).read(node)) }
}

object XmlPath {
  def \(elem: String): XmlPath = XmlPath(List(elem))
  def apply(): XmlPath = XmlPath(List.empty)
}

case class XmlPath(path: List[String]) {
  def \(elem: String): XmlPath = XmlPath(path :+ elem)

  def apply(node: NodeSeq): NodeSeq = path.foldLeft(node) { (nodeSeq, elem) ⇒ nodeSeq \ elem }

  def read[T](implicit r: XmlRead[T]): XmlRead[T] = XmlRead { node ⇒
    val n = apply(node)
    if (n.isEmpty) XmlError("Node not found", this)
    r.read(n)
  }

  def readOptional[T](implicit r: XmlRead[T]): XmlRead[Option[T]] = XmlRead { node ⇒
    val n = apply(node)
    if (n.isEmpty) XmlSuccess(None)
    else r.read(n).map(Some(_))
  }

  def ++(other: XmlPath) = {
    XmlPath(path ::: other.path)
  }

  override def toString: String = path.mkString("\\")
}

object XmlResult {
  def apply[T](valueProducer: ⇒ T) = {
    try {
      XmlSuccess(valueProducer)
    } catch {
      case NonFatal(t) ⇒ XmlError(s"Error while parsing ${t.getMessage}")
    }
  }
}

sealed trait XmlResult[+T] {
  def map[R](f: T ⇒ R): XmlResult[R] = this match {
    case XmlSuccess(v) ⇒ XmlSuccess(f(v))
    case e: XmlError   ⇒ e
  }

  def flatMap[R](f: T ⇒ XmlResult[R]): XmlResult[R] = this match {
    case XmlSuccess(v) ⇒ f(v)
    case e: XmlError   ⇒ e
  }

  def addErrorPathPrefix(path: XmlPath) = this match {
    case XmlError(e, errorPath) ⇒ XmlError(e, errorPath = path ++ errorPath)
    case s: XmlSuccess[T]       ⇒ s
  }

  def orElse[TT >: T](f: ⇒ XmlResult[TT]) = this match {
    case XmlError(_, _)        ⇒ f
    case other @ XmlSuccess(_) ⇒ other
  }

  def getOrElse[TT >: T](otherwise: ⇒ TT) = this match {
    case XmlError(_, _) ⇒ otherwise
    case XmlSuccess(v)  ⇒ v
  }
}

case class XmlSuccess[T](value: T) extends XmlResult[T]
case class XmlError(error: String, errorPath: XmlPath = XmlPath(List.empty)) extends XmlResult[Nothing]
