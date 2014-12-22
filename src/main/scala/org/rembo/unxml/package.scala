package org.rembo.unxml

import annotation._
import scala.util.control.NonFatal
import scala.xml.NodeSeq

object XmlReads {
  def apply[T](f: NodeSeq ⇒ XmlResult[T]) = new XmlReads[T]() {
    def reads(nodeSeq: NodeSeq) = f(nodeSeq)
  }
}

@implicitNotFound(msg = "An implicit XmlRead for ${T} is required.")
trait XmlReads[T] {
  def reads(nodeSeq: NodeSeq): XmlResult[T]

  def map[R](f: T ⇒ R): XmlReads[R] = XmlReads { node ⇒ reads(node).map(v ⇒ f(v)) }
  def flatMap[R](f: T ⇒ XmlReads[R]): XmlReads[R] = XmlReads { node ⇒ reads(node).flatMap(t ⇒ f(t).reads(node)) }
}

object XmlPath {
  val empty = XmlPath(List.empty)

  def \(elem: String): XmlPath = XmlPath(List(elem))
  def apply(): XmlPath = XmlPath(List.empty)
}

case class XmlPath(path: List[String]) {
  def \(elem: String): XmlPath = XmlPath(path :+ elem)

  def apply(node: NodeSeq): NodeSeq = path.foldLeft(node) { (nodeSeq, elem) ⇒ nodeSeq \ elem }

  def init = XmlPath(path.init)

  def startsWith(other: XmlPath) = path.startsWith(other.path)

  def read[T](implicit r: XmlReads[T]): XmlReads[T] = XmlReads { node ⇒
    val n = apply(node)
    if (n.isEmpty) XmlError("Node not found", this)
    r.reads(n)
  }

  def readOptional[T](implicit r: XmlReads[T]): XmlReads[Option[T]] = XmlReads { node ⇒
    val n = apply(node)
    if (n.isEmpty) XmlSuccess(None)
    else r.reads(n).map(Some(_))
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
