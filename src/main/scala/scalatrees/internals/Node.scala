package com.scalatrees.internals

import java.io._

object MalformedNodeException extends Exception

@SerialVersionUID(100L)
abstract class Node[InputClass, LabelType: Fractional] extends Serializable {
  def predict(input: InputClass): LabelType
}

case class TerminalNode[InputClass, LabelType: Fractional](value: LabelType) extends Node[InputClass, LabelType] {
  def predict(input: InputClass): LabelType = value
}

case class TraversalNode[InputClass, LabelType: Fractional](
  accessLambda: InputClass => Double,
  threshold: Double,
  left: Node[InputClass, LabelType],
  right: Node[InputClass, LabelType]
) extends Node[InputClass, LabelType] {
  def predict(input: InputClass): LabelType = {
    val value = accessLambda(input)
    (value > threshold, left, right) match {
      case (true, next, _) => next.predict(input)
      case (false, _, next) => next.predict(input)
      case _ => throw MalformedNodeException
    }
  }
}

case class Tree[InputClass, LabelType: Fractional](start: Node[InputClass, LabelType]) extends Node[InputClass, LabelType] {
  def predict(input: InputClass): LabelType = start.predict(input)

  def write(path: String): Unit = Tree.write(path, this)
}

object Tree {
  def load[InputClass, LabelType: Fractional](path: String): Tree[InputClass, LabelType] = {
    val ois = new ObjectInputStream(new FileInputStream("/tmp/scalatree"))
    val tree = ois.readObject.asInstanceOf[Tree[InputClass, LabelType]]
    ois.close
    tree
  }

  def write[InputClass, LabelType: Fractional](path: String, tree: Tree[InputClass, LabelType]): Unit = {
    val o = new ObjectOutputStream(new FileOutputStream("/tmp/scalatree"))
    o.writeObject(tree)
    o.close
  }
}
