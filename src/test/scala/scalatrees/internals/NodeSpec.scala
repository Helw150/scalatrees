package com.scalatrees.internals

import org.scalatest._
import com.scalatrees.internals.ImplicitFractionals._

class NodeSpec extends FlatSpec with Matchers {
  val leftNode = TerminalNode[Seq[Double], Boolean](true)
  val rightNode = TerminalNode[Seq[Double], Boolean](false)
  val headNode = TraversalNode[Seq[Double], Boolean](_(0), 1, leftNode, rightNode)
  val tree = Tree[Seq[Double], Boolean](headNode)

  ".write" should "serialize without errors" in {
    tree.write("/tmp/scalatree")
  }

  ".load" should "read and predict correctly" in {
    val newHead = Tree.load[Seq[Double], Boolean]("/tmp/scalatree")

    newHead.predict(Seq(5.0)) should be(headNode.predict(Seq(5.0)))
  }

  ".predict" should "chain predictions" in {
    tree.predict(Seq(5.0)) should be(true)
    tree.predict(Seq(0.0)) should be(false)
  }
}
