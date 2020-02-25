package com.scalatrees.algorithms

import org.scalatest._
import com.scalatrees.internals.ImplicitFractionals._

class RandomForestSpec extends FlatSpec with Matchers {
  val testMatrix = Seq(
    Seq(1.0),
    Seq(2.0),
    Seq(3.0)
  )
  val testLabels = Seq(0.0, 0.0, 1.0)
  val forest = RandomForest.train[Double](testMatrix, testLabels, 10)

  ".buildTree" should "build a tree which partitions cleanly sliced data when expensive" in {
    forest.predict(Seq(5)) should be(1.0)
    forest.predict(Seq(-10)) should be(0.0)
  }

  it should "build a tree which partitions evenly sliced data when cheap" in {
    forest.predict(Seq(5)) should be(1.0)
    forest.predict(Seq(-10)) should be(0.0)
  }
}
