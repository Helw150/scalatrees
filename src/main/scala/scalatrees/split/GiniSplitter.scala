package com.scalatrees.split

class GiniSplitter[LabelType: Fractional](
  maxDepth: Int = scala.Int.MaxValue,
  minGroupSize: Int = 2,
  splitTestsOpt: Option[Int] = None
) extends BaseSplitter[LabelType](maxDepth, minGroupSize, splitTestsOpt) {
  def giniImpurity(
    splitData: Seq[Seq[LabeledInput]]
  ): Double = {
    val sampleSize = splitData.flatten.length
    splitData.map { split =>
      val splitSize = split.length.toDouble
      val groupGiniIndex = 1 - split
        .groupBy(_.label)
        .values
        .map { partition =>
          val classProb = (partition.length / splitSize)
          classProb * classProb
        }
        .sum
      val groupWeight = splitSize / sampleSize.toDouble
      groupGiniIndex * groupWeight
    }.sum
  }

  override def splitEval(
    split1: Seq[LabeledInput],
    split2: Seq[LabeledInput]
  ): Double = giniImpurity(Seq(split1, split2))
}
