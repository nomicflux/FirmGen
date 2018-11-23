package markov

import scala.annotation.tailrec
import scala.util.Random

object MarkovChain {
  type MarkovMatrix = Map[NGram, Map[Option[Char], Double]]
  case class ProbWithCumulation(probHere: Double, probCumulative: Double)

  case class WChainSpec(n: Int, skip: Int, weight: Double)

  private def takeNGramWithFollowing(n: Int, skip: Int)(string: String): (NGram, Option[Char]) = {
    val (ngram, next) = string.splitAt(n*skip)
    (NGram.fromString(n, skip, ngram), next.headOption)
  }

  private def cleanBlanks(string: String): String = string.replaceAll(" +", " ")

  private def takeAllNGramsWithFollowing(n: Int, skip: Int)(string: String): Vector[(NGram, Option[Char])] =
    (" " * ((n - 1) * skip + 1) + string).tails.take(string.length + 1).map(ngram ⇒ takeNGramWithFollowing(n, skip)(ngram)).map{ case (ngram, next) => (ngram.map(cleanBlanks), next) }.toVector

  private def toMarkovMatrix(ngrams: Vector[(NGram, Option[Char])]): MarkovMatrix =
    ngrams.groupBy(_._1).mapValues(_.groupBy(_._2).mapValues(_.size.toDouble))

  def normalizeRow(row: Map[Option[Char], Double]): Map[Option[Char], Double] = {
    val cts = row.values.sum
    if (cts == 0.0) row
    else row.mapValues( _ / cts )
  }

  def cumulateRow(row: Map[Option[Char], Double]): Vector[(Option[Char], ProbWithCumulation)] = {
    row.toVector.foldLeft((Vector.empty[(Option[Char], ProbWithCumulation)], 0.0d)) { case ((acc, prob), (k, v)) =>
      val newProb = prob + v
      (acc :+ (k, ProbWithCumulation(v, newProb)), newProb)
    }._1
  }

  private def normalizeMatrix(matrix: MarkovMatrix): MarkovMatrix = matrix.mapValues(row => normalizeRow(row))

  def empty(chainSize: Int, skip: Int): MarkovChain = SingleMarkovChain(chainSize, skip, Map.empty)

  def empty(weights: Seq[WChainSpec]): MarkovChain = MarkovChains(weights.map(w => WeightedChain(w.weight, MarkovChain.empty(w.n, w.skip))))

  def apply(chainSize: Int, skip: Int, docs: Vector[String]): MarkovChain = {
    val matrix = toMarkovMatrix(docs.flatMap(takeAllNGramsWithFollowing(chainSize, skip)))
    SingleMarkovChain(chainSize, skip, normalizeMatrix(matrix))
  }
}

sealed trait MarkovChain {
  def generateFromDocs(docs: Vector[String]): MarkovChain
  def nextChar(string: String): Option[(Option[Char], Double)]
  def generateWord: String = {
    @tailrec
    def generateWordHelper(currWord: String): String = nextChar(currWord) match {
      case None => currWord
      case Some((None, _)) ⇒ currWord
      case Some((Some(char), _)) ⇒ generateWordHelper(currWord + char)
    }

    generateWordHelper(" ").trim
  }
}

case class SingleMarkovChain(chainSize: Int, skip: Int, probs: MarkovChain.MarkovMatrix) extends MarkovChain {
  private def splitOption[A](vec: Vector[A]): Option[(A, Vector[A])] =
    if (vec.isEmpty) None else Some((vec.head, vec.tail))

  def generateFromDocs(docs: Vector[String]): MarkovChain = MarkovChain(chainSize, skip, docs)

  def getRow(string: String): Map[Option[Char], Double] = probs.getOrElse(NGram.fromString(chainSize, skip, string), Map.empty)

  def nextChar(string: String): Option[(Option[Char], Double)] = {
    val prob = Random.nextDouble()
    val row = MarkovChain.cumulateRow(getRow(string.toLowerCase.takeRight(chainSize)))
    row.dropWhile(_._2.probCumulative < prob).headOption.map(kv ⇒ (kv._1, kv._2.probHere))
  }
}

case class WeightedChain(weight: Double, chain: MarkovChain) {
  def map(f: MarkovChain ⇒ MarkovChain): WeightedChain = this.copy(chain = f(chain))
}

case class MarkovChains(wchains: Seq[WeightedChain]) extends MarkovChain {
  def generateFromDocs(docs: Vector[String]): MarkovChain = {
    MarkovChains(wchains.map(wchain ⇒ wchain.map(_.generateFromDocs(docs))))
  }

  def nextChar(string: String): Option[(Option[Char], Double)] = {
    val charsWithProbs: Seq[(Option[Char], Double)] =
      wchains.flatMap( wchain => {
                    wchain.chain.nextChar(string).map { case (char, prob) => (char, prob * wchain.weight) }
                  }
      )
    val normalizedRow = MarkovChain.normalizeRow(charsWithProbs.toMap)
    val cumulatedRow: Vector[(Option[Char], MarkovChain.ProbWithCumulation)] = MarkovChain.cumulateRow(normalizedRow)
    val rand = Random.nextDouble()
    cumulatedRow.dropWhile( _._2.probCumulative < rand ).headOption.map(kv ⇒ (kv._1, kv._2.probHere))
  }
}
