package markov

import scala.annotation.tailrec
import scala.util.Random

object MarkovChain {
  type MarkovMatrix = Map[NGram, Map[Option[Char], Double]]

  case class WChainSpec(n: Int, skip: Int, weight: Double)

  private def takeNGramWithFollowing(n: Int, skip: Int)(string: String): (NGram, Option[Char]) = {
    val (ngram, next) = string.splitAt(n*skip)
    (NGram.fromString(n, skip, ngram), next.headOption)
  }

  private def takeAllNGramsWithFollowing(n: Int, skip: Int)(string: String): Vector[(NGram, Option[Char])] =
    string.tails.take(string.length - n*skip + 1).map(takeNGramWithFollowing(n, skip)(_)).toVector

  private def toMarkovMatrix(ngrams: Vector[(NGram, Option[Char])]): MarkovMatrix =
    ngrams.groupBy(_._1).mapValues(_.groupBy(_._2).mapValues(_.size.toDouble))

  def normalizeRow(row: Map[Option[Char], Double]): Map[Option[Char], Double] =
    if (row.isEmpty) row
    else {
      val cts = row.values.sum
      row.mapValues( _ / cts )
    }

  def cumulateRow(row: Map[Option[Char], Double]): Vector[(Option[Char], Double, Double)] = {
    row.toVector.foldLeft((Vector.empty[(Option[Char], Double, Double)], 0.0d)) { case ((acc, prob), (k, v)) =>
      val newProb = prob + v
      (acc :+ (k, v, newProb), newProb)
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
  def nextChar(string: String): (Option[Char], Double)
}

case class SingleMarkovChain(chainSize: Int, skip: Int, probs: MarkovChain.MarkovMatrix) extends MarkovChain {
  private def splitOption[A](vec: Vector[A]): Option[(A, Vector[A])] =
    if (vec.isEmpty) None else Some((vec.head, vec.tail))

  def generateFromDocs(docs: Vector[String]): MarkovChain = MarkovChain(chainSize, skip, docs)

  def getRow(string: String): Map[Option[Char], Double] = probs.getOrElse(NGram.fromString(chainSize, skip, string), Map.empty)

  def nextChar(string: String): (Option[Char], Double) = {
    val prob = Random.nextDouble()
    val row = MarkovChain.cumulateRow(getRow(string))
    val next: Option[(Option[Char], Double)] = row.dropWhile(_._3 < prob).headOption.map(kv ⇒ (kv._1, kv._2))
    next.getOrElse((None, 0.0d))
  }
}

case class WeightedChain(weight: Double, chain: MarkovChain) {
  def map(f: MarkovChain ⇒ MarkovChain): WeightedChain = this.copy(chain = f(chain))
}

case class MarkovChains(wchains: Seq[WeightedChain]) extends MarkovChain {
  def generateFromDocs(docs: Vector[String]): MarkovChain = {
    MarkovChains(wchains.map(wchain ⇒ wchain.map(_.generateFromDocs(docs))))
  }

  def nextChar(string: String): (Option[Char], Double) = {
    val charsWithProbs: Seq[(Option[Char], Double)] =
      wchains.map( wchain => {
                    val (char, prob) = wchain.chain.nextChar(string)
                    (char, prob * wchain.weight)
                  }
      )
    val normalizedRow: Vector[(Option[Char], Double, Double)] = MarkovChain.cumulateRow(MarkovChain.normalizeRow(charsWithProbs.toMap))
    val rand = Random.nextDouble()
    val next: Option[(Option[Char], Double)] = normalizedRow.dropWhile( _._3 < rand ).headOption.map(kv ⇒ (kv._1, kv._2))
    next.getOrElse((None, 0.0d))
  }
}
