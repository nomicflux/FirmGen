package markov

import scala.annotation.tailrec
import scala.util.Random

object MarkovChain {
  type MarkovMatrix = Map[NGram, Vector[(Option[Char], Double)]]
  private def takeNGramWithFollowing(n: Int)(string: String): (NGram, Option[Char]) = {
    val (ngram, next) = string.splitAt(n)
    (NGram.fromString(n, ngram), next.headOption)
  }

  private def takeAllNGramsWithFollowing(n: Int)(string: String): Vector[(NGram, Option[Char])] =
    string.tails.map(takeNGramWithFollowing(n)).toVector

  private def toMarkovMatrix(ngrams: Vector[(NGram, Option[Char])]): MarkovMatrix =
    ngrams.groupBy(_._1).mapValues(_.groupBy(_._2).mapValues(_.size.toDouble).toVector)

  private def normalizeRow(row: Vector[(Option[Char], Double)]): Vector[(Option[Char], Double)] = {
    val cts = row.map(_._2).sum
    row.map( item => item.copy(_2 = item._2 / cts) )
  }

  private def cumulateRow(row: Vector[(Option[Char], Double)]): Vector[(Option[Char], Double)] = {
    row.foldLeft((Vector.empty[(Option[Char], Double)], 0.0)) { case ((acc, prob), (next, newProb)) =>
      val cumulative = prob + newProb
      (acc :+ (next, cumulative), cumulative)
    }._1
  }

  private def normalizeMatrix(matrix: MarkovMatrix): MarkovMatrix = matrix.mapValues(row => cumulateRow(normalizeRow(row)))

  def apply(chainSize: Int, docs: Vector[String]): MarkovChain = {
    val matrix = toMarkovMatrix(docs.flatMap(takeAllNGramsWithFollowing(chainSize)))
    MarkovChain(chainSize, normalizeMatrix(matrix))
  }
}

case class MarkovChain(chainSize: Int, probs: MarkovChain.MarkovMatrix) {
  private def splitOption[A](vec: Vector[A]): Option[(A, Vector[A])] = 
    if (vec.isEmpty) None else Some((vec.head, vec.tail))

  def nextChar(string: String): (Option[Char], Double) = {
    val ngram = NGram.fromString(chainSize, string)
    val prob = Random.nextDouble()   

    @tailrec
    def nextCharHelper(remainder: Vector[(Option[Char], Double)], prevProb: Double): (Option[Char], Double) =
      splitOption(remainder) match {
        case None => (None, prevProb)
        case Some(((head, currProb), rest)) if currProb < prob => nextCharHelper(rest, currProb)
        case Some(((head, currProb), rest)) => (head, currProb - prevProb)
      }

    nextCharHelper(probs.getOrElse(ngram, Vector.empty), 0.0)
  }
}
