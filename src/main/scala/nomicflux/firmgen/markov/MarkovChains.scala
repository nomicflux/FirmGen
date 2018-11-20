package markov

import scala.util.Random

case class WeightedChain(weight: Double, chain: MarkovChain)

object MarkovChains {
  case class Weight(n: Int, weight: Double)
  def apply(weights: Seq[Weight], docs: Vector[String]): MarkovChains = {
    val chains = weights.map( weight => WeightedChain( weight.weight, MarkovChain( weight.n, docs ) ) )
    MarkovChains(chains)
  }
}

case class MarkovChains(chains: Seq[WeightedChain]) {
  private def combineRows(map1: (Map[Option[Char], Double], Double),
                          map2: (Map[Option[Char], Double], Double)): (Map[Option[Char], Double], Double) = {
    val totalWeight = map1._2 + map2._2
    val newMap = map2._1.foldLeft(map1._1) { case (acc, (k, v)) => acc.get(k) match {
                                              case None => acc + (k -> v)
                                              case Some(prob) => acc + (k -> (prob * map1._2 + v * map2._2))
                                            }}
    (MarkovChain.normalizeRow(newMap), totalWeight)
  }

  private def getWeightedAvg(maps: (Map[Option[Char], Double], Double)): Map[Option[Char], Double] =
    maps.foldLeft((Map.empty, 0.0d))(combineRows)._1

  private def getRows(string: String): Map[Option[Char], Double] = {
    val allRows = chains.map(wchain => (wchain.chain.getRow(string), wchain.weight))
    getWeightedAvg(allRows)
  }

  def nextChar(string: String): Option[Char] = {
    val weightedRow: Vector[(Option[Char], Double)] = MarkovChain.cumulateRow(getRows(string))
    val rand = Random.nextDouble()
    weightedRow.dropWhile( _._2 < rand ).headOption.map(_._1)
  }
}
