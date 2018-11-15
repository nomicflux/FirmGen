package markov

case class WeightedChain(weight: Double, chain: MarkovChain)

object MarkovChains {
  case class Weight(n: Int, weight: Double)
  def apply(weights: Seq[Weight], docs: Vector[String]): MarkovChains = {
    val chains = weights.map( weight => WeightedChain( weight.weight, MarkovChain( weight.n, docs ) ) )
    MarkovChains(chains)
  }
}

case class MarkovChains(chains: Seq[WeightedChain]) {
  def nextChar(string: String): Option[Char] = {
    val candidates: Seq[(Option[Char], Double)] = chains.map { wchain => 
      val (c, p) = wchain.chain.nextChar(string)
      (c, p * wchain.weight)
    }
    candidates.foldLeft((Option.empty[Char], 0.0)) { case ((accC, accP), (nextC, nextP)) => 
      if (nextP > accP) (nextC, nextP) else (accC, accP) 
    }._1
  }
}
