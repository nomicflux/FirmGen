package markov

object NGram {
  def fromString(n: Int, string: String): NGram = NGram(n, string.take(n))
}

case class NGram(n: Int, value: String) {
  def shift(string: String): NGram = this.copy(value = (value ++ string).takeRight(n))
}
