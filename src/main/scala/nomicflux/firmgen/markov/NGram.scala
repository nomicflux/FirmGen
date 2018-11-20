package markov

object NGram {
  def fromString(n: Int, skip: Int, string: String): NGram = NGram(n, string.group(skip).map(_.head).take(n))
}

case class NGram(n: Int, skip: Int, value: String) {
  def shift(string: String): NGram = this.copy( value = (value ++ string.group(skip).map(_.head)).takeRight(n) )
}
