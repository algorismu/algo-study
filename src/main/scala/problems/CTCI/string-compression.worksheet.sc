/** String Compression: Implement a method to perform basic string compression
  * using the counts of repeated characters. For example, the string aabcccccaaa
  * would become a2b1c5a3. If the "compressed" string would not become smaller
  * than the original string, your method should return the original string. You
  * can assume the string has only uppercase and lowercase letters (a - z).
  */

type Compression = List[(Char, Int)]

extension (comp: Compression)
  def source: String =
    comp.map((c, n) => c.toString * n).mkString

  def string: String =
    comp.map((c, n) => c.toString + n.toString).mkString
end extension

extension (s: String)
  private def toCompression: Compression =
    val first = s.headOption
    first match
      case Some(c) =>
        val (reps, rest) = (s.takeWhile(x => x == c), s.dropWhile(x => x == c))
        (c, reps.length) :: rest.toCompression
      case None => Nil

  def compressed: String =
    val result = s.toCompression.string
    if result.length >= s.length
    then s
    else result
end extension


// Test
val test = "aabcccccaaa"

test.compressed
