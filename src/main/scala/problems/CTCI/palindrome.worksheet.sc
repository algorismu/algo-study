def isPermutationOfPalindrome(phrase: String): Boolean =
  // Hepler functions
  def defaultCount: Option[Int] => Option[Int] =
    case Some(n) => Some(n + 1)
    case None    => Some(1)

  def isSingleton: ((Char, Int)) => Boolean =
    (c, n) => n == 1

  def hasEvenFrequency: ((Char, Int)) => Boolean =
    (c, n) => n % 2 == 0

  // Solution starts here
  if phrase.length < 2
  then true
  else
    // Filter alphanumeric characters
    val text = phrase.filter(_.isLetterOrDigit).toLowerCase

    // Count character frequency
    var charFrequency = Map[Char, Int]()
    for char <- text do
      val updatedFrequency = charFrequency.updatedWith(char)(defaultCount)
      charFrequency = updatedFrequency

    if text.length % 2 == 1 && charFrequency.count(isSingleton) == 1
    then
      val charsWithEvenCount = charFrequency.count(hasEvenFrequency)
      charsWithEvenCount == charFrequency.size - 1
    else charFrequency.forall(hasEvenFrequency)

end isPermutationOfPalindrome

// Test on ASCII
isPermutationOfPalindrome("ABBA")
isPermutationOfPalindrome("")
isPermutationOfPalindrome(" ")
isPermutationOfPalindrome("Eve")
isPermutationOfPalindrome(" anna ")
isPermutationOfPalindrome("Tact Coa")
isPermutationOfPalindrome("TACO CAT")
isPermutationOfPalindrome("nana")
isPermutationOfPalindrome("TypescripT")
isPermutationOfPalindrome("Moon Knight")
isPermutationOfPalindrome("a man, a plan, a canal panama!")
