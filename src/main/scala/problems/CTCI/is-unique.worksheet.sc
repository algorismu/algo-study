/** Implement an algorithm to determine if a string has all unique characters.
  * What if you cannot use additional data structures?
  */

def isUnique(s: String): Boolean =
  val chars = s.toLowerCase.toCharArray.toSet
  chars.size == s.length

// Testing ASCII
isUnique("isUnique")
isUnique("iI")
isUnique("abcd")
isUnique("x")
isUnique("")

// Testing Unicode
isUnique("こんにちは")
isUnique("ちち")
isUnique("مرحبا")
isUnique("ممكن")
