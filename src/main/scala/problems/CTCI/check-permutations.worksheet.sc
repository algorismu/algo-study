/** Given two strings, write a method to decide if one is a permutation of the
  * other.
  */

// Q: Should we consider case-sensitivity in ASCII mode?
// Q: Is white-space significant?

def checkPermutation(a: String, b: String): Boolean =
  if a.length != b.length
  then false
  else
    val charA = a.toLowerCase.toList.sorted
    val charB = b.toLowerCase.toList.sorted
    charA == charB
end checkPermutation

// Testing ASCII
checkPermutation("", "")
checkPermutation("lame", "male")
checkPermutation("EVE", "eve")
checkPermutation("Name", "MEAN")
