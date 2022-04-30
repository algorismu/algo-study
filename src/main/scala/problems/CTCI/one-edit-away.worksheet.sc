/* There are three types of edits that can be performed on strings:
insert a character, remove a character, or replace a character.
Given two strings, write a function to check if they are one edit (or zero edits) away.
EXAMPLE
    pale, ple   -> true
    pales, pale -> true
    pale,bale   -> true
    pale,bae    -> false
 */

def oneEditAway(a: String, b: String): Boolean =
  if a == b
  then true // Zero edits away
  else if a.length == b.length
  then
    // Equal length, then should be one replacement
    val charPairs = a zip b
    val commonChars = charPairs.count((x, y) => x == y)
    commonChars == a.length - 1
  // one string is longer that the other by obly one character
  else if Math.abs(a.length - b.length) == 1 && a.length < b.length
  then a.forall(c => b contains c)
  else b.forall(c => a contains c)
end oneEditAway

// Test on provided samples
oneEditAway("pale", "ple")
oneEditAway("pales", "pale")
oneEditAway("pale", "bale")
oneEditAway("pale", "bae")
oneEditAway("box", "xbox")
