package algorithms.hanoi

/** A Solver for the Tower of Hanoi puzzle
  *
  * -- Steps:
  *   - Move n-1 disks from src to tmp
  *   - Move the nth disk from src to dst
  *   - Move n-1 disks from tmp to dst
  */
def hanoi(n: Int) =
  def transfer(n: Int, src: String, dst: String, tmp: String): Unit =
    if n < 1 then return
    else
      transfer(n - 1, src, tmp, dst)
      println(s"Move disk $n from $src to $dst")
      transfer(n - 1, tmp, dst, src)
    end if
  transfer(n, "A", "B", "Temp")
