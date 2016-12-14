package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  val anSet = map((x => x >0 && x < 101), x => x +1)
  println(anSet)

  val s1 = singletonSet(1)
  val s11 = singletonSet(-1)
}
