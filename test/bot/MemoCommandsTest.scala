package bot

import org.scalatest.FunSuite

/**
  * Test memo commands (parser only at the moment).
  */
class MemoCommandsTest extends FunSuite {
  val commands: Seq[String] = Seq(
    "memo get dadjoke",
    "memo set dadjoke Did you hear about the new haptics peripheral for VR sailing games? It's called the Oculus Raft!",
    "memo clear dadjoke",
    "memo list",
    "help memo",
    "help memo get"
  )

  val memoCommands = new MemoCommands(null)

  for (command <- commands) {
    test(s"parse $command") {
      assert(memoCommands.accept.isDefinedAt(command.split(' ')))
    }
  }
}
