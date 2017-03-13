package bot

import scala.util.Random

/**
  * Replies to commands that don't exist.
  */
object Sass {
  val responses = IndexedSeq(
    "You're cute, but no.",
    "You're cute, but maybe we could do something else instead?",
    "You're not my real mom and I don't have to do what you say!",
    "You're not my supervisor!",
    "Not now, I have a headache.",
    "Not now, I'm hung over.",
    "Not now, I'm kinda drunk, but, like, not a fun buzz at all.",
    "You're so darling, but I have no idea what you're talking about.",
    "Maybe we better take it to DM.",
    "Reply hazy, try again.",
    "Ask again later.",
    "Better not tell you now.",
    "Concentrate and ask again!",
    "Don't count on it.",
    "Outlook not so good.",
    "My sources say no.",
    "My source code says no.",
    "Mom won't let me.",
    "Navy SEALs are standing by.",
    "How about a tactical nuclear strike?",
    "Are you sitting on the F5 key? Cause your ass is refreshing.",
    "You had me at \"Hello World.\"",
    "Mind if I run a sniffer to see if your ports are open?",
    "I think you could be an integral part of my project life cycle.",
    "How about we go home and you handle my exception?",
    "We have top men working on it right now. Top... men."
  )

  private val rand = new Random()

  def randomResponse: String = {
    responses(rand.nextInt(responses.length))
  }
}
