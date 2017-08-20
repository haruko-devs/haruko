package bot

/**
  * TODO: support for non-English locales is left as an exercise to the reader.
  */
case class Pronoun(
  subject: String,
  `object`: String,
  determiner: String,
  possessive: String,
  reflexive: String,
  contractionExists: String,
  contractionPossesses: String,
  contractionExistsNegated: String,
  contractionPossessesNegated: String
)

object Pronoun {
  val fallback: Pronoun = Pronoun(
    subject = "they",
    `object` = "them",
    determiner = "their",
    possessive = "theirs",
    reflexive = "themself",
    contractionExists = "'re",
    contractionPossesses = "'ve",
    contractionExistsNegated = "aren't",
    contractionPossessesNegated = "haven't"
  )

  private val all: Map[String, Pronoun] = Seq(
    fallback,
    Pronoun(
      subject = "she",
      `object` = "her",
      determiner = "her",
      possessive = "hers",
      reflexive = "herself",
      contractionExists = "'s",
      contractionPossesses = "'s",
      contractionExistsNegated = "isn't",
      contractionPossessesNegated = "hasn't"
    ),
    Pronoun(
      subject = "he",
      `object` = "him",
      determiner = "his",
      possessive = "his",
      reflexive = "himself",
      contractionExists = "'s",
      contractionPossesses = "'s",
      contractionExistsNegated = "isn't",
      contractionPossessesNegated = "hasn't"
    ),
    Pronoun(
      subject = "ey",
      `object` = "em",
      determiner = "eir",
      possessive = "eirs",
      reflexive = "eirself",
      contractionExists = "'re",
      contractionPossesses = "'ve",
      contractionExistsNegated = "aren't",
      contractionPossessesNegated = "haven't"
    )
  )
    .map(pronoun => s"${pronoun.subject}/${pronoun.`object`}" -> pronoun)
    .toMap

  def lookup(short: String): Pronoun = all.getOrElse(short, fallback)
}
