package calculator

object TweetLength {
  final val MaxTweetLength = 140
  final val Thresholds: (Int, Int) = (20, 10)

  def tweetRemainingCharsCount(tweetText: Signal[String]): Signal[Int] = {
    Signal(140 - tweetLength(tweetText()))
  }

  def colorForRemainingCharsCount(remainingCharsCount: Signal[Int]): Signal[String] = {
    Signal(remainingCharsCount() match {
      case v if v > Thresholds._1 => "gray" // Twitter counter is gray for rem chars > 20
      case v if v > Thresholds._2 => "orange" // Twitter counter is darkred for 10 < rem chars < 20, but I'll use orange
      case v if v >= 0 => "darkred" // Twitter counter is red for rem chars <= 10, it goes negative keeping red color
      case _ => "red"
    })
  }

  /** Computes the length of a tweet, given its text string.
   *  This is not equivalent to text.length, as tweet lengths count the number
   *  of Unicode *code points* in the string.
   *  Note that this is still a simplified view of the reality. Full details
   *  can be found at
   *  https://dev.twitter.com/overview/api/counting-characters
   */
  private def tweetLength(text: String): Int = {
    /* This should be simply text.codePointCount(0, text.length), but it
     * is not implemented in Scala.js 0.6.2.
     */
    if (text.isEmpty) 0
    else {
      text.length - text.init.zip(text.tail).count(
          (Character.isSurrogatePair _).tupled)
    }
  }
}
