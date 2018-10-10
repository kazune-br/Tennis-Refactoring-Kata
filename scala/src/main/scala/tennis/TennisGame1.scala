package tennis


class TennisGame1(val player1Name: String, val player2Name: String) extends TennisGame {
  var player1Score: Score = new Score()
  var player2Score: Score = new Score()

  def wonPoint(playerName: String) {
    if (playerName == "player1")
      player1Score.++
    else
      player2Score.++
  }

  def calculateScore(): String = {
    var score: String = ""

    if (player1Score.even(player2Score)) {
      score = player1Score.point match {
        case 0 => "Love-All"
        case 1 => "Fifteen-All"
        case 2 => "Thirty-All"
        case _ => "Deuce"
      }
    }
    else if (player1Score.isDeuce(player2Score)) {
      score = player1Score.scoreFromDeuce(player2Score)
    }
    else {
      score += player1Score.toString() + "-" + player2Score.toString()
    }
    score
  }


  class Score() {
    def scoreFromDeuce(player2Score: Score): String = {

      val difference = point - player2Score.point
      difference match {
        case 1 => "Advantage player1"
        case -1 => "Advantage player2"
        case _ if difference >= 2 => "Win for player1"
        case _ => "Win for player2"
      }
    }

    override def toString() = {
      (point match {
        case 0 => "Love"
        case 1 => "Fifteen"
        case 2 => "Thirty"
        case 3 => "Forty"
      })
    }

    def isDeuce(m_score2: Score): Boolean = point >= 4 || m_score2.point >= 4

    def even(m_score2: Score): Boolean = point == m_score2.point

    var point: Int = 0

    def ++(): Score = {
      point = point + 1
      return this
    }
  }

}
