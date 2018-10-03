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
    var tempScore = 0
    if (player1Score.even(player2Score)) {
      score = player1Score.point match {
        case 0 => "Love-All"
        case 1 => "Fifteen-All"
        case 2 => "Thirty-All"
        case _ => "Deuce"
      }
    }
    else if (player1Score.isDeuce(player2Score)) {
      val minusResult = player1Score.point - player2Score.point
      if (minusResult == 1) score = "Advantage player1"
      else if (minusResult == -1) score = "Advantage player2"
      else if (minusResult >= 2) score = "Win for player1"
      else score = "Win for player2"
    }
    else {
      (1 to 2).foreach { i =>
        if (i == 1) tempScore = player1Score.point
        else {
          score += "-"
          tempScore = player2Score.point
        }
        val tempScore2 = tempScore match {
          case 0 => "Love"
          case 1 => "Fifteen"
          case 2 => "Thirty"
          case 3 => "Forty"
        }
        score += tempScore2
      }
    }
    score
  }

  class Score() {
    def isDeuce(m_score2: Score): Boolean = point >= 4 || m_score2.point >= 4

    def even(m_score2: Score): Boolean = point == m_score2.point

    var point: Int = 0

    def ++(): Score = {
      point = point + 1
      return this
    }
  }

}
