package org.spbsu.mkn.scala

import scala.io.StdIn.readLine
import scala.util.Random
import scala.util.Random

object TheGame {

  sealed trait GuessResult

  case class Correct(numTries: Int) extends GuessResult

  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  case class WrongInput() extends GuessResult

  class RepeatingDigitsException extends RuntimeException {
    override def getMessage: String = {
      "Your input contains repeating symbols."
    }
  }

  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException {
    override def getMessage: String = {
      s"Your input length is $got, but expected $expected."
    }
  }

  private val alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNPQRSTUVWXYZ0123456789"

  def generateNumberString(length: Int): String = Random.shuffle(alphabet).toString().take(length)

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (secret.length != userInput.length) {
      throw new WrongNumberLengthException(secret.length, userInput.length)
    }
    if (!userInput.forall(ch => userInput.count(_ == ch) == 1)) {
      throw new RepeatingDigitsException
    }
    val secretAsArray = secret.toCharArray
    if (userInput == secret) {
      Correct(numTries)
    }
    else {
      Incorrect(
        (0 until secret.length).count(i => secret(i) == userInput(i)),
        (0 until secret.length).count(i => secret(i) != userInput(i) && secretAsArray.contains(userInput(i)))
      )
    }
  }

  private def readLength(): Int = {
    var length = 0
    println("Print preferred length.")
    try {
      length = readLine().toInt
    } catch {
      case _ =>
        println("Your input is not a number.")
        0
    }
    length
  }

  private def getUserInput(secret: String): GuessResult = {
    val input = readLine()
    try {
      validate(secret, input)
    } catch {
      case e: RuntimeException =>
        println(e.getMessage)
        WrongInput()
      case _ => ???
    }
  }

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")

    var appIsOn = true

    while (appIsOn) {
      println("Do you want to play next game? Please, answer yes/no in any case")
      val answer = readLine().toLowerCase()
      if (answer == "no") {
        appIsOn = false
      } else if (answer == "yes") {
        var wordLen = 0
        while (wordLen == 0) {
          wordLen = readLength()
        }

        val secret = generateNumberString(wordLen)
        var numTries = 0
        var gameIsGoing = true

        while (gameIsGoing) {
          numTries += 1

          println("Make your guess")

          var turnResult = getUserInput(secret)
          while (turnResult.isInstanceOf[TheGame.WrongInput]) {
            println("Try again.")
            turnResult = getUserInput(secret)
          }

          turnResult match {
            case _: Correct =>
              println(s"Congratz! You win in $numTries")
              gameIsGoing = false
            case Incorrect(a, b) =>
              println(s"Oops, you are wrong. But you have $a bulls and $b cows.")
            case _ => ???
          }
        }
      } else {
        println("It's not yes or no :( Try again.")
      }
    }
  }
}
