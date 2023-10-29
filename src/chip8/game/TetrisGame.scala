package chip8.game

import engine.GameBase
import engine.graphics.{Color, Point, Rectangle}
import processing.core.PApplet
import processing.event.KeyEvent
import chip8.logic._
import chip8.game.TetrisGame._
import chip8.logic.{Point => GridPoint}
import ddf.minim._
import scala.collection.mutable

class TetrisGame extends GameBase {

  var gameLogic: TetrisLogic = TetrisLogic()
  val updateTimer = new UpdateTimer(TetrisLogic.FramesPerSecond.toFloat)
  val gridDims: Dimensions = gameLogic.gridDims
  val widthInPixels: Int = (WidthCellInPixels * gridDims.width).ceil.toInt
  val heightInPixels: Int = (HeightCellInPixels * gridDims.height).ceil.toInt
  val screenArea: Rectangle = Rectangle(Point(0, 0), widthInPixels.toFloat, heightInPixels.toFloat)
  var timers: mutable.Map[String, Char] = mutable.Map[String, Char]("delayTimer" -> 0, "soundTimer" -> 0)
  val delayDisplay: Boolean = false
  val minim: Minim = new Minim(this)
  var audioPlayer: AudioPlayer = _

  override def draw(): Unit = {
    updateState()
    runStep()
    drawGrid()
    handleSound()
  }

  def setupAudio(soundFile: String): Unit = {
    audioPlayer = minim.loadFile(soundFile)
    audioPlayer.loop()
    audioPlayer.mute()
  }

  def handleSound(): Unit = {
    if (timers("soundTimer") > 0) {
      audioPlayer.unmute()
    } else {
      audioPlayer.mute()
    }
  }

  def runStep(): Unit = {
    var drawInstructionExecuted: Boolean = false
    timers.keys.foreach { timer =>
      if (timers(timer) > 0) timers(timer) = (timers(timer) - 1).toChar
    }

    for (_ <- 0 until TetrisLogic.ClockSpeed / TetrisLogic.FramesPerSecond) {
      if (delayDisplay && drawInstructionExecuted) return

      val (newTimers, newDrawInstructionExecuted) = gameLogic.step(timers)
      timers = newTimers
      drawInstructionExecuted = newDrawInstructionExecuted

    }
  }


  def drawGrid(): Unit = {
    val widthPerCell = screenArea.width / gridDims.width
    val heightPerCell = screenArea.height / gridDims.height

    for (p <- gridDims.allPointsInside) {
      drawCell(getCell(p), gameLogic.getCellType(p))
    }

    def getCell(p: GridPoint): Rectangle = {
      val leftUp = Point(screenArea.left + p.x * widthPerCell,
        screenArea.top + p.y * heightPerCell)
      Rectangle(leftUp, widthPerCell, heightPerCell)
    }

    def drawCell(area: Rectangle, tetrisColor: CellType): Unit = {
      val color = tetrisBlockToColor(tetrisColor)
      setFillColor(color)
      drawRectangle(area)
    }

  }

  override def keyPressed(event: KeyEvent): Unit = {
    gameLogic.keyPressed(event.getKeyCode)
  }

  override def keyReleased(event: KeyEvent): Unit = {
    gameLogic.keyReleased(event.getKeyCode)
  }

  override def settings(): Unit = {
    pixelDensity(displayDensity())
    // If line below gives errors try size(widthInPixels, heightInPixels, PConstants.P2D)
    size(widthInPixels, heightInPixels)
  }

  override def setup(): Unit = {
    updateTimer.init()
    setupAudio("src/chip8/logic/beep.wav")
  }

  def updateState(): Unit = {
    if (updateTimer.timeForNextFrame()) {
      updateTimer.advanceFrame()
    }
  }

  def tetrisBlockToColor(color: CellType): Color =
    color match {
      case ICell => Color.LightBlue
      case OCell => Color.White
      case LCell => Color.Orange
      case JCell => Color.Blue
      case SCell => Color.Green
      case Empty => Color.Black
      case TCell => Color.Purple
      case ZCell => Color.Red
    }
}

object TetrisGame {
  val WidthCellInPixels: Double = 15 * TetrisLogic.DrawSizeFactor
  val HeightCellInPixels: Double = WidthCellInPixels

  def main(args: Array[String]): Unit = {
    PApplet.main("chip8.game.TetrisGame")
  }

}