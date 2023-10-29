package chip8.emulator

import engine.GameBase
import engine.graphics.{Color, Point, Rectangle}
import processing.core.PApplet
import processing.event.KeyEvent
import chip8.logic._
import chip8.emulator.Chip8Emulator._
import chip8.logic.{Point => GridPoint}
import ddf.minim._
import scala.collection.mutable

class Chip8Emulator extends GameBase {

  //YOUR PREFERED COLORSCHEME AND GAME HERE
  private val gameLogic: Chip8Logic = Chip8Logic(getColorScheme("Default"), "[PATH/TO/YOUR/ROM]")
  //Enable this if you need compatibility with older CHIP-8 games
  private val delayDisplay: Boolean = false

  private val updateTimer = new UpdateTimer(Chip8Logic.FramesPerSecond.toFloat)
  val gridDims: Dimensions = gameLogic.gridDims
  private val widthInPixels: Int = (WidthCellInPixels * gridDims.width).ceil.toInt
  private val heightInPixels: Int = (HeightCellInPixels * gridDims.height).ceil.toInt
  private val screenArea: Rectangle = Rectangle(Point(0, 0), widthInPixels.toFloat, heightInPixels.toFloat)
  var timers: mutable.Map[String, Char] = mutable.Map[String, Char]("delayTimer" -> 0, "soundTimer" -> 0)
  private val minim: Minim = new Minim(this)
  private var audioPlayer: AudioPlayer = _

  override def draw(): Unit = {
    updateState()
    runStep()
    drawGrid()
    handleSound()
  }

  private def runStep(): Unit = {
    var drawInstructionExecuted: Boolean = false
    timers.keys.foreach { timer =>
      if (timers(timer) > 0) timers(timer) = (timers(timer) - 1).toChar
    }

    for (_ <- 0 until Chip8Logic.ClockSpeed / Chip8Logic.FramesPerSecond) {
      if (delayDisplay && drawInstructionExecuted) return

      val (newTimers, newDrawInstructionExecuted) = gameLogic.step(timers)
      timers = newTimers
      drawInstructionExecuted = newDrawInstructionExecuted
    }
  }


  private def drawGrid(): Unit = {
    val widthPerCell = screenArea.width / gridDims.width
    val heightPerCell = screenArea.height / gridDims.height

    for (p <- gridDims.allPointsInside) {
      drawCell(getCell(p), gameLogic.getCellColor(p))
    }

    def getCell(p: GridPoint): Rectangle = {
      val leftUp = Point(screenArea.left + p.x * widthPerCell,
        screenArea.top + p.y * heightPerCell)
      Rectangle(leftUp, widthPerCell, heightPerCell)
    }

    def drawCell(area: Rectangle, color: Color): Unit = {
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
    size(widthInPixels, heightInPixels)
  }

  private def setupAudio(): Unit = {
    audioPlayer = minim.loadFile("resources/beep.wav")
    audioPlayer.loop()
    audioPlayer.mute()
  }

  private def handleSound(): Unit = {
    if (timers("soundTimer") > 0) {
      audioPlayer.unmute()
    } else {
      audioPlayer.mute()
    }
  }

  override def setup(): Unit = {
    updateTimer.init()
    setupAudio()
    noStroke()
  }

  private def updateState(): Unit = {
    if (updateTimer.timeForNextFrame()) {
      updateTimer.advanceFrame()
    }
  }

  private def getColorScheme(schemeName: String): Map[String, Color] = {
    schemeName match {
      case "Default" => Map("Empty" -> Color.Black, "Filled" -> Color.White)
      case "Reverse" => Map("Empty" -> Color.White, "Filled" -> Color.Black)
      case "Sky" => Map("Empty" -> Color.LightBlue, "Filled" -> Color.DarkBlue)
      case "Forest" => Map("Empty" -> Color.Green, "Filled" -> Color.Brown)
      case "Desert" => Map("Empty" -> Color.SandyBrown, "Filled" -> Color.DarkRed)
      case _ => Map("Empty" -> Color.Black, "Filled" -> Color.White) // default
    }
  }
}

object Chip8Emulator {
  private val WidthCellInPixels: Double = 15 * Chip8Logic.DrawSizeFactor
  private val HeightCellInPixels: Double = WidthCellInPixels

  def main(args: Array[String]): Unit = {
    PApplet.main("chip8.emulator.Chip8Emulator")
  }
}