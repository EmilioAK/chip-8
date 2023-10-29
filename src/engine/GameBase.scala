package engine
import engine.graphics.{Color, Rectangle}
import processing.core.PApplet

class GameBase extends PApplet {
  class UpdateTimer(val framesPerSecond: Float) {

    private val frameDuration: Float = 1000 / framesPerSecond // ms
    private var nextFrame: Float = Float.MaxValue

    def init(): Unit = nextFrame = currentTime() + frameDuration
    def timeForNextFrame(): Boolean = currentTime() >= nextFrame
    def advanceFrame(): Unit = nextFrame = nextFrame + frameDuration

  }
  def currentTime(): Int = millis()

  def drawRectangle(r: Rectangle): Unit =
    rect(r.left,r.top, r.width, r.height)

  def setFillColor(c: Color): Unit =
    fill(c.red, c.green, c.blue, c.alpha)
}
