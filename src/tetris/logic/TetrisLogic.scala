package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._

import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.collection.mutable.Stack



/** To implement Tetris, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``tetris`` package.
 */
class TetrisLogic(val randomGen: RandomGenerator,
                  val gridDims : Dimensions,
                  val initialBoard: Seq[Seq[CellType]]) {


  def this(random: RandomGenerator, gridDims : Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))

  object AddressStack {
    // Define the stack
    private val stack = mutable.Stack[Int]()

    // Push a 16-bit address onto the stack
    def push(address: Int): Unit = {
      if (address >= 0x0000 && address <= 0xFFFF) {
        stack.push(address)
      } else {
        throw new IllegalArgumentException("Invalid 16-bit address!")
      }
    }

    // Pop a 16-bit address from the stack
    def pop(): Int = {
      if (stack.isEmpty) {
        throw new NoSuchElementException("Stack is empty!")
      } else {
        stack.pop()
      }
    }

    // Peek the top of the stack without removing it
    def peek(): Int = {
      if (stack.isEmpty) {
        throw new NoSuchElementException("Stack is empty!")
      } else {
        stack.top
      }
    }

    // Check if the stack is empty
    def isEmpty: Boolean = stack.isEmpty
  }

  val memory = new Array[Byte](4096)
  val font: Array[Byte] = {
    val zero: Array[Int] = Array(0xF0, 0x90, 0x90, 0x90, 0xF0)
    val one: Array[Int] = Array(0x20, 0x60, 0x20, 0x20, 0x70)
    val two: Array[Int] = Array(0xF0, 0x10, 0xF0, 0x80, 0xF0)
    val three: Array[Int] = Array(0xF0, 0x10, 0xF0, 0x10, 0xF0)
    val four: Array[Int] = Array(0x90, 0x90, 0xF0, 0x10, 0x10)
    val five: Array[Int] = Array(0xF0, 0x80, 0xF0, 0x10, 0xF0)
    val six: Array[Int] = Array(0xF0, 0x80, 0xF0, 0x90, 0xF0)
    val seven: Array[Int] = Array(0xF0, 0x10, 0x20, 0x40, 0x40)
    val eight: Array[Int] = Array(0xF0, 0x90, 0xF0, 0x90, 0xF0)
    val nine: Array[Int] = Array(0xF0, 0x90, 0xF0, 0x10, 0xF0)
    val A: Array[Int] = Array(0xF0, 0x90, 0xF0, 0x90, 0x90)
    val B: Array[Int] = Array(0xE0, 0x90, 0xE0, 0x90, 0xE0)
    val C: Array[Int] = Array(0xF0, 0x80, 0x80, 0x80, 0xF0)
    val D: Array[Int] = Array(0xE0, 0x90, 0x90, 0x90, 0xE0)
    val E: Array[Int] = Array(0xF0, 0x80, 0xF0, 0x80, 0xF0)
    val F: Array[Int] = Array(0xF0, 0x80, 0xF0, 0x80, 0x80)

    (zero ++ one ++ two ++ three ++ four ++ five ++ six ++ seven ++ eight ++ nine ++ A ++ B ++ C ++ D ++ E ++ F).map(_.toByte)
  }


  // Load font into memory
  Array.copy(font, 0, memory, 0x50, font.length)

  def loadProgramIntoMemory(filePath: String): Unit = {
    val program = Files.readAllBytes(Paths.get(filePath))
    Array.copy(program, 0, memory, 0x200, program.length)
  }
  var programCounter = 512

  loadProgramIntoMemory("src/tetris/logic/IBM.ch8")
  def fetch(): Int = {
    val instruction = ((memory(programCounter) & 0xFF) << 8) | (memory(programCounter + 1) & 0xFF) // Promote bytes to int before bitwise operations
    programCounter += 2 // Increment the program counter to the next instruction
    instruction
  }

  var instruction = fetch()
  println(s"Instruction: $instruction, Hex: 0x${instruction.toHexString.toUpperCase}")
  instruction = fetch()
  println(s"Instruction: $instruction, Hex: 0x${instruction.toHexString.toUpperCase}")


  // TODO implement me
  def keyPressed(key: Int): Unit = ()

  // TODO implement me
  def isGameOver: Boolean = false

  // TODO implement me
  def getCellType(p : Point): CellType = {
    if (p.x % 2 == 0) return OCell
    Empty
  }
}

object TetrisLogic {

  val FramesPerSecond: Int = 60 // change this to speed up or slow down the game

  val DrawSizeFactor = 1.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller



  def makeEmptyBoard(gridDims : Dimensions): Seq[Seq[CellType]] = {
    val emptyLine = Seq.fill(gridDims.width)(Empty)
    Seq.fill(gridDims.height)(emptyLine)
  }


  // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultDims.width and DefaultDims.height


  val DefaultWidth: Int = 64
  val NrTopInvisibleLines: Int = 0
  val DefaultVisibleHeight: Int = 32
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  val DefaultDims : Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)


  def apply() = new TetrisLogic(new ScalaRandomGen(),
    DefaultDims,
    makeEmptyBoard(DefaultDims))

}