package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._

import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.util.control.Breaks.break
import scala.concurrent.duration._
import processing.event.KeyEvent



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

  private val screen = Array.ofDim[Boolean](gridDims.height, gridDims.width)
  def resetScreen(): Unit = {
    for {
      y <- 0 until gridDims.height
      x <- 0 until gridDims.width
    } {
      screen(y)(x) = false
    }
  }

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

  val memory = new Array[Char](4096)
  val registers: Array[Char] = new Array[Char](16)
  val font: Array[Char] = {
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

    (zero ++ one ++ two ++ three ++ four ++ five ++ six ++ seven ++ eight ++ nine ++ A ++ B ++ C ++ D ++ E ++ F).map(_.toChar)
  }


  // Load font into memory
  Array.copy(font, 0, memory, 0x50, font.length)

  def loadProgramIntoMemory(filePath: String): Unit = {
    val program = Files.readAllBytes(Paths.get(filePath))
    Array.copy(program.map(b => (b & 0xFF).toChar), 0, memory, 0x200, program.length)
  }

  var programCounter = 512
  var indexRegister = 0
  var delayTimer: Char = 0
  var soundTimer: Char = 0
  var keysPressed = mutable.Queue[Int]()

  loadProgramIntoMemory("src/tetris/logic/4-flags.ch8")

  def fetch(): Int = {
    val instruction = ((memory(programCounter).toInt & 0xFF) << 8) | (memory(programCounter + 1).toInt & 0xFF)
    programCounter += 2
    instruction
  }


  def step(): Unit = {//TODO: Keypresses dont seem to work
    val instruction = fetch()
    if (delayTimer > 0) delayTimer = (delayTimer - 1).toChar
    if (soundTimer > 0) soundTimer = (soundTimer - 1).toChar

      val firstNibble = (instruction & 0xF000) >> 12
      val x = (instruction & 0x0F00) >> 8 // Extract X
      val y = (instruction & 0x00F0) >> 4 // Extract Y
      val n = instruction & 0x000F // Extract N
      val nn = instruction & 0x00FF // Extract NN
      val nnn = instruction & 0x0FFF // Extract NNN

      firstNibble match {
        case 0x0 => {
          nnn match {
            case 0x0E0 => resetScreen()
            case 0x0EE => {
              programCounter = AddressStack.pop()
            }
          }
        }
        case 0x1 => {
          programCounter = nnn
        }
        case 0x2 => {
          AddressStack.push(programCounter)
          programCounter = nnn
        }
        case 0x3 => {
          val value = registers(x) & 0xFF
          if (value == nn) {
            programCounter += 2
          }
        }
        case 0x4 =>
          if (registers(x) != nn) {
            programCounter += 2
          }
        case 0x5 => {
          if (registers(x) == registers(y)) {
            programCounter += 2
          }
        }
        case 0x6 => {
          registers(x) = nn.toChar
        }
        case 0x7 => {
          registers(x) = ((registers(x) + nn) & 0xFF).toChar
        }
        case 0x8 =>
          n match {
            case 0x0 => registers(x) = registers(y)
            case 0x1 => registers(x) = (registers(x) | registers(y)).toChar
            case 0x2 => registers(x) = (registers(x) & registers(y)).toChar
            case 0x3 => registers(x) = (registers(x) ^ registers(y)).toChar
            case 0x4 => {
              val flag: Char = if (registers(x) + registers(y) > 255) 1 else 0
              registers(x) = ((registers(x) + registers(y)) & 0xFF).toChar
              registers(0xF) = flag
            }
            case 0x5 => {
              val flag: Char = if (registers(x) >= registers(y)) 1 else 0
              registers(x) = ((registers(x) - registers(y)) & 0xFF).toChar
              registers(0xF) = flag
            }
            case 0x7 => {
              val flag: Char = if (registers(y) >= registers(x)) 1 else 0
              registers(x) = ((registers(y) - registers(x)) & 0xFF).toChar
              registers(0xF) = flag
            }
            case 0xE =>
              val flag: Char = (((registers(x).toInt & 0x80) >> 7) & 0xFF).toChar
              registers(x) = ((registers(x).toInt << 1) & 0xFF).toChar
              registers(0xF) = flag
            case 0x6 =>
              val flag: Char = (registers(x) & 0x1).toChar
              registers(x) = (registers(x) >> 1).toChar
              registers(0xF) = flag
          }
        case 0x9 => {
          if (registers(x) != registers(y)) {
            programCounter += 2
          }
        }
        case 0xA => {
          indexRegister = nnn
        }
        case 0xB => {
          programCounter = nnn + registers(0)
        }
        case 0xC => {
          val randomValue = scala.util.Random.nextInt(256)
          registers(x) = (randomValue & nn).toChar
        }
        case 0xD => {
          val xCoordinate = (registers(x) & 0xFF) % gridDims.width
          val yCoordinate = (registers(y) & 0xFF) % gridDims.height
          registers(0xF) = 0
          for (row <- 0 until n) {
            val spriteData = memory(indexRegister + row)
            for (bit <- 0 until 8) {
              val mask = 1 << (7 - bit)
              val spritePixel = (spriteData & mask) != 0
              if (xCoordinate + bit < gridDims.width && yCoordinate + row < gridDims.height) {
                val screenPixel = screen(yCoordinate + row)(xCoordinate + bit)
                if (spritePixel && screenPixel) {
                  screen(yCoordinate + row)(xCoordinate + bit) = false
                  registers(0xF) = 1
                } else if (spritePixel) {
                  screen(yCoordinate + row)(xCoordinate + bit) = true
                }
              }
            }
          }
        }
        case 0xE => {
          nn match {
            case 0x9E => if (isKeyPressed(registers(x).toInt)) programCounter += 2
            case 0xA1 => if (!isKeyPressed(registers(x).toInt)) programCounter += 2
          }
        }
        case 0xF => {
          nn match {
            case 0x07 => registers(x) = delayTimer
            case 0x15 => delayTimer = registers(x)
            case 0x18 => soundTimer = registers(x)
            case 0x1E =>
              val sum = indexRegister + registers(x).toInt
              indexRegister = sum & 0xFFF  // Ensure the index register wraps around
              registers(0xF) = if (sum > 0xFFF) 1.toChar else 0.toChar
            case 0x0A =>
              if(keysPressed.isEmpty) {
                programCounter -= 2
              } else {
                val key = keysPressed.dequeue() // Get and remove the first element from the queue
                registers(x) = mapProcessingKeyToChip8Key(key.toChar).toChar
              }
            case 0x29 => indexRegister = registers(x) * 5 + 0x50
            case 0x33 =>
              val value = registers(x) & 0xFF
              memory(indexRegister) = (value / 100).toChar // Hundreds place
              memory(indexRegister + 1) = ((value % 100) / 10).toChar // Tens place
              memory(indexRegister + 2) = (value % 10).toChar // Ones place
            case 0x55 =>
              Array.copy(registers, 0, memory, indexRegister, x + 1)

            case 0x65 =>
              Array.copy(memory, indexRegister, registers, 0, x + 1)
          }
        }
      }
    }

  // TODO implement me

  def mapProcessingKeyToChip8Key(key: Char): Int = {
    key match {
      case '1' => 0x1
      case '2' => 0x2
      case '3' => 0x3
      case '4' => 0xC
      case 'q' | 'Q' => 0x4
      case 'w' | 'W' => 0x5
      case 'e' | 'E' => 0x6
      case 'r' | 'R' => 0xD
      case 'a' | 'A' => 0x7
      case 's' | 'S' => 0x8
      case 'd' | 'D' => 0x9
      case 'f' | 'F' => 0xE
      case 'z' | 'Z' => 0xA
      case 'x' | 'X' => 0x0
      case 'c' | 'C' => 0xB
      case 'v' | 'V' => 0xF
      case _ => -1  // Indicates an unsupported key
    }
  }

  def isKeyPressed(chip8Key: Int): Boolean = {
    val mappedKey = mapProcessingKeyToChip8Key(chip8Key.toChar)
    keysPressed.contains(mappedKey)
  }

  def keyPressed(key: Int): Unit = {
    keysPressed.enqueue(key)
  }

  def keyReleased(key: Int): Unit = {
    keysPressed = keysPressed.filter(_ != key)
  }

  // TODO implement me
  def isGameOver: Boolean = false

  // TODO implement me
  def getCellType(p : Point): CellType = {
    if(screen(p.y)(p.x)) return OCell
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