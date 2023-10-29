package chip8.logic

import engine.graphics.Color

import java.nio.file.{Files, Paths}
import scala.collection.mutable

class Chip8Logic(val gridDims: Dimensions, val colorScheme: Map[String, Color], val programPath: String) {
  private val screen = Array.ofDim[Boolean](gridDims.height, gridDims.width)
  private val memory = new Array[Char](4096)
  private val registers: Array[Char] = new Array[Char](16)
  private var programCounter = 512
  private var indexRegister = 0
  private var keysPressed: mutable.Queue[Char] = mutable.Queue[Char]()
  initializeMemory()

  private def fetch(): Int = {
    val instruction = ((memory(programCounter).toInt & 0xFF) << 8) | (memory(programCounter + 1).toInt & 0xFF)
    programCounter += 2
    instruction
  }

  def step(timers: mutable.Map[String, Char]): (mutable.Map[String, Char], Boolean) = {
    val instruction = fetch()
    val firstNibble = (instruction & 0xF000) >> 12
    val secondNibble = (instruction & 0x0F00) >> 8
    val thirdNibble = (instruction & 0x00F0) >> 4
    val fourthNibble = instruction & 0x000F
    val thirdToFourthNibble = instruction & 0x00FF
    val secondToFourthNibble = instruction & 0x0FFF
    val eightBitMask = 0xFF

    firstNibble match {
      case 0x0 =>
        secondToFourthNibble match {
          case 0x0E0 => clearScreen()
          case 0x0EE => programCounter = AddressStack.pop()
        }
      case 0x1 => programCounter = secondToFourthNibble
      case 0x2 =>
        AddressStack.push(programCounter)
        programCounter = secondToFourthNibble

      case 0x3 =>
        val value = registers(secondNibble) & eightBitMask
        if (value == thirdToFourthNibble) {
          programCounter += 2
        }
      case 0x4 =>
        if (registers(secondNibble) != thirdToFourthNibble) {
          programCounter += 2
        }
      case 0x5 =>
        if (registers(secondNibble) == registers(thirdNibble)) {
          programCounter += 2
        }
      case 0x6 =>
        registers(secondNibble) = thirdToFourthNibble.toChar

      case 0x7 =>
        registers(secondNibble) = ((registers(secondNibble) + thirdToFourthNibble) & eightBitMask).toChar

      case 0x8 =>
        fourthNibble match {
          case 0x0 => registers(secondNibble) = registers(thirdNibble)
          case 0x1 =>
            registers(secondNibble) = (registers(secondNibble) | registers(thirdNibble)).toChar
            registers(0xF) = 0.toChar
          case 0x2 =>
            registers(secondNibble) = (registers(secondNibble) & registers(thirdNibble)).toChar
            registers(0xF) = 0.toChar
          case 0x3 =>
            registers(secondNibble) = (registers(secondNibble) ^ registers(thirdNibble)).toChar
            registers(0xF) = 0.toChar
          case 0x4 =>
            val flag: Char = if (registers(secondNibble) + registers(thirdNibble) > 0xFF) 1 else 0
            registers(secondNibble) = ((registers(secondNibble) + registers(thirdNibble)) & eightBitMask).toChar
            registers(0xF) = flag
          case 0x5 =>
            val flag: Char = if (registers(secondNibble) >= registers(thirdNibble)) 1 else 0
            registers(secondNibble) = ((registers(secondNibble) - registers(thirdNibble)) & eightBitMask).toChar
            registers(0xF) = flag
          case 0x7 =>
            val flag: Char = if (registers(thirdNibble) >= registers(secondNibble)) 1 else 0
            registers(secondNibble) = ((registers(thirdNibble) - registers(secondNibble)) & eightBitMask).toChar
            registers(0xF) = flag
          case 0xE =>
            registers(secondNibble) = registers(thirdNibble)
            val flag: Char = (((registers(secondNibble).toInt & 0x80) >> 7) & eightBitMask).toChar
            registers(secondNibble) = ((registers(secondNibble).toInt << 1) & eightBitMask).toChar
            registers(0xF) = flag
          case 0x6 =>
            registers(secondNibble) = registers(thirdNibble)
            val flag: Char = (registers(secondNibble) & 0x1).toChar
            registers(secondNibble) = (registers(secondNibble) >> 1).toChar
            registers(0xF) = flag
        }
      case 0x9 =>
        if (registers(secondNibble) != registers(thirdNibble)) {
          programCounter += 2
        }
      case 0xA => indexRegister = secondToFourthNibble
      case 0xB => programCounter = secondToFourthNibble + registers(0)
      case 0xC =>
        val randomValue = scala.util.Random.nextInt(256)
        registers(secondNibble) = (randomValue & thirdToFourthNibble).toChar
      case 0xD =>
        val xCoordinate = (registers(secondNibble) & eightBitMask) % gridDims.width
        val yCoordinate = (registers(thirdNibble) & eightBitMask) % gridDims.height
        drawToScreen(xCoordinate, yCoordinate, fourthNibble)
        return (timers, true)
      case 0xE =>
        thirdToFourthNibble match {
          case 0x9E => if (keysPressed.contains(registers(secondNibble))) programCounter += 2
          case 0xA1 => if (!keysPressed.contains(registers(secondNibble))) programCounter += 2
        }
      case 0xF =>
        thirdToFourthNibble match {
          case 0x07 => registers(secondNibble) = timers("delayTimer")
          case 0x15 => timers("delayTimer") = registers(secondNibble)
          case 0x18 => timers("soundTimer") = registers(secondNibble)
          case 0x1E =>
            val sum = indexRegister + registers(secondNibble).toInt
            indexRegister = sum & 0xFFF
            registers(0xF) = if (sum > 0xFFF) 1.toChar else 0.toChar
          case 0x0A =>
            if (keysPressed.isEmpty) {
              programCounter -= 2
            } else {
              registers(secondNibble) = keysPressed.dequeue()
            }
          case 0x29 => indexRegister = registers(secondNibble) * 5 + 0x50
          case 0x33 =>
            val value = registers(secondNibble) & eightBitMask
            memory(indexRegister) = (value / 100).toChar // Hundreds place
            memory(indexRegister + 1) = ((value % 100) / 10).toChar // Tens place
            memory(indexRegister + 2) = (value % 10).toChar // Ones place
          case 0x55 =>
            Array.copy(registers, 0, memory, indexRegister, secondNibble + 1)
            indexRegister += (secondNibble + 1)
          case 0x65 =>
            Array.copy(memory, indexRegister, registers, 0, secondNibble + 1)
            indexRegister += (secondNibble + 1)
        }
    }
    (timers, false)
  }

  private def drawToScreen(xCoordinate: Int, yCoordinate: Int, fourthNibble: Int): Unit = {
    registers(0xF) = 0
    for (row <- 0 until fourthNibble) {
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

  private def clearScreen(): Unit = {
    for {
      y <- 0 until gridDims.height
      x <- 0 until gridDims.width
    } {
      screen(y)(x) = false
    }
  }

  private val mapProcessingKeyToChip8Key: Map[Int, Char] = Map(
    49 -> 0x1, // '1'
    50 -> 0x2, // '2'
    51 -> 0x3, // '3'
    52 -> 0xC, // '4'
    81 -> 0x4, // 'q'
    87 -> 0x5, // 'w'
    69 -> 0x6, // 'e'
    82 -> 0xD, // 'r'
    65 -> 0x7, // 'a'
    83 -> 0x8, // 's'
    68 -> 0x9, // 'd'
    70 -> 0xE, // 'f'
    90 -> 0xA, // 'z'
    88 -> 0x0, // 'x'
    67 -> 0xB, // 'c'
    86 -> 0xF // 'v'
  )

  def keyPressed(key: Int): Unit = {
    mapProcessingKeyToChip8Key.get(key.toChar).foreach { mappedKey =>
      keysPressed.enqueue(mappedKey)
    }
  }

  def keyReleased(key: Int): Unit = {
    mapProcessingKeyToChip8Key.get(key.toChar).foreach { mappedKey =>
      keysPressed = keysPressed.filter(_ != mappedKey)
    }
  }

  def getCellColor(p: Point): Color = {
    if (screen(p.y)(p.x)) return colorScheme("Filled")
    colorScheme("Empty")
  }

  private def initializeMemory(): Unit = {
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

      (zero ++ one ++ two ++ three ++ four ++ five ++ six ++ seven ++ eight ++ nine ++ A ++ B ++ C ++ D ++ E ++ F)
        .map(_.toChar)
    }

    def loadProgramIntoMemory(filePath: String): Unit = {
      val program = Files.readAllBytes(Paths.get(filePath))
      Array.copy(program.map(b => (b & 0xFF).toChar), 0, memory, 0x200, program.length)
    }

    Array.copy(font, 0, memory, 0x50, font.length)
    loadProgramIntoMemory(programPath)
  }

  private object AddressStack {
    private val stack = mutable.Stack[Int]()

    def push(address: Int): Unit = {
      if (address >= 0x0000 && address <= 0xFFFF) {
        stack.push(address)
      } else {
        throw new IllegalArgumentException("Invalid 16-bit address!")
      }
    }

    def pop(): Int = {
      if (stack.isEmpty) {
        throw new NoSuchElementException("Stack is empty!")
      } else {
        stack.pop()
      }
    }
  }

}

object Chip8Logic {
  val ClockSpeed: Int = 700
  val FramesPerSecond: Int = 60
  val DrawSizeFactor = 1.0
  private val DefaultWidth: Int = 64
  private val DefaultHeight: Int = 32
  private val DefaultDims: Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)

  def apply(colorScheme: Map[String, Color], programPath: String) = new Chip8Logic(DefaultDims, colorScheme,
    programPath)
}