package chip8.logic
import engine.graphics.Color

import java.nio.file.{Files, Paths}
import scala.collection.mutable

class Chip8Logic(val gridDims : Dimensions, val colorScheme: Map[String, Color]) {
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
  var keysPressed = mutable.Queue[Char]()

  loadProgramIntoMemory("src/chip8/logic/BRIX")

  def fetch(): Int = {
    val instruction = ((memory(programCounter).toInt & 0xFF) << 8) | (memory(programCounter + 1).toInt & 0xFF)
    programCounter += 2
    instruction
  }


  def step(timers: mutable.Map[String, Char]): (mutable.Map[String, Char], Boolean) = {
    val instruction = fetch()
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
            case 0x1 =>
              registers(x) = (registers(x) | registers(y)).toChar
              registers(0xF) = 0.toChar
            case 0x2 =>
              registers(x) = (registers(x) & registers(y)).toChar
              registers(0xF) = 0.toChar
            case 0x3 =>
              registers(x) = (registers(x) ^ registers(y)).toChar
              registers(0xF) = 0.toChar
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
              registers(x) = registers(y) // Set VX to the value of VY
              val flag: Char = (((registers(x).toInt & 0x80) >> 7) & 0xFF).toChar
              registers(x) = ((registers(x).toInt << 1) & 0xFF).toChar
              registers(0xF) = flag
            case 0x6 =>
              registers(x) = registers(y) // Set VX to the value of VY
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
          return (timers, true)
        }
        case 0xE => {
          nn match {
            case 0x9E => if (keysPressed.contains(registers(x))) programCounter += 2
            case 0xA1 => if (!keysPressed.contains(registers(x))) programCounter += 2
          }
        }
        case 0xF => {
          nn match {
            case 0x07 => registers(x) = timers("delayTimer")
            case 0x15 => timers("delayTimer") = registers(x)
            case 0x18 => timers("soundTimer") = registers(x)
            case 0x1E =>
              val sum = indexRegister + registers(x).toInt
              indexRegister = sum & 0xFFF  // Ensure the index register wraps around
              registers(0xF) = if (sum > 0xFFF) 1.toChar else 0.toChar
            case 0x0A =>
              if(keysPressed.isEmpty) {
                programCounter -= 2
              } else {
                registers(x) = keysPressed.dequeue()
              }
            case 0x29 => indexRegister = registers(x) * 5 + 0x50
            case 0x33 =>
              val value = registers(x) & 0xFF
              memory(indexRegister) = (value / 100).toChar // Hundreds place
              memory(indexRegister + 1) = ((value % 100) / 10).toChar // Tens place
              memory(indexRegister + 2) = (value % 10).toChar // Ones place
            case 0x55 =>
              Array.copy(registers, 0, memory, indexRegister, x + 1)
              indexRegister += (x + 1) // Increment the index register
            case 0x65 =>
              Array.copy(memory, indexRegister, registers, 0, x + 1)
              indexRegister += (x + 1) // Increment the index register
          }
        }
      }
    (timers, false)
    }

  // TODO implement me

  val mapProcessingKeyToChip8Key: Map[Int, Char] = Map(
    49 -> 0x1,  // '1'
    50 -> 0x2,  // '2'
    51 -> 0x3,  // '3'
    52 -> 0xC,  // '4'
    81 -> 0x4,  // 'q'
    87 -> 0x5,  // 'w'
    69 -> 0x6,  // 'e'
    82 -> 0xD,  // 'r'
    65 -> 0x7,  // 'a'
    83 -> 0x8,  // 's'
    68 -> 0x9,  // 'd'
    70 -> 0xE,  // 'f'
    90 -> 0xA,  // 'z'
    88 -> 0x0,  // 'x'
    67 -> 0xB,  // 'c'
    86 -> 0xF   // 'v'
  )

  def keyPressed(key: Int): Unit = {
    mapProcessingKeyToChip8Key.get(key.toChar) match {
      case Some(mappedKey) => keysPressed.enqueue(mappedKey)
      case None => println(s"$key not found")
    }
  }

  def keyReleased(key: Int): Unit = {
    mapProcessingKeyToChip8Key.get(key.toChar) match {
      case Some(mappedKey) => keysPressed = keysPressed.filter(_ != mappedKey)
      case None => println(s"$key not found")
    }
  }
  def getCellType(p : Point): Color = {
    if(screen(p.y)(p.x)) return colorScheme("Filled")
    colorScheme("Empty")
  }
}

object Chip8Logic {
  val ClockSpeed: Int = 700
  val FramesPerSecond: Int = 60
  val DrawSizeFactor = 1.0
  val DefaultWidth: Int = 64
  val DefaultHeight: Int = 32
  val DefaultDims : Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)
  def apply(colorScheme: Map[String, Color]) = new Chip8Logic(DefaultDims, colorScheme)
}