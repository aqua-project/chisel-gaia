import Chisel._

class Timer (
  val duration: Double = 10, // msec
  val frequency: Double = 66.6, // MHz
  val cycle: Int = 0
) extends Module {

  val io = new Bundle {
    val ready  = Bool(OUTPUT)
    val accept = Bool(INPUT)
  }

  val period  = if (cycle > 0) cycle else (duration * frequency * 1000) toLong
  val counter = RegInit(init = UInt(0, width = log2Up(cycle)))
  val state   = RegInit(Bool(false))

  when (state && io.accept) {
    state := Bool(false)
  }

  when (counter === UInt(period - 1)) {
    counter := UInt(0)
    state   := Bool(true)
  } .otherwise {
    counter := counter + UInt(1)
  }

  io.ready := state
}

object Timer {

  def main(args: Array[String]): Unit = {
    chiselMainTest(args, () => Module(new Timer(cycle = 6))) { c =>
      new TimerTests(c)
    }
  }

  class TimerTests(c: Timer) extends Tester(c, isTrace = false) {

    val timing  = "-----#-----#-----#-----#-----#-----#-----#-----#-----#"
    val redies  = "------#-----###---#########---#########---#-----#-----"
    val accepts = "------#-------#--------#--#-----------#############---"

    def c2i (c: Char) =
      c match {
        case '-' => 0
        case '#' => 1
        case  _  => -1
      }

    for ((ready, accept) <- redies zip accepts) {
      poke(c.io.accept, c2i(accept))
      expect(c.io.ready, c2i(ready))
      step(1)
    }
  }
}
