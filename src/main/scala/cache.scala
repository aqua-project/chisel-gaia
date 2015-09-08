import Chisel._

/*
 Specification
=================

 * line size :128 bit
 * Size of DRAM data :16 bit
 * Size of word: 32 bit
 * length of DRAM Addr : 22 bit
 * length of CPU Addr : 21 bit
 * NOW, DO NOT CARE ready bit of DRAM !! wtime must be correct.

 Arguments
=================
 * lineBits : bit size of lines

 */
class Cache (numOfWay : Int,lineBits : Int,wtime : Int) extends Module {
  val io = new Bundle {
    val cmdin = Decoupled (new Bundle {
      val we = Bool (INPUT);
      val addr = UInt (width = 21);
    }).flip;
    val cmdout = Decoupled (new Bundle {
      val we = Bool (INPUT);
      val addr = UInt (width = 22);
    });
    val wdataFromCore = Valid(UInt(width = 16)).flip;
    val rdataFromCore = Valid(UInt(width = 16));
    val wdataToDRAM = Valid(UInt(width = 32));
    val rdataToDRAM = Valid(UInt(width = 32)).flip;
  }
  val time = UInt(wtime, log2Up(wtime) + 1)
  val wordsPerLine = 4;

  // addr width
  val tagWidth   = 21 - lineBits - 2;
  val indexWidth = lineBits;
  val wordsWidth = 2;

  // splited addr 
  val tag        = Reg(UInt (width = tagWidth))
  val index      = Reg(UInt (width = indexWidth))
  val posInWords = Reg(UInt (width = wordsWidth))

  val numOfLine = 1 << lineBits

  val cacheSize = numOfLine * numOfWay;

  val cache = Mem(UInt(width = 32), cacheSize);
  val tagArray = Mem(UInt (width = tagWidth), cacheSize);

  val busy :: ready :: cacheStates = Enum (UInt(),2)
  val state   = Reg (init = ready)
  val count   = Reg (init = time)
  val wordCnt = Reg (init = UInt(0))
  val addr    = Reg (init = UInt(0));
  val we      = Reg (init = Bool(false))

  def isHit : Bool = {
    for (i <- 0 until wordsPerLine - 1) {
      if (tag == tagArray (index << wordsWidth + i)) {
        return Bool(true)
      }
    }
    return Bool(false)
  }

  io.cmdout.valid := Bool(false);
  io.cmdout.bits.addr := addr;
  io.cmdout.bits.we := we;
  io.rdataFromCore.bits := UInt (0);
  io.rdataFromCore.valid := Bool(false);
  io.wdataToDRAM.bits := UInt (0);
  io.wdataToDRAM.valid := Bool(false);

  switch (state) {
    is (ready) {
      when (io.cmdin.valid) {
        when (isHit) {
          when (io.cmdin.bits.we) {
            // write
            when (io.wdataFromCore.valid) {
              cache ((index << wordsWidth) + posInWords) := io.wdataFromCore.bits
            }
          } .otherwise {
            // read
            io.rdataFromCore.bits := cache ((index << wordsWidth) + posInWords)
            io.rdataFromCore.valid := Bool(true)
          }
        } .otherwise {
          state := busy
          count := time
          wordCnt := UInt(0)

          // burst access
          addr := io.cmdin.bits.addr ## UInt (0)
          we   := io.cmdin.bits.we

          tag        := io.cmdin.bits.addr.apply(20,indexWidth + wordsWidth);
          index      := io.cmdin.bits.addr.apply(indexWidth + wordsWidth - 1,wordsWidth);
          posInWords := io.cmdin.bits.addr.apply (wordsWidth - 1,0)

          io.cmdout.bits.addr := addr
          io.cmdout.bits.we   := we
          io.cmdout.valid     := Bool(true)
          when (we) {
            io.wdataToDRAM.bits := cache ((index << wordsWidth) ^ wordCnt)
            io.wdataToDRAM.valid := Bool(true)
          }
        }
      }
    }
    is (busy) {
      when (count === UInt (0)) {
        count := time
        when (we === Bool(false) && io.rdataToDRAM.valid) {
          when (wordCnt.apply (0) === UInt(0)) {
            cache ((index << wordsWidth) ^ wordCnt).apply (15,0) := io.rdataToDRAM.bits
          }.otherwise{
            cache ((index << wordsWidth) ^ wordCnt).apply (31,16) := io.rdataToDRAM.bits
          }
        }

        when (wordCnt === UInt(7)) {
          state := ready
          wordCnt := UInt(0)
          when (we) {
            when (io.wdataFromCore.valid) {
              cache ((index << wordsWidth) + posInWords) := io.wdataFromCore.bits
            }
          }.otherwise {
            io.rdataFromCore.bits := cache ((index << wordsWidth) + posInWords)
            io.rdataFromCore.valid := Bool(true)
          }
        } .otherwise {
          wordCnt := wordCnt + UInt (1);
        }

        when (we) {
          when (wordCnt.apply (0) === UInt(0)) {
            io.wdataToDRAM.bits := cache ((index << wordsWidth) ^ wordCnt).apply(15,0)
          } .otherwise {
            io.wdataToDRAM.bits := cache ((index << wordsWidth) ^ wordCnt).apply(31,16)
          }
          io.wdataToDRAM.valid := Bool(true)
        }
      } .otherwise {
        count := count - UInt (1);
      }
    }
  }

  io.cmdin.ready := (state === ready);
}

object Cache {
  def main (args : Array[String]) : Unit = {
    chiselMainTest (args, () => Module (new Cache (16,16,2))) { c =>
      new CacheTests (c)
    }
  }

  class CacheTests (c: Cache) extends Tester (c,isTrace = false) {
    // Not yet
  }
}
