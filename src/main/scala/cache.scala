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
 * wayBits : bit size of way
 * lineBits : bit size of lines

 */
class Cache (wayBits : Int,lineBits : Int,wtime : Int) extends Module {
  val io = new Bundle {
    val pohe = UInt (OUTPUT);
    val hyoe = UInt (OUTPUT);
    val d_wdata_from_core = UInt (OUTPUT);
    val foo = UInt (OUTPUT);
    val bar = UInt (OUTPUT);
    val cmdin = Decoupled (new Bundle {
      val we = Bool (INPUT);
      val addr = UInt (width = 21);
    }).flip;
    val cmdout = Decoupled (new Bundle {
      val we = Bool (INPUT);
      val addr = UInt (width = 22);
    });
    val wdataFromCore = Valid(UInt(width = 32)).flip;
    val rdataFromCore = Valid(UInt(width = 32));
    val wdataToDRAM = Valid(UInt(width = 16));
    val rdataToDRAM = Valid(UInt(width = 16)).flip;
  }
  val time = UInt(wtime-1, log2Down(wtime-1) + 1)
  val wordsPerLine = 4;

  // addr width
  val tagWidth   = 21 - lineBits - 2;
  val indexWidth = lineBits;
  val wordsWidth = 2;

  // splited addr 
  val tag        = Reg(UInt (width = tagWidth))
  val index      = Reg(UInt (width = indexWidth))
  val posInLine = Reg(UInt (width = wordsWidth))

  val numOfLine = 1 << lineBits
  val numOfWay  = 1 << wayBits

  val cacheSize = numOfLine * numOfWay;

  val cache = Mem(UInt(width = 32), cacheSize * wordsPerLine);
  val tagArray = Mem(UInt (width = tagWidth), cacheSize);
  val nextWay = Mem(UInt(width = log2Up (numOfWay) + 1), numOfLine);

  val busy :: ready :: cacheStates = Enum (UInt(),2)
  val state   = Reg (init = ready)
  val count   = Reg (init = time)
  val wordCnt = Reg (init = UInt(0,3))
  val addr    = Reg (init = UInt(0,22));
  val we      = Reg (init = Bool(false))

  val wdata_from_core = Reg (init = UInt (0,32));
  io.d_wdata_from_core := wdata_from_core;

  val dram_buff = Reg (init = Decoupled(UInt(width = 16)));

  val core_buff = Reg (init = Decoupled(UInt(width = 32)));

  io.cmdout.valid := (state === busy);
  io.cmdout.bits.addr := addr;
  io.cmdout.bits.we := we;
  io.rdataFromCore := core_buff;
  io.wdataToDRAM := dram_buff;

  val pohe = Reg (init = UInt(0));
  val hyoe = Reg (init = UInt(0));
  val foo = Reg (init = UInt(0));
  val bar = Reg (init = UInt(0));


  io.pohe := pohe;
  io.hyoe := hyoe;
  io.foo := foo;
  io.bar := bar;

  def hitWay (varTag : UInt, varIndex : UInt) : UInt = {
    for (i <- 0 until numOfWay ) {
      when (varTag === tagArray ((varIndex << wayBits) + UInt(i))) {
        return UInt(i);
      }
    }
    return UInt (numOfWay);
  }

  switch (state) {
    is (ready) {
      when (io.cmdin.valid) {
        val varIndex = io.cmdin.bits.addr.apply(indexWidth + wordsWidth - 1,wordsWidth);
        val varTag = io.cmdin.bits.addr.apply(20,indexWidth + wordsWidth);
        val nowWay = Mux(varTag === tagArray((varIndex << wayBits) + UInt(0)),UInt (0),
          Mux (varTag === tagArray((varIndex << wayBits) + UInt(1)),UInt (1),UInt(2)))
//        val nowWay = hitWay (varTag,varIndex);
        // for (i <- 0 to numOfWay - 1) {
        //     when (varTag === tagArray ((varIndex << wayBits) + UInt(i))) {
        //       nowWay = i
        //     }
        // }
        // when (varTag === tagArray((varIndex << wayBits) + UInt(0))) {
        //   nowWay = UInt(0);
        // } .elsewhen (varTag === tagArray((varIndex << wayBits) + UInt(1))) {
        //     nowWay = UInt(1);
        // } .otherwise {
        //     nowWay = UInt(2);
        // }

        pohe := nowWay;
        hyoe := varTag;
        foo  := tagArray((varIndex << wayBits) + UInt(0));
        bar  := tagArray((varIndex << wayBits) + UInt(1));

        when (nowWay != UInt(numOfWay)) {
          val tmp_posInLine = io.cmdin.bits.addr.apply (wordsWidth - 1,0)
          dram_buff.valid := Bool (false)
          when (io.cmdin.bits.we) {
            // write
            when (io.wdataFromCore.valid) {
              cache ((varIndex << (wayBits + lineBits)) + (nowWay << lineBits) + tmp_posInLine) := io.wdataFromCore.bits
            }
          } .otherwise {
            // read
            core_buff.bits := cache ((varIndex << (wayBits + lineBits)) + (nowWay << lineBits) + tmp_posInLine)
            core_buff.valid := Bool(true)
          }
        } .otherwise {
          core_buff.valid := Bool (false)
          state := busy
          count := time
          wordCnt := UInt(0)

          // burst access
          when (io.cmdin.bits.we) {
            // tag index words
            val tmp_index = io.cmdin.bits.addr.apply(indexWidth + wordsWidth - 1,wordsWidth)
            addr := tagArray ((tmp_index << wayBits) + nextWay (tmp_index)) ## io.cmdin.bits.addr.apply(indexWidth + wordsWidth - 1,0) ## UInt (0)
          }.otherwise {
            addr := io.cmdin.bits.addr ## UInt (0)
          }
          we   := io.cmdin.bits.we

          tag        := io.cmdin.bits.addr.apply(20,indexWidth + wordsWidth);
          index      := io.cmdin.bits.addr.apply(indexWidth + wordsWidth - 1,wordsWidth);
          posInLine := io.cmdin.bits.addr.apply (wordsWidth - 1,0)

          when (io.cmdin.bits.we) {
            dram_buff.bits := cache ((varIndex << (wayBits + lineBits)) + (nextWay (index) << lineBits) + UInt(0))
            dram_buff.valid := Bool(true)
            wdata_from_core := io.wdataFromCore.bits;
          }
        }
      }
    }
    is (busy) {
      when (count === UInt (0)) {
        count := time
        when (we === Bool(false) && io.rdataToDRAM.valid) {
          when (wordCnt (0) === UInt (0)) {
            cache ((index << (wayBits + lineBits)) + (nextWay (index) << lineBits) + wordCnt (wordsWidth,1)).apply(15,0) := io.rdataToDRAM.bits
          }.otherwise {
            cache ((index << (wayBits + lineBits)) + (nextWay (index) << lineBits) + wordCnt (wordsWidth,1)).apply(31,16) := io.rdataToDRAM.bits
          }
        }

        // finish
        when (wordCnt === UInt(7)) {
          when (we) {
            cache ((index << (wayBits + lineBits)) + (nextWay (index) << lineBits) + posInLine) := wdata_from_core
          }.otherwise {
            core_buff.bits := cache ((index << (wayBits + lineBits)) + (nextWay (index) << lineBits) + posInLine);
            core_buff.valid := Bool(true)
          }
          state := ready
          wordCnt := UInt(0)
          tagArray ((index << wayBits) + nextWay (index)) := tag
          when (nextWay (index) === UInt(numOfWay - 1)) {
            nextWay (index) := UInt (0)
          }.otherwise {
            nextWay (index) := nextWay (index) + UInt (1);
          }
        } .otherwise {
          wordCnt := wordCnt + UInt (1);
        }

        when (we) {
          when (wordCnt (0) === UInt (0)) {
            dram_buff.bits := cache ((index << (wayBits + lineBits)) + (nextWay (index) << lineBits) + wordCnt (wordsWidth,1)).apply(15,0)
          }.otherwise {
            dram_buff.bits := cache ((index << (wayBits + lineBits)) + (nextWay (index) << lineBits) + wordCnt (wordsWidth,1)).apply(31,16)
          }
          dram_buff.valid := Bool(true)
        }
      } .otherwise {
        count := count - UInt (1);
      }
    }
  }

  io.cmdin.ready := (state === ready);
}

object Cache {
  val wayBits = 1;
  val lineBits = 5;
  val wtime = 2;

  def main (args : Array[String]) : Unit = {
    chiselMainTest (args, () => Module (new Cache (wayBits,lineBits,wtime))) { c =>
      new CacheTests (c)
    }
  }

  class CacheTests (c: Cache) extends Tester (c) {

    def readTest (addr: Int,value: Int,expectMiss: Boolean) {
      print ("start of read test\n");
      while (peek (c.io.cmdin.ready) == 0) {
        step (1);
      }
      poke (c.io.cmdin.bits.we,false);
      poke (c.io.cmdin.bits.addr,addr);
      poke (c.io.cmdin.valid,true);
      step (1);
      pohe ()
      if (!expectMiss) {
        expect (c.io.cmdin.ready,1)
        expect (c.io.rdataFromCore.valid,1)
        expect (c.io.rdataFromCore.bits,value)
        print ("end of read test\n");
        return
      }
      expect (c.io.cmdout.valid,1);
      expect (c.io.cmdout.bits.we,0);
      expect (c.io.cmdout.bits.addr,addr);
      for (i <- 0 to 3) {
        // TODO: In this test,timing is critical
        // low bit
        step (wtime)
        poke (c.io.rdataToDRAM.bits,value & ((1 << 16) - 1));
        poke (c.io.rdataToDRAM.valid,true);

        // high bit
        step (wtime);
        poke (c.io.rdataToDRAM.valid,true);
        poke (c.io.rdataToDRAM.bits,value >> 16);
      }

      expect (c.io.rdataFromCore.valid,1)
      expect (c.io.rdataFromCore.bits,value)
      print ("end of read test\n");
    }

    def writeTest (addr: Int,value: Int,expectMiss: Boolean) {
      print ("start of write test\n");
      while (peek (c.io.cmdin.ready) == 0) {
        step (1);
      }
      poke (c.io.cmdin.bits.we,true);
      poke (c.io.cmdin.bits.addr,addr);
      poke (c.io.cmdin.valid,true);
      poke (c.io.wdataFromCore.valid,true)
      poke (c.io.wdataFromCore.bits,value)

      step (1);
      pohe ()
      if (!expectMiss) {
        expect (c.io.cmdin.ready,1)
        print ("end of write test\n");
        return
      }
      expect (c.io.cmdout.valid,1);
      expect (c.io.cmdout.bits.we,1);
      printf ("cmdout addr:%d\n",peek (c.io.cmdout.bits.addr));

      for (i <- 0 to 3) {
        // low bit
        printf ("wdata to DRAM:%d\n",peek(c.io.wdataToDRAM.bits));

        expect (c.io.wdataToDRAM.valid,1);
        step (wtime)

        // high bit
        printf ("wdata to DRAM:%d\n",peek(c.io.wdataToDRAM.bits));
        expect (c.io.wdataToDRAM.valid,1);
        step (wtime);
      }
      print ("end of write test\n");
    }

    // expect 1024 and 1025 are in the same line
    writeTest (1024,1234567890,true);
    readTest  (1024,1234567890,false);
    writeTest (1025,1234567890,false);
    readTest  (1025,1234567890,false);

    // expect working as 2 nowWay
    writeTest (1024,234567890,false);
    writeTest (2048,123456789,true);
    readTest  (2048,123456789,false);
    readTest  (1024,234567890,false);
    def pohe () {
      printf ("pohe:%d\n",peek (c.io.pohe));
      peek (c.io.hyoe);
      peek (c.io.foo);
      peek (c.io.bar);
      if (peek (c.io.pohe) == 0) {
        print("POHE!\n");
      } else {
        print("HYOE\n");
      }
    }
  }
}
