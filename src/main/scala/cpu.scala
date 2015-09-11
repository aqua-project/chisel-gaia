import Chisel._

class CPU extends Module {

  val io = new Bundle {

  }

  val f = Reg(new Bundle {
    val nextpc = UInt(width = 32)
  })

  val d = Reg(new Bundle {
    val opcode = UInt(width = 4)
    val reg_dest = UInt(width = 5)
    val reg_a = UInt(width = 5)
    val reg_b = UInt(width = 5)
    val data_x = UInt(width = 32)
    val data_a = UInt(width = 32)
    val data_b = UInt(width = 32)
    val data_l = UInt(width = 8)
    val data_d = UInt(width = 32)
    val tag = UInt(width = 5)
    val nextpc = UInt(width = 32)
    val alu_write = Bool()
    val misc_write = Bool()
    val mem_write = Bool()
    val mem_read = Bool()
    val mem_byte = Bool()
    val pc_addr = UInt(width = 32)
    val pc_src = Bool()
  })

  val e = Reg(new Bundle {
    val alu_dest = UInt(width = 5)
    val alu_write = Bool()
    val misc_res = UInt(width = 32)
    val misc_dest = UInt(width = 5)
    val misc_write = Bool()
    val mem_addr = UInt(width = 32)
    val mem_write = Bool()
    val mem_read = Bool()
    val mem_byte = Bool()
  })

  val m = Reg(new Bundle {
    val res = UInt(width = 32)
    val reg_dest = UInt(width = 5)
    val reg_write = Bool()
    val reg_mem = Bool()
  })

  val flag = Reg(new Bundle {
    val int_en = Bool()
    val int_pc = UInt(width = 32)
    val int_cause = UInt(width = 32)
    val int_handler = Uint(width = 32)
    val vmm_en = Bool()
    val vmm_pd = Bool()
    val eoi = Bool()
  })


  def hazard_unit {
    val op = io.i_data(31, 28)
    val reg_x = io.i_data(27, 23)
    val reg_a = io.i_data(22, 18)
    val reg_b = Mux(op === op_alu, io.i_data(17, 13), UInt(0))

    stall := Bool(false)

    // load hazard
    when (op === op_st || op === op_stb) {
      when (d.mem_read && d.reg_dest && (d.reg_dest === reg_x || d.reg_dest === reg_a)) {
        stall := Bool(true)
      }
    } .otherwise {
      when (d.mem_read && d.reg_dest && (d.reg_dest === reg_a || d.reg_dest === reg_b)) {
        stall := Bool(true)
      }
    }

    // branch hazard
    switch (op) {
      is(op_bne, op_beq) {
        when (d.alu_write && d.reg_dest && (d.reg_dest === reg_x || d.reg_dest === reg_a)) {
          stall := Bool(true)
        }
        when (d.misc_write && d.reg_dest && (d.reg_dest === reg_x || d.reg_dest === reg_a)) {
          stall := Bool(true)
        }

        when (e.mem_read && e.misc_dest && (e.misc_dest === reg_x || e.misc_dest === reg_a)) {
          stall := Bool(true)
        }
      }
      is(op_jr) {
        when (d.alu_write && d.reg_dest && d.reg_dest === reg_a) {
          stall := Bool(true)
        }
        when (d.misc_write && d.reg_dest && d.reg_dest === reg_a) {
          stall := Bool(true)
        }

        when (e.mem_read && e.misc_dest && e.misc_dest === reg_a) {
          stall := Bool(true)
        }
      }
      is(op_sysenter, op_sysexit) {
        when (d.mem_write || d.mem_read) {
          stall := Bool(true)
        }
      }
    }

    // trap hazard
    when (io.int_go) {
      when (d.mem_write || d.mem_read) {
        stall := Bool(true)
      }
    }
  }

  def detect_branch (rf: Vec, int_hdr: UInt, int_pc: UInt, pc_src: Bool, pc_addr: UInt) {
    val op = io.i_data(31, 28)
    val reg_x = io.i_data(27, 23)
    val reg_a = io.i_data(22, 18)

    def forward (reg: UInt, data: UInt) {
      // perform forwarding, but results may be wrong...
      when (e.misc_write && e.misc_dest && e.misc_dest === reg) {
        data := e.misc_res
      } .elsewhen (e.alu_write && e.alu_dest && e.alu_dest === reg) {
        data := io.alu_res
      } .otherwise {
        data := rf(reg)
      }
    }

    forward(reg_x, data_x)
    forward(reg_a, data_a)

    switch (op) {
      is (op_jl, op_bne, op_beq) {
        pc_addr := f.nextpc + (io.i_data(15, 0) << 2)
      }
      is (op_jr) {
        pc_addr := data_a
      }
      is (op_sysenter) {
        pc_addr := int_hdr
      }
      is (op_sysexit) {
        pc_addr := int_pc
      }
    }

    pc_src := UInt(0)

    switch (op) {
      is (op_jl, op_jr, op_sysenter, op_sysexit) {
        pc_src := Bool(true)
      }
      is (op_bne) {
        pc_src := data_x != data_a
      }
      is (op_beq) {
        pc_src := data_x === data_a
      }
    }
  }

  def trap_unit (int_en: Bool) {
    val op = io.i_data(31, 28)

    flag.eoi := Bool(false)

    unless (io.d_stall || flag.eoi || stall || d.pc_src) {
      val nexteoi = (! flag.eoi && int_en && io.int_go)

      when (nexteoi) {
        flag.int_cause := io.int_cause
        flag.int_pc := f.nextpc
        flag.int_en := Bool(false)
      } .elsewhen (op === op_sysenter) {
        flag.int_cause := UInt("h00000003")
        flag.int_pc := f.nextpc
        flag.int_en := Bool(false)
      } .elsewhen (op === op_sysexit) {
        flag.int_en := Bool(true)
      }

      flag.eoi := nexteoi
    }
  }

  def write_unit {
    when (m.reg_write) {
      for (i <- 1 to 31) {
        when (m.reg_dest === UInt(i)) {
          regfile(UInt(i)) := Mux(m.reg_mem, io.d_data, m.res)
        }
      }
    }
  }

  def memory_unit {
    val addr = e.mem_addr
    val data = e.misc_res
    val we = e.mem_write
    val re = e.mem_read

    m.reg_write := e.alu_write || e.misc_write

    when (e.alu_write) {
      m.res := io.alu_res
      m.reg_dest := e.alu_dest
    } .elsewhen (e.misc_write) {
      m.res := e.misc_res
      m.reg_dest := e.misc_dest
    }

    when (UInt("h80001100") <= addr && addr <= UInt("h80002000")) {
      m.reg_mem := Bool(false)
    } .otherwise {
      m.reg_mem := e.mem_read
    }

    when (re) {
      switch (addr) {
        is(UInt("h80001100")) {
          m.res := flag.int_handler
        }
        is(UInt("h80001104")) {
          m.res := flag.int_en
        }
        is(UInt("h80001108")) {
          m.res := flag.int_pc
        }
        is(UInt("h8000110C")) {
          m.res := flag.int_cause
        }
        is(UInt("h80001200")) {
          m.res := flag.vmm_en
        }
        is(UInt("h80001204")) {
          m.res := flag.vmm_pd
        }
      }
    }

    when (we) {
      switch (addr) {
        is(UInt("h80001100")) {
          flag.int_handler := data
        }
        is(UInt("h80001104")) {
          flag.int_en := data(0)
        }
        is(UInt("h80001108")) {
          flag.int_pc := data
        }
        is(UInt("h8000110C")) {
          flag.int_cause := data
        }
        is(UInt("h80001200")) {
          flag.vmm_en := data(0)
        }
        is(UInt("h80001204")) {
          flag.vmm_pd := data
        }
      }
    }

    cai := (addr === UInt("h80001200") || addr == UInt("h80001204")) && we

    when (io.d_stall) {
      m.reg_write := Bool(false)
    }
  }

  def execute_unit {

    def data_forward (reg_src: UInt, reg_data: UInt, res: UInt) {
      when (e.misc_write && e.misc_dest && e.misc_dest === reg_src) {
        res := e.misc_res
      } .elsewhen (e.alu_write && e.alu_dest && e.alu_dest === reg_src) {
        res := io.alu_res
      } .elsewhen (m.reg_write && m.reg_dest && m.reg_dest === reg_src) {
        res := Mux(m.reg_mem, io.d_data, m.res)
      } .otherwise {
        res := reg_data
      }
    }

    data_forward(d.reg_a, d.data_a, data_a)
    data_forward(d.reg_b, d.data_b, data_b)
    data_forward(d.reg_dest, d.data_x, data_x)

    unless (io.d_stall) {
      e.mem_addr := Mux(d.mem_byte, data_a + d.data_d, data_a + (d.data_d << 2))
      e.mem_write := d.mem_write
      e.mem_read := d.mem_read
      e.mem_byte := d.mem_byte

      // alu
      e.alu_write := d.alu_write
      e.alu_dest := d.alu_dest

      // misc
      switch (d.opcode) {
        is(op_ldl) {
          e.misc_res := d.data_d
        }
        is(op_ldh) {
          e.misc_res := Cat(d.data_d(15, 0), data_a(15, 0))
        }
        is(op_jl, op_jr) {
          e.misc_res := d.nextpc
        }
        is(op_st, op_stb) {
          e.misc_res := data_x
        }
      }
      e.misc_write := d.misc_write
      e.misc_dest := d.reg_dest
    }

    when (! io.d_stall && flag.eoi) { // flush operation if it came from *previous* ID
      e.alu_write := Bool(false)
      e.misc_write := Bool(false)
      e.mem_write := Bool(false)
      e.mem_read := Bool(false)
    }
  }

  def decode_unit {
    // val inst = UInt(0, width = 32)

    // // TODO: this circuit should be moved to icache
    // unless (io.i_stall) {
    //   inst := Mux(f.inst_src, f.inst, io.i_data)
    // }

    val op = io.i_data(31, 28)

    unless (io.d_stall) {
      d.opcode := op
      d.reg_dest := io.i_data(27, 23)
      d.reg_a := io.i_data(22, 18)
      d.reg_b := io.i_data(17, 13)
      d.data_l := io.i_data(12, 5)
      d.data_d := io.i_data(15, 0)
      d.tag := io.i_data(4, 0)

      d.nextpc := f.nextpc
      d.alu_write := op === op_alu
      d.misc_write := Vec(op_ldl, op_ldh, op_ld, op_ldb, op_jl, op_jr).contains(op)
      d.mem_write := Vec(op_st, op_stb).contains(op)
      d.mem_read := Vec(op_ld, op_ldb).contains(op)
      d.mem_byte := Vec(op_ldb, op_stb).contains(op)

      detect_branch
    }

    when ((! io.d_stall && (stall || d.pc_src)) || flag.eoi) {
      d.alu_write := Bool(false)
      d.misc_write := Bool(false)
      d.mem_write := Bool(false)
      d.mem_read := Bool(false)
      d.pc_src := Bool(false)
    }

    d.data_x := ;
    d.data_a := ;
    d.data_b := ;
  }

  def fetch_unit {
    val pc = UInt(0, width = 32)

    when (eoi) {
      pc := flag.int_handler
    } .elsewhen (d.pc_src) {
      pc := d.pc_addr
    } .otherwise {
      pc := f.nextpc
    }

    unless (stall || io.d_stall || io.i_stall) {
      f.nextpc := pc + UInt(4)
    }

    // // TODO: this circuit should be moved to icache
    // f.inst_src := (! eoi && ! d.pc_src && (stall || io.d_stall) && ! io.i_stall)
    // f.inst := inst
  }

  val stall = Bool(false)

  hazard_unit

  write_unit

  memory_unit

  execute_unit

  decode_unit

  fetch_unit

  trap_unit
}

class CPUTests(c: CPU) extends Tester(c) {
  step(1)
}

object CPU {
  def main(args: Array[String]) {
    chiselMainTest(args, () => Module(new CPU)) { c =>
      new CPUTests(c)
    }
  }
}
