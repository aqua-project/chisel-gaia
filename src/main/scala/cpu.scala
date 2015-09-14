import Chisel._
import ALU._

class ICacheCmd extends Bundle {
  val req = new Bundle {
    val addr = UInt(OUTPUT, width = 32)
  }
  val res = new Bundle {
    val stall = Bool(INPUT)
    val data = UInt(INPUT, width = 32)
  }
}

class DCacheCmd extends Bundle {
  val req = new Bundle {
    val addr = UInt(OUTPUT, width = 32)
    val data = UInt(OUTPUT, width = 32)
    val we = Bool(OUTPUT)
    val re = Bool(OUTPUT)
    val ba = Bool(OUTPUT)
  }
  val res = new Bundle {
    val stall = Bool(INPUT)
    val data = UInt(INPUT, width = 32)
  }
}

class SystemBus extends Bundle {
  val int_go = Bool(INPUT)
  val int_cause = UInt(INPUT, width = 32)
  val eoi = Bool(OUTPUT)
  val eoi_id = UInt(OUTPUT, width = 32)
  val vmm_en = Bool(OUTPUT)
  val vmm_pd = UInt(OUTPUT, width = 32)
  val cai = Bool(OUTPUT)
}

class CPU extends Module {

  val op_alu = UInt(0)
  val op_ldl = UInt(2)
  val op_ldh = UInt(3)
  val op_jl = UInt(4)
  val op_jr = UInt(5)
  val op_ld = UInt(6)
  val op_ldb = UInt(7)
  val op_st = UInt(8)
  val op_stb = UInt(9)
  val op_sysenter = UInt(12)
  val op_sysexit = UInt(13)
  val op_bne = UInt(14)
  val op_beq = UInt(15)

  val io = new Bundle {
    val bus = new SystemBus
    val ic = new ICacheCmd
    val dc = new DCacheCmd
    val alu = ALUIO().flip // FIXME: io.alu.stall
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

  val w = Reg(new Bundle {
    val regfile = Vec(UInt(width = 32), 32)
  })

  val flag = Reg(new Bundle {
    val int_en = Bool()
    val int_pc = UInt(width = 32)
    val int_cause = UInt(width = 32)
    val int_hp = UInt(width = 32)
    val vmm_en = Bool()
    val vmm_pd = UInt(width = 32)
    val eoi = Bool()
  })


  def hazard_unit {
    val op = io.ic.res.data(31, 28)
    val reg_x = io.ic.res.data(27, 23)
    val reg_a = io.ic.res.data(22, 18)
    val reg_b = io.ic.res.data(17, 13)

    def dest_match (dest: UInt) =
      MuxCase(Bool(false), Seq(
        Vec(op_alu).contains(op) ->
          (dest === reg_a || dest === reg_b),
        Vec(op_st, op_stb, op_bne, op_beq).contains(op) ->
          (dest === reg_x || dest === reg_a),
        Vec(op_jr, op_ldh, op_ld, op_ldb).contains(op) ->
          (dest === reg_a)))

    stall := Bool(false)

    // load hazard
    when (d.mem_read && orR(d.reg_dest)) {
      stall := stall || dest_match(d.reg_dest)
    }

    // data hazard in branch unit
    when ((d.alu_write || d.misc_write) && orR(d.reg_dest)) {
      stall := stall || dest_match(d.reg_dest)
    }
    when (e.mem_read && orR(e.misc_dest)) {
      stall := stall || dest_match(e.misc_dest)
    }

    // trap hazard
    when (io.bus.int_go || Vec(op_sysenter, op_sysexit).contains(op)) {
      stall := stall || (d.mem_read || d.mem_write)
    }
  }

  def forward_unit {

    for (i <- 1 to 31) {
      fw.regfile(i) := Mux(m.reg_write && m.reg_dest === UInt(i),
        Mux(m.reg_mem, io.dc.res.data, m.res), w.regfile(UInt(i)))
    }

    def ex_forward (reg_src: UInt, reg_data: UInt) =
      MuxCase(reg_data, Seq(
        (e.misc_write && orR(e.misc_dest) && e.misc_dest === reg_src) ->
          e.misc_res,
        (e.alu_write && orR(e.alu_dest) && e.alu_dest === reg_src) ->
          io.alu.r,
        (m.reg_write && orR(m.reg_dest) && m.reg_dest === reg_src) ->
          Mux(m.reg_mem, io.dc.res.data, m.res)))

    def id_forward (reg_src: UInt) =
      MuxCase(fw.regfile(reg_src), Seq(
        (e.misc_write && orR(e.misc_dest) && e.misc_dest === reg_src) ->
          e.misc_res,
        (e.alu_write && orR(e.alu_dest) && e.alu_dest === reg_src) ->
          io.alu.r))

    fw.ex_data_x := ex_forward(d.reg_dest, d.data_x)
    fw.ex_data_a := ex_forward(d.reg_a, d.data_a)
    fw.ex_data_b := ex_forward(d.reg_b, d.data_b)

    fw.id_data_x := id_forward(io.ic.res.data(27, 23))
    fw.id_data_a := id_forward(io.ic.res.data(22, 18))
    fw.id_data_b := id_forward(io.ic.res.data(17, 13))

    val e_data = e.misc_res

    fw.int_en := Mux(! (e.mem_write && e.mem_addr === UInt("h80001104")),
      flag.int_en, e_data(0))
    fw.int_pc := Mux(! (e.mem_write && e.mem_addr === UInt("h80001108")),
      flag.int_pc, e_data)
    fw.int_hp := Mux(! (e.mem_write && e.mem_addr === UInt("h80001100")),
      flag.int_hp, e_data)
  }

  def detect_branch {
    val op = io.ic.res.data(31, 28)

    val data_a = fw.id_data_a
    val data_x = fw.id_data_x

    d.pc_addr := PriorityMux(Seq(
      (op === op_jr) ->
        data_a,
      (op === op_sysenter) ->
        fw.int_hp,
      (op === op_sysexit) ->
        fw.int_pc,
      Vec(op_jl, op_bne, op_beq).contains(op) ->
        (f.nextpc + (io.ic.res.data(15, 0) << 2))))

    d.pc_src := MuxCase(Bool(false), Seq(
      Vec(op_jl, op_jr, op_sysenter, op_sysexit).contains(op) ->
        Bool(true),
      Vec(op_beq).contains(op) ->
        (data_x === data_a),
      Vec(op_bne).contains(op) ->
        (data_x != data_a)))
  }

  def trap_unit {
    val op = io.ic.res.data(31, 28)

    flag.eoi := Bool(false)

    unless (io.dc.res.stall || flag.eoi || stall || d.pc_src) {
      val nexteoi = (! flag.eoi && fw.int_en && io.bus.int_go)

      when (nexteoi) {
        flag.int_cause := io.bus.int_cause
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
          w.regfile(i) := Mux(m.reg_mem, io.dc.res.data, m.res)
        }
      }
    }
  }

  def memory_unit {
    val addr = e.mem_addr
    val data = e.misc_res
    val we = e.mem_write
    val re = e.mem_read
    val ba = e.mem_byte

    m.reg_write := e.alu_write || e.misc_write
    m.reg_dest := Mux(e.alu_write, e.alu_dest, e.misc_dest)
    m.reg_mem := e.mem_read && ! (UInt("h80001100") <= addr && addr <= UInt("h80002000"))
    m.res := Mux(e.alu_write, io.alu.r, e.misc_res)

    when (re) {
      switch (addr) {
        is(UInt("h80001100")) {
          m.res := flag.int_hp
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
          flag.int_hp := data
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

    when (io.dc.res.stall) {
      m.reg_write := Bool(false)
    }

    io.dc.req.addr := addr
    io.dc.req.data := data
    io.dc.req.we := we
    io.dc.req.re := re
    io.dc.req.ba := ba
    io.bus.vmm_en := flag.vmm_en
    io.bus.vmm_pd := flag.vmm_pd
    io.bus.eoi := flag.eoi
    io.bus.eoi_id := flag.int_cause
    io.bus.cai := (addr === UInt("h80001200") || addr === UInt("h80001204")) && we
  }

  def execute_unit {
    val data_a = fw.ex_data_a
    val data_b = fw.ex_data_b
    val data_x = fw.ex_data_x

    unless (io.dc.res.stall) {
      e.mem_addr := Mux(d.mem_byte, data_a + d.data_d, data_a + (d.data_d << 2))
      e.mem_write := d.mem_write
      e.mem_read := d.mem_read
      e.mem_byte := d.mem_byte

      // alu
      e.alu_write := d.alu_write
      e.alu_dest := d.reg_dest

      // misc
      e.misc_write := d.misc_write
      e.misc_dest := d.reg_dest

      e.misc_res := PriorityMux(Seq(
        Vec(op_ldl).contains(d.opcode) ->
          d.data_d,
        Vec(op_ldh).contains(d.opcode) ->
          Cat(d.data_d(15, 0), data_a(15, 0)),
        Vec(op_jl, op_jr).contains(d.opcode) ->
          d.nextpc,
        Vec(op_st, op_stb).contains(d.opcode) ->
          data_x))
    }

    when (! io.dc.res.stall && flag.eoi) { // flush operation if it came from *previous* ID
      e.alu_write := Bool(false)
      e.misc_write := Bool(false)
      e.mem_write := Bool(false)
      e.mem_read := Bool(false)
    }

    io.alu.optag := d.tag
    io.alu.a := data_a
    io.alu.b := data_b
    io.alu.l := d.data_l
  }

  def decode_unit {
    // val inst = UInt(0, width = 32)

    // // TODO: this circuit should be moved to icache
    // unless (io.ic.res.stall) {
    //   inst := Mux(f.inst_src, f.inst, io.ic.res.data)
    // }

    val op = io.ic.res.data(31, 28)

    // update even when io.dc.res.stall is active
    d.data_x := fw.id_data_x;
    d.data_a := fw.id_data_a;
    d.data_b := fw.id_data_b;

    unless (io.dc.res.stall) {
      d.opcode := op
      d.reg_dest := io.ic.res.data(27, 23)
      d.reg_a := io.ic.res.data(22, 18)
      d.reg_b := io.ic.res.data(17, 13)
      d.data_l := io.ic.res.data(12, 5)
      d.data_d := io.ic.res.data(15, 0)
      d.tag := io.ic.res.data(4, 0)

      d.nextpc := f.nextpc
      d.alu_write := op === op_alu
      d.misc_write := Vec(op_ldl, op_ldh, op_ld, op_ldb, op_jl, op_jr).contains(op)
      d.mem_write := Vec(op_st, op_stb).contains(op)
      d.mem_read := Vec(op_ld, op_ldb).contains(op)
      d.mem_byte := Vec(op_ldb, op_stb).contains(op)

      detect_branch
    }

    when ((! io.dc.res.stall && (stall || d.pc_src)) || flag.eoi) {
      d.alu_write := Bool(false)
      d.misc_write := Bool(false)
      d.mem_write := Bool(false)
      d.mem_read := Bool(false)
      d.pc_src := Bool(false)
    }
  }

  def fetch_unit {

    val pc = MuxCase(f.nextpc, Seq(
      flag.eoi ->
        flag.int_hp,
      d.pc_src ->
        d.pc_addr))

    unless (stall || io.dc.res.stall || io.ic.res.stall) {
      f.nextpc := pc + UInt(4)
    }

    io.ic.req.addr := pc

    // // TODO: this circuit should be moved to icache
    // f.inst_src := (! eoi && ! d.pc_src && (stall || io.dc.res.stall) && ! io.ic.res.stall)
    // f.inst := inst
  }


  val stall = Wire(Bool())

  val pc = Wire(UInt(width = 32))

  val fw = Wire(Some(new Bundle {
    val ex_data_x = UInt(width = 32)
    val ex_data_a = UInt(width = 32)
    val ex_data_b = UInt(width = 32)
    val id_data_x = UInt(width = 32)
    val id_data_a = UInt(width = 32)
    val id_data_b = UInt(width = 32)
    val int_en = Bool()
    val int_pc = UInt(width = 32)
    val int_hp = UInt(width = 32)
    val regfile = Vec(UInt(width = 32), 32)
  }))

  write_unit

  memory_unit

  forward_unit

  execute_unit

  hazard_unit

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
