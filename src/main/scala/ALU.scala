import Chisel._

class ALUIO extends Bundle {
  val stall = Bool(INPUT)
  val optag = UInt(INPUT, width = 5)
  val a = UInt(INPUT, width = 32)
  val b = UInt(INPUT, width = 32)
  val l = UInt(INPUT, width = 8)
  val r = UInt(OUTPUT, width = 32)
}

object ALUIO {
  def apply(): ALUIO = new ALUIO
}

class ALU extends Module {

  val op_add = UInt(0)
  val op_sub = UInt(1)
  val op_shl = UInt(2)
  val op_shr = UInt(3)
  val op_sar = UInt(4)
  val op_and = UInt(5)
  val op_ior = UInt(6)
  val op_xor = UInt(7)
  val op_adda = UInt(8)
  val op_cmpult = UInt(22)
  val op_cmpule = UInt(23)
  val op_cmpne = UInt(24)
  val op_cmpeq = UInt(25)
  val op_cmplt = UInt(26)
  val op_cmple = UInt(27)

  val io = new ALUIO

  val x = io.a
  val y = (io.b.toSInt + io.l.toSInt).toUInt
  val z = Reg(init = UInt(0, width = 32))

  io.r := z

  unless (io.stall) {
    switch (io.optag) {
      is(op_add) {
        z := x + y
      }
      is(op_sub) {
        z := x - y
      }
      is(op_shl) {
        z := x << y;
      }
      is(op_shr) {
        z := x >> y;
      }
      is(op_sar) {
        z := x.toSInt >> y;
      }
      is(op_and) {
        z := x & y
      }
      is(op_ior) {
        z := x | y
      }
      is(op_xor) {
        z := x ^ y
      }
      is(op_adda) {
        z := x + (y << 2)
      }
      is(op_cmpult) {
        z := x < y
      }
      is(op_cmpule) {
        z := x <= y
      }
      is(op_cmpne) {
        z := (x != y)
      }
      is(op_cmpeq) {
        z := (x === y)
      }
      is(op_cmplt) {
        z := (x.toSInt < y.toSInt)
      }
      is(op_cmple) {
        z := (x.toSInt <= y.toSInt)
      }
    }
  }
}

class ALUTests(c: ALU) extends Tester(c) {
  poke(c.io.stall, 0)

  step(1)

  def test(op: UInt, a: Int, b: Int, l: Int, r: Int) {
    poke(c.io.optag, op.litValue())
    poke(c.io.a, a)
    poke(c.io.b, b)
    poke(c.io.l, l)
    step(1)
    expect(c.io.r, r)
  }

  test(c.op_add, 1, 2, 0, 3)
  test(c.op_add, 1, 2, 3, 6)
  test(c.op_add, 1, 2, -1, 2)

  test(c.op_sub, 1, 2, 0, -1)
  test(c.op_sub, 10, 9, 0, 1)
  test(c.op_sub, 1, 2, 3, -4)
  test(c.op_sub, 1, 2, -1, 0)
}

object ALU {
  def main(args: Array[String]) {
    chiselMainTest(args, () => Module(new ALU)) { c =>
      new ALUTests(c)
    }
  }
}
