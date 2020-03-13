package woop
package core

import chisel3._
import chisel3.util._
import woop.consts._
import woop.configs._
import woop.utils._
import woop.dumps._


class EHU extends Module {
  val io = IO(new Bundle {
  })

  io.ex_flush.valid := io.fu_in.ex.et =/= ET_None
  when (io.ex_flush.valid) {
    when (cpr_status.EXL === 0.U) {
      when (fu_in.wb.is_ds) {
        cpr_cause.BD := Y
        cpr_epc := fu_in.wb.pc - 4.U
      } .otherwise {
        cpr_cause.BD := N
        cpr_epc := fu_in.wb.pc
      }

      when (fu_in.ex.et === ET_TLB_REFILL) {
        offset := 0x000.U
      } .elsewhen(intr_valid && cpr_cause.IV.asBool) {
        offset := 0x200.U
      } .otherwise {
        offset := 0x180.U
      }
    } .otherwise {
      offset := 0x180.U
    }
    cpr_cause.ExcCode := MuxCase(0.U, Array(
      (fu_in.ex.et =/= ET_None) -> fu_in.ex.code,
      intr_valid -> EC_Int))

    when (fu_in.ex.et === ET_Eret) {
      when (cpr_status.ERL === 1.U) {
        cpr_status.ERL := 0.U
      } .otherwise {
        cpr_status.EXL := 0.U
      }
    } .otherwise {
      cpr_status.EXL := 1.U
    }

    when (fu_in.ex.et === ET_ADDR_ERR) {
      cpr_badvaddr := fu_in.ex.addr
    } .elsewhen(fu_in.ex.et === ET_TLB_Inv ||
      fu_in.ex.et === ET_TLB_Mod ||
      fu_in.ex.et === ET_TLB_REFILL) {
      val vaddr = fu_in.ex.addr
      cpr_badvaddr := vaddr
      cpr_context.badvpn2 := vaddr >> 13
      cpr_entry_hi.vpn := vaddr >> 13
      cpr_entry_hi.asid := fu_in.ex.asid
    }
  }
  io.ex_flush.bits.br_target := Mux(
    fu_in.ex.et === ET_Eret, cpr_epc,
    Mux(cpr_status.BEV === 1.U, "hbfc00200".U + offset,
      "h80000000".U + offset))
}
