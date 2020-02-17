package MipsNPC
package ModuleIO

import chisel3._
import chisel3.util._

import Configure._
import ModuleConsts._
import MemConsts._


class FlushIO extends Bundle {
    val br_target = Output(UInt(conf.xprlen.W));
}

class BypassIO extends Bundle {
    val wen = Output(Bool()); // need bypass
    val reg_dest_idx = Output(UInt(REG_SZ.W));
    val data = Output(UInt(conf.xprlen.W));
}

class WriteBackIO extends BypassIO {
    val npc = Output(UInt(conf.xprlen.W));
}

class IFU_IDU_IO extends Bundle {
	val npc = Output(UInt(conf.xprlen.W));
	val instr = Output(UInt(conf.xprlen.W));
}

class IDU_ISU_IO extends Bundle {
	val npc = Output(UInt(conf.xprlen.W));
	val instr = Output(UInt(conf.xprlen.W));
	val fu_type = Output(UInt(FU_TYPE_SZ.W))
	val fu_op = Output(UInt(FU_OP_SZ.W));
	val op1_sel = Output(UInt(OP1_SEL_SZ.W));
	val op2_sel = Output(UInt(OP2_SEL_SZ.W));
	val dest_sel = Output(UInt(DEST_SEL_SZ.W));
}

//========================================================
//         ISU --> {ALU, LSU, MDU, CMOVU, BRU}
//========================================================
class ISU_ALU_IO extends Bundle {
	val fu_op = Output(UInt(FU_OP_SZ.W));
    val npc = Output(UInt(conf.xprlen.W)); // deleted by dce
	val op1 = Output(UInt(conf.xprlen.W));
	val op2 = Output(UInt(conf.xprlen.W));
	val reg_dest_idx = Output(UInt(REG_SZ.W));
}

class ISU_MDU_IO extends ISU_ALU_IO { }

class ISU_BRU_IO extends Bundle {
	val fu_op = Output(UInt(FU_OP_SZ.W));
	val npc = Output(UInt(conf.xprlen.W)); // PC next
	val rs_data = Output(UInt(conf.xprlen.W));
	val rt_data = Output(UInt(conf.xprlen.W));
	val addr = Output(UInt(ADDR_SZ.W));
	val se_off = Output(UInt(conf.xprlen.W));
	val reg_dest_idx = Output(UInt(REG_SZ.W));
}

class ISU_LSU_IO extends Bundle {
	val fu_op = Output(UInt(FU_OP_SZ.W));
    val npc = Output(UInt(conf.xprlen.W)); // deleted by dce
	val base = Output(UInt(conf.xprlen.W));
	val offset = Output(UInt(conf.xprlen.W));
	val data = Output(UInt(conf.xprlen.W));
	val reg_dest_idx = Output(UInt(REG_SZ.W));
}

class ISU_CMOVU_IO extends ISU_ALU_IO { }


//========================================================//
//        {ALU, LSU, MDU, CMOVU, BRU}  -->  WBU           //
//========================================================//
class LSU_WBU_IO extends Bundle {
    val npc = Output(UInt(conf.xprlen.W)); // deleted by dce
	val need_wb = Output(Bool());
	val reg_dest_idx = Output(UInt(REG_SZ.W));
	val data = Output(UInt(conf.xprlen.W));
}

class ALU_WBU_IO extends Bundle {
    val npc = Output(UInt(conf.xprlen.W)); // deleted by dce
	val reg_dest_idx = Output(UInt(REG_SZ.W));
	val data = Output(UInt(conf.xprlen.W));
}

class MDU_WBU_IO extends Bundle {
    val npc = Output(UInt(conf.xprlen.W)); // deleted by dce
	val need_wb = Output(Bool());
	val reg_dest_idx = Output(UInt(REG_SZ.W));
	val data = Output(UInt(conf.xprlen.W));
}

class BRU_WBU_IO extends Bundle {
    val npc = Output(UInt(conf.xprlen.W)); // deleted by dce
	val need_br = Output(Bool());
	val br_target = Output(UInt(conf.xprlen.W));
	val need_wb = Output(Bool());
	val reg_dest_idx = Output(UInt(REG_SZ.W));
	val data = Output(UInt(conf.xprlen.W));
}

class CMOVU_WBU_IO extends Bundle {
    val npc = Output(UInt(conf.xprlen.W));
	val need_wb = Output(Bool());
	val reg_dest_idx = Output(UInt(REG_SZ.W));
	val data = Output(UInt(conf.xprlen.W));
}

//========================================================//
//               xxx --> GPIO --> xxx                     //
//========================================================//
class GPIO_OUT extends Bundle {
    val halted = Output(Bool());
    val code = Output(UInt(conf.xprlen.W));
    val reason = Output(UInt(conf.xprlen.W));
}

class IDU_GPIO_IO extends Bundle {
    val halted = Input(Bool());
    val invalid_instr = Output(Bool());
    val npc = Output(UInt(conf.xprlen.W));
    val instr = Output(UInt(conf.xprlen.W));
}

class CROSSBAR_GPIO_IO extends Bundle {
    val invalid_address = Output(Bool());
    val addr = Output(UInt(conf.xprlen.W));
}
