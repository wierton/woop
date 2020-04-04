package woop
package configs

import chisel3._

object conf {
  val xprlen = 32
  val addr_width = 32
  val data_width = 32
  val xprbyte = xprlen / 8
  val start_addr = "hbfc00000".U
  val axi_data_width = 32
  val axi_id_width = 4
  val mio_cycles = 3
  val icache_stages = 10
  val INSTR_ID_SZ = 8

  val nICacheSets = 256
  val nICacheWays = 4
  val nICacheWayBytes = 16
  val nDCacheSets = 256
  val nDCacheWays = 4
  val nDCacheWayBytes = 16

  val TLB_BITS = 5
  val tlbsz = (1 << TLB_BITS)
  val PABITS = 32

  val log_Top = true
  val log_MemMux = true
  val log_IMemCistern = true
  val log_CrossbarNx1 = true
  val log_Cistern = true
  val log_SimICache = true
  val log_rf = true
  val log_IFU = true
  val log_IMemPipe = true
  val log_IDU = true
  val log_BRU = true
  val log_ISU = true
  val log_EXU = true
  val log_PRU = true
  val log_ALU = true
  val log_LSU = true
  val log_MDU = true
  val log_CP0 = true
  val log_TLB = true
  val log_LSMDU = true
}
