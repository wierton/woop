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
  val axi_id_width = 3
  val mio_cycles = 3
  val icache_stages = 2
  val mul_stages = 7
  val div_stages = 45
  val random_delay = true
  val INSTR_ID_SZ = 8

  val nICacheSets = 256
  val nICacheWays = 4
  val nICacheWayBytes = 16
  val nDCacheSets = 256
  val nDCacheWays = 4
  val nDCacheWayBytes = 16

  val nSimICacheEntries = 256

  val TLB_BITS = 5
  val tlbsz = (1 << TLB_BITS)
  val PABITS = 32

  val log_Top = false
  val log_MemMux = false
  val log_IMemCistern = false
  val log_CrossbarNx1 = false
  val log_Cistern = false
  val log_SimICache = true
  val log_DeviceAccessor = true
  val log_rf = false
  val log_IFU = false
  val log_IMemPipe = false
  val log_IDU = false
  val log_BRU = false
  val log_ISU = false
  val log_EXU = false
  val log_PRU = false
  val log_ALU = false
  val log_LSU = false
  val log_MDU = false
  val log_CP0 = false
  val log_EHU = false
  val log_TLB = false
  val log_TLB_ENTRY = false
  val log_MSU = false
}
