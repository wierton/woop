package woop
package configs

import chisel3._

object conf {
  val ADDR_WIDTH = 29
  val DATA_WIDTH = 32
  val INIT_PC = "hbfc00000".U
  val MUL_STAGES = 7
  val DIV_STAGES = 45
  val INSTR_ID_SZ = 8

  val mio_cycles = 3
  val icache_stages = 2
  val random_delay = true

  val nICacheSets = 256
  val nICacheWaysPerSet = 4
  val nICacheWordsPerWay = 16
  val nDCacheSets = 256
  val nDCacheWays = 4
  val nDCacheWayBytes = 16
  val nSimICacheEntries = 256

  val AXI4_DATA_WIDTH = 32
  val AXI4_ID_WIDTH = 3
  val AXI4_BURST_LENGTH = 32
  val AXI4_BURST_BYTES = AXI4_DATA_WIDTH / 8

  val TLB_BITS = 5
  val TLBSZ = (1 << TLB_BITS)
  val PABITS = 32

  val log_Top = false
  val log_MemMux = false
  val log_IMemCistern = false
  val log_CrossbarNx1 = false
  val log_Cistern = false
  val log_SimICache = false
  val log_DeviceAccessor = false
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
