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
  val INSTR_ID_SZ = 8

  val nICacheSets = 64
  val nICacheWays = 4
  val ICacheDataW   = 64
  val nICacheWordsPerWay = 4
  val nDCacheSets = 64
  val nDCacheWays = 4
  val DCacheDataW = 64
  val nDCacheWordsPerWay = 4
  val tlbsz = 32

  val log_Top = true
  val log_MemMux = true
  val log_CrossbarNx1 = true
  val log_Cistern = true
  val log_rf = true
  val log_IFU = true
  val log_IFUPipelineData = true
  val log_BRIDU = true
  val log_PRALU = true
  val log_PRU = true
  val log_ALU = true
  val log_LSU = true
  val log_MDU = true
  val log_LSMDU = true
}
