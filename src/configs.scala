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
  val nICacheSets = 32
  val nICacheWays = 4
  val nICacheWordsPerWay = 4
  val INSTR_ID_SZ = 8
  val log_MemMux = false
  val log_rf = false
  val log_IFU = false
  val log_IFUPipelineData = false
  val log_BRIDU = false
  val log_PRALU = false
  val log_PRU = false
  val log_ALU = false
  val log_LSU = false
  val log_LSMDU = false
  val log_CrossbarNx1 = false
  val log_Cistern = false
}
