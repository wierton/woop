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
  val log_MemMux = true
  val log_rf = true
  val log_IFU = true
  val log_IFUPipelineData = true
  val log_BRIDU = true
  val log_PRALU = true
  val log_PRU = true
  val log_ALU = true
  val log_LSU = false
  val log_LSMDU = false
  val log_CrossbarNx1 = true
  val log_Cistern = true
}
