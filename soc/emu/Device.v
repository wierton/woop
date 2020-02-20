import "DPI-C" function void device_io
(
  input  bit  in_req_valid,
  input  int  in_req_bits_addr,
  input  int  in_req_bits_data,
  input  byte in_req_bits_func,
  input  byte in_req_bits_wstrb,
  output int  in_resp_bits_data
);

module SimDev(
  input         clock,
  input         reset,
  output        in_req_ready,
  input         in_req_valid,
  input  [0:0]  in_req_bits_is_cached,
  input  [0:0]  in_req_bits_is_aligned,
  input  [31:0] in_req_bits_addr,
  input  [31:0] in_req_bits_data,
  input  [0:0]  in_req_bits_func,
  input  [3:0]  in_req_bits_wstrb,
  input         in_resp_ready,
  output        in_resp_valid,
  output [31:0] in_resp_bits_data
);

// delayed inputs
wire #0.1 __in_req_valid = in_req_valid;
wire [31:0] #0.1 __in_req_bits_addr = in_req_bits_addr;
wire [31:0] #0.1 __in_req_bits_data = in_req_bits_data;
wire [7:0] #0.1 __in_req_bits_func = {7'b0, in_req_bits_func};
wire [7:0] #0.1 __in_req_bits_wstrb = {4'b0, in_req_bits_wstrb};

int __in_resp_bits_data;
assign #0.1 in_resp_bits_data = resp_data;

reg resp_valid;
reg [31:0] resp_data;
// we are always ready
assign in_req_ready = !resp_valid;
// resp will be valid next cycle
assign in_resp_valid = resp_valid;

always @(posedge clock)
begin
  if (!reset) begin
    device_io(
      __in_req_valid,
      __in_req_bits_addr,
      __in_req_bits_data,
      __in_req_bits_func,
      __in_req_bits_wstrb,
      __in_resp_bits_data
    );
    if (__in_req_valid && in_req_ready) // in_req_fire
      resp_valid <= 1'b1;
    else if (resp_valid && in_resp_ready) // in_resp_fire
      resp_valid <= 1'b0;
    resp_data <= __in_resp_bits_data;
  end
end

always @(posedge clock)
begin
  if (reset) begin
    resp_valid <= 1'b0;
  end
end
endmodule
