module Counter (
    clk,
    rst_n,
    cnt_value
);
  input clk;
  input rst_n;
  output [3:0] cnt_value;

  reg [3:0] cnt;

  always @(negedge rst_n or posedge clk)
    if (!rst_n) cnt <= 4'b0;
    else if (cnt == 10) cnt <= 4'b0;
    else cnt <= cnt + 1;

  assign cnt_value = cnt;
endmodule
