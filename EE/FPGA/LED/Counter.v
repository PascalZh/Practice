module Counter (
           clk,
           rst,
           cnt_value
       );
input clk;
input rst;
output [3:0] cnt_value;

reg [3:0] cnt;

always @(negedge rst or posedge clk)
    if (!rst)
        cnt <= 4'b0;
    else if (cnt == 10)
        cnt <= 4'b0;
    else
        cnt <= cnt + 4'b1;

assign cnt_value = cnt;
endmodule