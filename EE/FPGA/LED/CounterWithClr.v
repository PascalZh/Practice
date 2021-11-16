module CounterWithClr (
           clk,
           clr,
           en,
           cnt_value
       );

input clk, clr, en;
output [3:0] cnt_value;
reg [3:0] cnt;

always @(posedge clk, posedge clr)
    if (clr)
        cnt <= 4'b0;
    else if (en)
        cnt <= cnt + 1;

assign cnt_value = cnt;

endmodule
