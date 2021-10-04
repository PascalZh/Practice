module LED (
           input clk,
           input rst,
           output reg led
       );

reg [24-1:0] cnt;

// initial begin
//     cnt = 24'b0;
// end

always @(posedge clk, posedge rst) begin
    if (rst)
        cnt <= 24'd0;
    else if (cnt == 1249999)
        cnt <= 24'd0;
    else
        cnt <= cnt + 24'b1;
end

// 控制LED循环闪动
always @(posedge clk, posedge rst) begin
    if (rst)
        led <= 1'd0;
    else if (cnt == 1249999)
        led <= ~led;
end

endmodule
