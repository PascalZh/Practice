module ser_data_sender(
    input [7:0] data,
    input rst_n,
    input en,
    input clk,
    output reg tx,
    output reg tx_done
);

reg [9-1:0] cnt;

// 计数器，仅当en=1时计数、en=0时清零
always @(posedge clk, negedge rst_n)
if (!rst_n)
     cnt <= 0;
else if (en)
    if (cnt == 399)
        cnt <= 0;
    else
        cnt <= cnt + 1;
else
    cnt <= 0;

always @(posedge clk, negedge rst_n)
if (!rst_n)
    tx_done <= 0;
else if (cnt == 399)
    tx_done <= 1;
else if (cnt == 0)
    tx_done <= 0;

always @(posedge clk, negedge rst_n)
if (!rst_n)
    tx <= 0;
else if (en) begin
    case (cnt)
        0: tx <= data[0];
        49: tx <= data[1];
        99: tx <= data[2];
        149: tx <= data[3];
        199: tx <= data[4];
        249: tx <= data[5];
        299: tx <= data[6];
        349: tx <= data[7];
        default: tx <= tx;
    endcase
end

endmodule
