// 模块说明
// 功能：UART发送功能
module uart_byte_tx(
    input [7:0] data_byte,
    input rst_n,
    input en_send,
    input clk,
    input [2:0] set_baud,
    output reg tx,
    output reg tx_done,
    output reg uart_state  // 1表示正在传输
);

reg [19:0] cnt_max;  // 用于设置bit发送的周期
reg [19:0] cnt;
reg [3:0] cnt_bit;  // bit发送所用的周期信号，cnt值每过cnt_max次计数后会使cnt_bit加一

always @(*) begin
    case (set_baud)
        0: cnt_max = 20832 / 100;
        1: cnt_max = 10415 / 100;
        2: cnt_max = 5207 / 100;
        default: cnt_max = 5207 / 100;
    endcase
end

// cnt计数器，仅当en=1时计数、en=0时清零
always @(posedge clk, negedge rst_n)
if (!rst_n)
     cnt <= 0;
else if (en_send)
    if (cnt == cnt_max)
        cnt <= 0;
    else
        cnt <= cnt + 1'd1;
else
    cnt <= 0;

// cnt_bit计数器，周期是cnt的cnt_max倍
always @(posedge clk, negedge rst_n)
if (!rst_n) begin
    cnt_bit <= 0;
    tx_done <= 0;
    uart_state <= 0;
end
else if (en_send) begin
    if (cnt == 1) begin  // 每次cnt为1时使得cnt_bit加一，这样就可以在cnt开始计时的下一个周期就马上使得cnt_bit变化
        // 花了2h调试的一个坑：cnt_bit需要计数11次，而不是10次；如果是10次的话，1是开头（0没法做开头、因为初始值就是0），0是结尾，如果在0的末尾、生成tx_done脉冲，那么0的末尾就是1的开头，这样就会在一开头就生成一个tx_done脉冲，这样就不是想要的结果了。所以我们需要多计数一次，以给开头和结尾留一个周期的空，这样就可以在这个周期里发送tx_done了。
        if (cnt_bit == 10) begin
            cnt_bit <= 0;
            tx_done <= 1;
            uart_state <= 0;
        end
        else begin
            cnt_bit <= cnt_bit + 1'd1;
            tx_done <= 0;
            uart_state <= 1;
        end

    end
    else begin
        cnt_bit <= cnt_bit;
        tx_done <= 0;
        uart_state <= uart_state;
    end
end
else begin
    cnt_bit <= 0;
    tx_done <= 0;
    uart_state <= 0;
end

always @(posedge clk, negedge rst_n)
if (!rst_n) begin
    tx <= #1 0;
end
else if (en_send) begin
    case (cnt_bit)
        1: tx <= #1 0;
        2: tx <= #1 data_byte[0];
        3: tx <= #1 data_byte[1];
        4: tx <= #1 data_byte[2];
        5: tx <= #1 data_byte[3];
        6: tx <= #1 data_byte[4];
        7: tx <= #1 data_byte[5];
        8: tx <= #1 data_byte[6];
        9: tx <= #1 data_byte[7];
        10: tx <= #1 1;
        default: tx <= #1 tx;
    endcase
end
else begin
    tx <= tx;
end

endmodule
