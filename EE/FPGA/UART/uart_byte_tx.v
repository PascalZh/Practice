//! 功能：UART发送功能
module uart_byte_tx (
    input [7:0] data_byte,
    input rst_n,
    input en_send,
    input clk,
    input [2:0] set_baud,
    output reg tx,
    output reg tx_done,
    output reg uart_state  //! 1表示正在传输
);

  reg [19:0] cnt_max;  // 用于设置bit发送的周期
  reg [19:0] cnt;
  reg [3:0] cnt_bit;  // bit发送所用的周期信号，cnt值每过cnt_max次计数后会使cnt_bit加一

  always @(*) begin
    case (set_baud)
      0: cnt_max = 20'd5208 - 20'd1;
      1: cnt_max = 20'd2604 - 20'd1;
      2: cnt_max = 20'd1302 - 20'd1;
      3: cnt_max = 20'd868 - 20'd1;
      4: cnt_max = 20'd434 - 20'd1;
      default: cnt_max = 20'd5208 - 20'd1;
    endcase
  end

  // cnt计数器，仅当en=1时计数、en=0时清零
  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) cnt <= 0;
    else if (en_send)
      if (cnt == cnt_max) cnt <= 0;
      else cnt <= cnt + 20'd1;
    else cnt <= 0;
  end

  // cnt_bit计数器，周期是cnt的cnt_max倍
  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      cnt_bit <= 0;
      tx_done <= 0;
      uart_state <= 0;
    end else if (en_send) begin
      if (cnt == cnt_max) begin
        if (cnt_bit == 9) begin
          cnt_bit <= 0;
          tx_done <= 1;
          uart_state <= 0;
        end else begin
          cnt_bit <= cnt_bit + 4'd1;
          tx_done <= 0;
          uart_state <= 1;
        end
      end else begin
        cnt_bit <= cnt_bit;
        tx_done <= 0;
        uart_state <= uart_state;
      end
    end else begin
      cnt_bit <= 0;
      tx_done <= 0;
      uart_state <= 0;
    end
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      tx <= #1 1;
    end else if (en_send) begin
      case (cnt_bit)
        0: tx <= #1 0;
        1: tx <= #1 data_byte[0];
        2: tx <= #1 data_byte[1];
        3: tx <= #1 data_byte[2];
        4: tx <= #1 data_byte[3];
        5: tx <= #1 data_byte[4];
        6: tx <= #1 data_byte[5];
        7: tx <= #1 data_byte[6];
        8: tx <= #1 data_byte[7];
        9: tx <= #1 1;
        default: tx <= #1 tx;
      endcase
    end else begin
      tx <= 1;
    end
  end

endmodule
