module key_detect (
    input key_n,
    input clk,
    input rst_n,
    output reg press_down,
    output reg press_up
);

  localparam unsigned Idle = 2'b00, WaitDown = 2'b01, Down = 2'b10, WaitUp = 2'b11;
  reg [ 1:0] state;

  reg [19:0] cnt;
  reg en_cnt, cnt_full;

  reg key_nq1;
  reg key_nq2;
  reg key_nq3;
  reg key_nq4;
  always @(posedge clk) key_nq1 <= key_n;
  always @(posedge clk) key_nq2 <= key_nq1;
  always @(posedge clk) key_nq3 <= key_nq2;
  always @(posedge clk) key_nq4 <= key_nq3;

  wire p_edge, n_edge;
  assign p_edge = !key_nq4 && key_nq3;
  assign n_edge = key_nq4 && !key_nq3;

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      state <= Idle;
      en_cnt <= 1'b0;
      press_down <= 1'b0;
      press_up <= 1'b0;
    end else begin
      press_up   <= 1'b0;
      press_down <= 1'b0;
      case (state)
        Idle: begin
          if (n_edge) begin
            state  <= WaitDown;
            en_cnt <= 1'b1;
          end
        end
        WaitDown: begin
          if (p_edge) begin
            state  <= Idle;
            en_cnt <= 1'b0;
          end else if (cnt_full) begin
            state <= Down;
            en_cnt <= 1'b0;
            // key is pressed down
            press_down <= 1'b1;
          end
        end
        Down: begin
          if (p_edge) begin
            state  <= WaitUp;
            en_cnt <= 1'b1;
          end
        end
        WaitUp: begin
          if (n_edge) begin
            state  <= Down;
            en_cnt <= 1'b0;
          end else if (cnt_full) begin
            state <= Idle;
            en_cnt <= 1'b0;
            // key is correctly detected here
            press_up <= 1'b1;
          end
        end
        default: begin
          state <= Idle;
          en_cnt <= 1'b0;
          press_down <= 1'b0;
          press_up <= 1'b0;
        end
      endcase
    end
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      cnt <= 1'b0;
      cnt_full <= 1'b0;
    end else if (en_cnt) begin
      cnt <= cnt + 20'd1;
      if (cnt == 20'd100_000 - 20'd2) cnt_full <= 1'b1;
    end else begin
      cnt <= 1'b0;
      cnt_full <= 1'b0;
    end
  end

endmodule
