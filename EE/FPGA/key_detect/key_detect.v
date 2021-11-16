module key_detect (
    input key_n,
    input clk,
    input rst_n,
    output reg press_down,
    output reg press_up
);

  reg [1:0] state;
  localparam Idle = 2'b00, WaitDown = 2'b01, Down = 2'b10, WaitUp = 2'b11;

  reg [19:0] cnt;
  reg en_cnt, cnt_full;

  reg key_sync1;
  reg key_sync2;
  reg key_synced_pre;
  reg key_synced;
  always @(posedge clk) key_sync1 <= key_n;
  always @(posedge clk) key_sync2 <= key_sync1;
  always @(posedge clk) key_synced <= key_sync2;
  always @(posedge clk) key_synced_pre <= key_synced;

  wire p_edge, n_edge;
  assign p_edge = !key_synced_pre && key_synced;
  assign n_edge = key_synced_pre && !key_synced;

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      state <= Idle;
      en_cnt <= 0;
      press_down <= 0;
      press_up <= 0;
    end else begin
      press_up   <= 0;
      press_down <= 0;
      case (state)
        Idle: begin
          if (n_edge) begin
            state  <= WaitDown;
            en_cnt <= 1;
          end
        end
        WaitDown: begin
          if (p_edge) begin
            state  <= Idle;
            en_cnt <= 0;
          end else if (cnt_full) begin
            state <= Down;
            en_cnt <= 0;
            // key is pressed down
            press_down <= 1;
          end
        end
        Down: begin
          if (p_edge) begin
            state  <= WaitUp;
            en_cnt <= 1;
          end
        end
        WaitUp: begin
          if (n_edge) begin
            state  <= Down;
            en_cnt <= 0;
          end else if (cnt_full) begin
            state <= Idle;
            en_cnt <= 0;
            // key is correctly detected here
            press_up <= 1;
          end
        end
        default: begin
          state <= Idle;
          en_cnt <= 0;
          press_down <= 0;
          press_up <= 0;
        end
      endcase
    end
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      cnt <= 0;
      cnt_full <= 0;
    end else if (en_cnt) begin
      cnt <= cnt + 1;
      if (cnt == 20'd100_000 - 2) cnt_full <= 1;
    end else begin
      cnt <= 0;
      cnt_full <= 0;
    end
  end

endmodule
