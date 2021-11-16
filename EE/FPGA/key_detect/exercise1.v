`include "global_config.vh"

module exercise1 #(
    parameter CNT_WIDTH = 32
) (
    input clk,
    input rst_n,
    input key,
    output wire [4-1:0] led_
);

  localparam FlashTimesWidth = 6;

  reg en_cnt;
  reg [CNT_WIDTH-1:0] cnt;

  wire press_down, press_up;
  reg clr1_n, clr2_n;
  reg [6-1:0] flash_times_x2;
  wire flash_over1, flash_over2;

  key_detect key_detect_inst (
      .key_n(key),
      .clk(clk),
      .rst_n(rst_n),
      .press_down(press_down),
      .press_up(press_up)
  );

  flash_led #(
      .NumOfLeds(1),
      .FlashTimesWidth(FlashTimesWidth)
  ) flash_led_inst1 (
      .clk(clk),
      .rst_n(rst_n),
      .clr_n(clr1_n),
      .flash_times_x2(flash_times_x2),
      .led_n(led_[0]),
      .flash_over(flash_over1)
  );
  flash_led #(
      .NumOfLeds(3),
      .FlashTimesWidth(FlashTimesWidth)
  ) flash_led_inst2 (
      .clk(clk),
      .rst_n(rst_n),
      .clr_n(clr2_n),
      .flash_times_x2(flash_times_x2),
      .led_n(led_[3:1]),
      .flash_over(flash_over2)
  );

  always @(posedge clk, negedge rst_n) begin : main_proc
    if (rst_n == 0) begin
      en_cnt <= 0;
      clr1_n <= 1;
      clr2_n <= 1;
      flash_times_x2 <= 0;
      // led_ <= 4'b1111;
    end else begin
      if (press_down == 1) begin
        en_cnt <= 1;
      end else if (press_up == 1) begin
        en_cnt <= 0;
        // led_   <= ~led_;
        if (flash_over1 && flash_over2) begin
          if (cnt < `PERIODS_OF_1S) begin
            flash_times_x2 <= 5 * 2;
            clr1_n <= 0;
          end else if (cnt < 2 * `PERIODS_OF_1S) begin
            flash_times_x2 <= 10 * 2;
            clr1_n <= 0;
          end else begin
            flash_times_x2 <= 20 * 2;
            clr1_n <= 0;
            clr2_n <= 0;
          end
        end
      end else begin
        clr1_n <= 1;
        clr2_n <= 1;
      end
    end
  end

  always @(posedge clk, negedge rst_n) begin : cnt_loop_proc
    if (rst_n == 0) begin
      cnt <= 0;
    end else if (en_cnt == 1) begin
      if (cnt < 4294967295) begin
        cnt <= cnt + 1;
      end
    end else begin
      cnt <= 0;
    end
  end

endmodule
