`include "global_config.vh"

module flash_led #(
    parameter NumOfLeds = 1,
    parameter FlashTimesWidth = 6,
    parameter CNT_LED_WIDTH = 24,
    parameter PERIODS_OF_LED_FLASH = `PERIODS_OF_1S / 10
) (
    input clk,
    input rst_n,
    input clr_n,
    //! should be the specified flash times multiplies 2
    input [FlashTimesWidth-1:0] flash_times_x2,
    output reg [NumOfLeds-1:0] led_n,
    output reg flash_over
);
  reg [  CNT_LED_WIDTH-1:0] cnt_led;  //! time counter for LED
  reg [FlashTimesWidth-1:0] cnt_flash_times;  //! counts for how many times LED has flashed * 2

  always @(posedge clk, negedge rst_n) begin : flash_led_proc
    if (rst_n == 0) begin
      cnt_led <= PERIODS_OF_LED_FLASH;
      cnt_flash_times <= 0;
      led_n <= {NumOfLeds{1'b1}};
      flash_over <= 1;
    end else begin
      if (clr_n == 0) begin
        cnt_flash_times <= flash_times_x2;
        cnt_led <= PERIODS_OF_LED_FLASH;
      end else begin
        // logic for cnt_flash_times
        if (cnt_flash_times != 0) begin
          flash_over <= 0;
          if (cnt_led == 0) begin
            cnt_flash_times <= cnt_flash_times - 1;
            cnt_led <= PERIODS_OF_LED_FLASH;
          end else begin
            cnt_led <= cnt_led - 1;
          end
        end else begin
          flash_over <= 1;
        end

        // logic for led
        if (cnt_led == 0) begin
          led_n <= ~led_n;
          if (cnt_flash_times == 0) led_n <= {NumOfLeds{1'b1}};
        end
      end
    end
  end

endmodule
