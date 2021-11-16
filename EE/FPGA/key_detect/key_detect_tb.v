`timescale 1ns / 1ns
module key_detect_tb;
  reg clk;
  reg rst_n;
  wire press_down, press_up;
  reg key;
  reg [40:0] test;

  key_detect key_detect (
      .rst_n(rst_n),
      .clk(clk),
      .key(key),
      .press_down(press_down),
      .press_up(press_up)
  );

  localparam unsigned ClkPeriod = 10;
  initial clk = 0;
  always #(ClkPeriod / 2) clk = ~clk;

  initial begin
    rst_n = 0;
    test  = 0;
    #(ClkPeriod);
    rst_n = 1;
    key   = 1;
    #(1000 * ClkPeriod);
    test = 1;

    repeat (18) begin
      press_key();
      test = 2;
    end
    test = 0;
    #(500 * ClkPeriod);
    $stop;
  end

  integer rand_time;
  task automatic press_key;
    begin
      key = 1;

      repeat (10) begin
        rand_time = $urandom % (2 * ClkPeriod);
        #(rand_time) key = ~key;
      end

      key = 0;
      #(ClkPeriod * 100);  // wait to be detected

      repeat (10) begin
        rand_time = $urandom % (2 * ClkPeriod);
        #rand_time key = ~key;
      end

      key = 1;

      #(ClkPeriod * 200);  // wait to be detected
    end
  endtask

endmodule
