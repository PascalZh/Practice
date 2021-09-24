`timescale 1ns/1ns
module LED_test;

reg key;
wire led;

reg clk;
reg rst;
wire [3:0] cnt_value;

LED led_m(
        .key(key),
        .led(led)
    );

Counter counter_m(
            .clk(clk),
            .rst(rst),
            .cnt_value(cnt_value)
        );

initial clk = 0;
always #50 clk = ~clk;

initial begin
    key = 1;
    #200;
    key = 0;
    #200;
    key = 1;
    #200;

    rst = 0;
    #200
     rst = 1;
    #2000
     $stop;
end

endmodule
