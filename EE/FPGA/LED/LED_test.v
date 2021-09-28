`timescale 1ns/1ns
module LED_test;

reg key;
wire led;

reg clk;
reg en;
reg clr;
reg rst;
wire [3:0] cnt_value;

// LED led_m(
//         .key(key),
//         .led(led)
//     );

// Counter counter_m(
//             .clk(clk),
//             .rst(rst),
//             .cnt_value(cnt_value)
//         );

CounterWithClr cnt(
                   .clk(clk),
                   .en(en),
                   .clr(clr),
                   .cnt_value(cnt_value)
               );

initial clk = 0;
always #50 clk = ~clk;

initial begin
    clr = 0;
    en = 1;
    #400;
    clr = 1;
    #50;
    clr = 0;
    #400;
    en = 0;
    #100;
    en = 1;
    #100;
    $stop;
end

endmodule
