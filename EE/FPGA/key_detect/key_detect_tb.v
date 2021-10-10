module key_detect_tb;
reg clk;
reg rst_n;
wire [3:0] led;
reg key;

key_detect key_detect (
    .rst_n (rst_n),
    .clk (clk),
    .key (key),
    .led (led)
);

localparam CLK_PERIOD = 10;
initial clk = 0;
always #(CLK_PERIOD/2) clk=~clk;

initial begin
    rst_n = 0;
    #(CLK_PERIOD);
    rst_n = 1;

    repeat (18)
        press_key();
    $stop;
end

integer rand;
task press_key;
begin
    key = 1;

    repeat(10) begin
        rand = {$random} % (2 * CLK_PERIOD);
        #(rand) key = ~key;
    end

    key = 0;
    #(CLK_PERIOD * 100); // wait to be detected

    repeat(10) begin
        rand = {$random} % (2 * CLK_PERIOD);
        #rand key = ~key;
    end

    key = 1;

    #(CLK_PERIOD * 200); // wait to be detected
end
endtask

endmodule
