`timescale 1ns/1ns
module ser_data_sender_test;

reg [8-1:0] data;
reg rst_n, en, clk;
wire tx, tx_done;

ser_data_sender sender(
    .data(data),
    .rst_n(rst_n),
    .en(en),
    .clk(clk),
    .tx(tx),
    .tx_done(tx_done)
);

initial clk = 1;
always #10 clk = ~clk;

initial begin
    rst_n = 0;
    #200;
    rst_n = 1;
    #200;

    en = 1;
    data = 8'b0000_1111;
    @(posedge tx_done);
    en = 0;
    #5000;

    en = 1;
    data = 8'b1010_1010;
    @(posedge tx_done);
    en = 0;
    #5000;

    en = 1;
    data = 8'b1110_1110;
    @(posedge tx_done);
    en = 0;
    #5000;

    $stop;
end

endmodule
