module uart_byte_tx_tb;

reg [7:0] data_byte;
reg rst_n, en, clk;
reg [2:0] set_baud;
wire tx, tx_done;

uart_byte_tx uart_byte_tx(
    .data_byte(data_byte),
    .rst_n(rst_n),
    .en_send(en),
    .clk(clk),
    .set_baud(set_baud),
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
    set_baud = 0;
    data_byte = 8'b0000_1111;
    @(posedge tx_done);
    en = 0;
    #5000;

    en = 1;
    set_baud = 1;
    data_byte = 8'b1010_1010;
    @(posedge tx_done);
    en = 0;
    #5000;

    en = 1;
    set_baud = 2;
    data_byte = 8'b1110_1110;
    @(posedge tx_done);
    en = 0;
    #5000;

    $stop;
end

endmodule
