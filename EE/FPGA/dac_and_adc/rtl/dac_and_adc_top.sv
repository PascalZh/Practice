module dac_and_adc_top (
    input  clk,
    input  rst_n,
    output tx,

    output reg adc_cs_n,
    output reg adc_sclk,
    output reg adc_din,
    input adc_dout,
    input [2:0] adc_addr
);

  reg tx_done;
  reg rdreq;
  reg wrreq;
  reg [11:0] fifo_data;
  reg [7:0] data_byte;
  reg [11:0] adc_data;
  wire empty, full;
  wire not_full = ~full;
  reg en_send;

  ctrl_fifo2uart ctrl_fifo2uart_dut (
      .clk(clk),
      .rst_n(rst_n),
      .fifo_rdreq(rdreq),
      .fifo_data(fifo_data),
      .fifo_full(full),
      .fifo_empty(empty),
      .uart_en_send(en_send),
      .uart_data(data_byte),
      .uart_tx_done(tx_done)
  );

  uart_byte_tx uart_byte_tx_dut (
      .data_byte(data_byte),
      .rst_n(rst_n),
      .en_send(en_send),
      .clk(clk),
      .set_baud(),
      .tx(tx),
      .tx_done(tx_done),
      .uart_state()
  );

  fifo fifo_dut (
      .clock(clk),
      .data(adc_data),
      .rdreq(rdreq),
      .wrreq(wrreq),
      .almost_empty(),
      .almost_full(),
      .empty(empty),
      .full(full),
      .q(fifo_data),
      .usedw()
  );

  adc_adc128s022 adc_adc128s022_dut (
      .clk(clk),
      .rst_n(rst_n),
      .receiving_start(not_full),
      .receiving_stop(full),
      .receiving_done(wrreq),
      .data(adc_data),
      .addr(adc_addr),
      .adc_cs_n(adc_cs_n),
      .adc_sclk(adc_sclk),
      .adc_din(adc_din),
      .adc_dout(adc_dout)
  );


endmodule
