module dac_and_adc_top (
    input clk,
    input rst_n,

    output tx,  //! uart tx

    output adc_cs_n,
    output adc_sclk,
    output adc_din,
    input adc_dout,

    input key_n,  //! press key to start dac
    output dac_cs_n,
    output dac_din,
    output dac_sclk
);

  wire tx_done;
  wire fifo_rdreq;
  wire [11:0] q;
  wire [7:0] uart_data;
  wire [11:0] adc_data;
  wire almost_empty, almost_full;
  wire uart_en_send;
  wire adc_receiving_start;
  wire adc_receiving_done;
  reg [2:0] adc_addr = 3'b0;
  wire start_all;

  ctrl_fifo2uart ctrl_fifo2uart_dut (
      .clk(clk),
      .rst_n(rst_n),
      .start_all(start_all),
      .fifo_rdreq(fifo_rdreq),
      .fifo_data(q),
      .fifo_almost_full(almost_full),
      .fifo_almost_empty(almost_empty),
      .uart_en_send(uart_en_send),
      .uart_data(uart_data),
      .uart_tx_done(tx_done),
      .adc_receiving_start(adc_receiving_start),
      .adc_receiving_done(adc_receiving_done)
  );

  uart_byte_tx uart_byte_tx_dut (
      .data_byte(uart_data),
      .rst_n(rst_n),
      .en_send(uart_en_send),
      .clk(clk),
      .set_baud(),
      .tx(tx),
      .tx_done(tx_done)
  );

  fifo fifo_dut (
      .clock(clk),
      .data(adc_data),
      .rdreq(fifo_rdreq),
      .wrreq(adc_receiving_done),
      .almost_empty(almost_empty),
      .almost_full(almost_full),
      .empty(),
      .full(),
      .q(q),
      .usedw()
  );

  adc_adc128s022 adc_adc128s022_dut (
      .clk(clk),
      .rst_n(rst_n),
      .receiving_start(adc_receiving_start),
      .receiving_done(adc_receiving_done),
      .data(adc_data),
      .addr(adc_addr),
      .adc_cs_n(adc_cs_n),
      .adc_sclk(adc_sclk),
      .adc_din(adc_din),
      .adc_dout(adc_dout)
  );

  test_dac_tlv5618 test_dac_tlv5618_dut (
      .clk(clk),
      .rst_n(rst_n),
      .key_n(key_n),
      .dac_cs_n(dac_cs_n),
      .dac_din(dac_din),
      .dac_sclk(dac_sclk)
  );

  key_detect key_detect_dut (
      .key_n(key_n),
      .clk(clk),
      .rst_n(rst_n),
      .press_down(),
      .press_up(start_all)
  );

endmodule
