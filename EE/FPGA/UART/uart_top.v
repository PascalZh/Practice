module uart_top (
    input  clk,
    input  rst_n,
    input  key_n,
    output tx
);

  wire press_down, press_up;
  reg key_state;
  reg en_send;
  wire tx_done;
  reg [7:0] data_byte;

  key_detect key_detect_dut (
      .key_n(key_n),
      .clk(clk),
      .rst_n(rst_n),
      .press_down(press_down),
      .press_up(press_up)
  );

  uart_byte_tx uart_byte_tx_dut (
      .data_byte(data_byte),
      .rst_n(rst_n),
      .en_send(en_send),
      .clk(clk),
      .set_baud(3'h0),
      .tx(tx),
      .tx_done(tx_done),
      .uart_state()
  );

  issp u0 (
      .source(data_byte)  // sources.source
  );

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      en_send <= 0;
    end else if (key_state) begin
      en_send <= 1;
    end else if (!key_state && tx_done) begin
      en_send <= 0;
    end
  end
  
  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      key_state <= 0;
    end else if (press_down) begin
      key_state <= 1;
    end else if (press_up) begin
      key_state <= 0;
    end
  end

endmodule
