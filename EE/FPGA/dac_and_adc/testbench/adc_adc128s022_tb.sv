module adc_adc128s022_tb;

  // Parameters
  localparam unsigned DivCntParam = 4;
  localparam unsigned ClkPeriod = 10;

  // Ports
  reg clk = 0;
  reg rst_n = 0;
  reg receiving_start = 0;
  reg receiving_stop = 0;
  wire receiving_done;
  wire [11:0] data;
  reg [2:0] addr;
  wire adc_cs_n;
  wire adc_sclk;
  wire adc_din;
  reg adc_dout = 0;

  reg [11:0] mock_data[5];
  initial begin
    mock_data[0] = 12'b0101_1010_1111;
    mock_data[1] = 12'b1111_1111_1111;
    mock_data[2] = 12'b1111_1100_0000;
    mock_data[3] = 12'b1111_0000_0000;
    mock_data[4] = 12'b1100_0000_0000;
  end

  adc_adc128s022 #(
      .DivCntParam(DivCntParam)
  ) adc_adc128s022_dut (
      .clk(clk),
      .rst_n(rst_n),
      .receiving_start(receiving_start),
      .receiving_stop(receiving_stop),
      .receiving_done(receiving_done),
      .data(data),
      .addr(addr),
      .adc_cs_n(adc_cs_n),
      .adc_sclk(adc_sclk),
      .adc_din(adc_din),
      .adc_dout(adc_dout)
  );

  initial begin
    rst_n = 0;
    #(ClkPeriod) rst_n = 1;

    receiving_start = 1;
    addr = 3'b101;
    #(ClkPeriod) receiving_start = 0;

    for (int j = 0; j < 5; j++) begin
      repeat (4) @(negedge adc_sclk);
      for (int i = 11; i >= 0; i--) begin
        @(negedge adc_sclk) adc_dout = mock_data[j][i];
      end
    end

    #(20*ClkPeriod);
    receiving_stop = 1;
    #(ClkPeriod) receiving_stop = 0;

    #(100*ClkPeriod);
    $stop;
  end

  always #(ClkPeriod / 2) clk = !clk;

endmodule
