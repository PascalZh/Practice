`timescale 1ns / 1ns

module dac_tlv5618_tb ();

  reg clk;
  reg rst_n;
  reg [15:0] data;
  reg sending_start;
  wire sending_done;

  wire dac_cs_n;
  wire dac_din;
  wire dac_sclk;

  dac_tlv5618 dac_tlv5618 (
      .clk  (clk),
      .rst_n(rst_n),

      .data(data),
      .sending_start(sending_start),
      .sending_done(sending_done),

      .dac_cs_n (dac_cs_n),
      .dac_din  (dac_din),
      .dac_sclk (dac_sclk)
  );

  initial clk = 1;
  always #10 clk = ~clk;

  initial begin
    rst_n = 0;
    sending_start = 0;
    data = 0;
    #201;
    rst_n = 1;
    #200;

    data = 16'hC_AAA;
    sending_start = 1;
    #20;
    sending_start = 0;
    #200;
    wait (sending_done);

    #20000;

    data = 16'h4_555;
    sending_start = 1;
    #20;
    sending_start = 0;
    #200;
    wait (sending_done);

    #20000;

    data = 16'h1_555;
    sending_start = 1;
    #20;
    sending_start = 0;
    #200;
    wait (sending_done);
    #20000;
    data = 16'hf_555;
    sending_start = 1;
    #20;
    sending_start = 0;
    #200;
    wait (sending_done);
    #20000;
    $stop;
  end

endmodule
