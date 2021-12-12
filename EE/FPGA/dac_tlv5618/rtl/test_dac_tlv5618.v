module test_dac_tlv5618 (
    input clk,
    input rst_n,

    input key_n,

    output dac_cs_n,
    output dac_din,
    output dac_sclk
);
  localparam unsigned DivCntMax = 2;
  reg [7:0] address;
  reg sending_start;
  wire sending_done;
  wire press_down, press_up;
  wire [15:0] data;

  rom_data rom_data_dut (
      .address(address),
      .clock(clk),
      .q(data)
  );

  key_detect key_detect_dut (
      .key_n(key_n),
      .clk(clk),
      .rst_n(rst_n),
      .press_down(press_down),
      .press_up(press_up)
  );

  dac_tlv5618 #(
      .DivCntMax(DivCntMax)
  ) dac_tlv5618_dut (
      .clk(clk),
      .rst_n(rst_n),
      .sending_start(sending_start),
      .sending_done(sending_done),
      .data(data),
      .dac_cs_n(dac_cs_n),
      .dac_din(dac_din),
      .dac_sclk(dac_sclk)
  );

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      address <= 8'b0;
      sending_start <= 1'b0;
    end else begin
      if (press_up || sending_done) sending_start <= 1'b1;
      else sending_start <= 1'b0;

      if (sending_done) begin
        if (address == 8'd255) address <= 8'd0;
        else address <= address + 1'b1;
      end
    end
  end

endmodule
