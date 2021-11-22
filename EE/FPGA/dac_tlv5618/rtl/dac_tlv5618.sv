module dac_tlv5618 #(
    parameter unsigned DivCntParam = 4  //! divide frequency ratio, maximum value is limited by `div_cnt`
) (
    input clk,
    input rst_n,
    input sending_start,
    input [16-1:0] data,
    output reg sending_done,

    output reg dac_cs_n,
    output reg dac_din,
    output reg dac_sclk
);

  reg en;
  //! start to count when en is 1, its frequency is 2 times of dac_sclk that are generated latter by it
  reg [2-1:0] div_cnt;  //! div_cnt will produce a signal of period DivCntParam
  reg [5:0] sending_seq_cnt;  //! start to count when en is 1, and div_cnt == DivCntParam - 1
  reg [16-1:0] r_data;  //! latch the data when sending_start is 1

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) r_data <= '0;
    else if (sending_start) r_data <= data;
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) en <= 0;
    else if (sending_start) en <= 1;
    else if (sending_done) en <= 0;
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) div_cnt <= 0;
    else if (en)
      if (div_cnt == DivCntParam - 1) div_cnt <= 0;
      else div_cnt <= div_cnt + 1;
    else div_cnt <= 0;
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) sending_seq_cnt <= 0;
    else if (en)
      if (div_cnt == DivCntParam - 1)
        if (sending_seq_cnt == 33) sending_seq_cnt <= 0;
        else sending_seq_cnt <= sending_seq_cnt + 1;
      else sending_seq_cnt <= sending_seq_cnt;
    else sending_seq_cnt <= 0;
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      dac_cs_n <= 1;
      dac_din  <= 0;
      dac_sclk <= 0;
    end else if (en && div_cnt == DivCntParam - 1) begin
      case (sending_seq_cnt)
        0: begin
          dac_cs_n <= 0;
          dac_din  <= r_data[15];
          dac_sclk <= 1;
        end

        1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31: begin
          dac_sclk <= 0;
        end

        2: begin
          dac_din  <= r_data[14];
          dac_sclk <= 1;
        end
        4: begin
          dac_din  <= r_data[13];
          dac_sclk <= 1;
        end
        6: begin
          dac_din  <= r_data[12];
          dac_sclk <= 1;
        end
        8: begin
          dac_din  <= r_data[11];
          dac_sclk <= 1;
        end
        10: begin
          dac_din  <= r_data[10];
          dac_sclk <= 1;
        end
        12: begin
          dac_din  <= r_data[9];
          dac_sclk <= 1;
        end
        14: begin
          dac_din  <= r_data[8];
          dac_sclk <= 1;
        end
        16: begin
          dac_din  <= r_data[7];
          dac_sclk <= 1;
        end
        18: begin
          dac_din  <= r_data[6];
          dac_sclk <= 1;
        end
        20: begin
          dac_din  <= r_data[5];
          dac_sclk <= 1;
        end
        22: begin
          dac_din  <= r_data[4];
          dac_sclk <= 1;
        end
        24: begin
          dac_din  <= r_data[3];
          dac_sclk <= 1;
        end
        26: begin
          dac_din  <= r_data[2];
          dac_sclk <= 1;
        end
        28: begin
          dac_din  <= r_data[1];
          dac_sclk <= 1;
        end
        30: begin
          dac_din  <= r_data[0];
          dac_sclk <= 1;
        end

        32: begin
          dac_sclk <= 1;
        end
        33: begin
          dac_cs_n <= 1;
        end
        default: ;
      endcase
    end
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) sending_done <= 0;
    else if (div_cnt == DivCntParam - 1 && sending_seq_cnt == 33) sending_done <= 1;
    else sending_done <= 0;
  end

endmodule
