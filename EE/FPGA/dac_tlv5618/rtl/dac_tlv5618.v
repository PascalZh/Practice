//! 12-bit DAC, the most significant 4 bits are used for control, maximum sclk frequency: 20MHz
module dac_tlv5618 #(
    parameter unsigned DivCntMax = 2  //! divide frequency ratio, maximum value is limited by `div_cnt`
) (
    input clk,
    input rst_n,

    input sending_start,
    output reg sending_done,
    input [15:0] data,

    output reg dac_cs_n,
    output reg dac_din,
    output reg dac_sclk
);

  reg en;
  //! will produce a signal of period DivCntMax, will be used to generate dac_sclk of period DivCntMax * 2
  reg [1:0] div_cnt;
  reg [5:0] sending_seq_cnt;  //! start to count when en is 1, and div_cnt == DivCntMax - 1
  reg [15:0] data_q;  //! latch the data when sending_start is 1

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) data_q <= 1'b0;
    else if (sending_start) data_q <= data;
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) en <= 1'b0;
    else if (sending_start) en <= 1'b1;
    else if (sending_done) en <= 1'b0;
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) div_cnt <= 1'b0;
    else if (en)
      if (div_cnt == DivCntMax - 1) div_cnt <= 1'b0;
      else div_cnt <= div_cnt + 1'b1;
    else div_cnt <= 1'b0;
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) sending_seq_cnt <= 1'b0;
    else if (en)
      if (div_cnt == DivCntMax - 1)
        if (sending_seq_cnt == 33) sending_seq_cnt <= 1'b0;
        else sending_seq_cnt <= sending_seq_cnt + 1'b1;
      else sending_seq_cnt <= sending_seq_cnt;
    else sending_seq_cnt <= 1'b0;
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      dac_cs_n <= 1'b1;
      dac_din  <= 1'b0;
      dac_sclk <= 1'b0;
    end else if (en && div_cnt == DivCntMax - 1) begin
      case (sending_seq_cnt)
        0: begin
          dac_cs_n <= 1'b0;
          dac_din  <= data_q[15];
          dac_sclk <= 1'b1;
        end

        1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31: begin
          dac_sclk <= 1'b0;
        end

        2: begin
          dac_din  <= data_q[14];
          dac_sclk <= 1'b1;
        end
        4: begin
          dac_din  <= data_q[13];
          dac_sclk <= 1'b1;
        end
        6: begin
          dac_din  <= data_q[12];
          dac_sclk <= 1'b1;
        end
        8: begin
          dac_din  <= data_q[11];
          dac_sclk <= 1'b1;
        end
        10: begin
          dac_din  <= data_q[10];
          dac_sclk <= 1'b1;
        end
        12: begin
          dac_din  <= data_q[9];
          dac_sclk <= 1'b1;
        end
        14: begin
          dac_din  <= data_q[8];
          dac_sclk <= 1'b1;
        end
        16: begin
          dac_din  <= data_q[7];
          dac_sclk <= 1'b1;
        end
        18: begin
          dac_din  <= data_q[6];
          dac_sclk <= 1'b1;
        end
        20: begin
          dac_din  <= data_q[5];
          dac_sclk <= 1'b1;
        end
        22: begin
          dac_din  <= data_q[4];
          dac_sclk <= 1'b1;
        end
        24: begin
          dac_din  <= data_q[3];
          dac_sclk <= 1'b1;
        end
        26: begin
          dac_din  <= data_q[2];
          dac_sclk <= 1'b1;
        end
        28: begin
          dac_din  <= data_q[1];
          dac_sclk <= 1'b1;
        end
        30: begin
          dac_din  <= data_q[0];
          dac_sclk <= 1'b1;
        end

        32: begin
          dac_sclk <= 1'b1;
        end
        33: begin
          dac_cs_n <= 1'b1;
        end
        default: ;
      endcase
    end
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) sending_done <= 1'b0;
    else if (div_cnt == DivCntMax - 1 && sending_seq_cnt == 33) sending_done <= 1'b1;
    else sending_done <= 1'b0;
  end

endmodule
