module adc_adc128s022 #(
    //! divide frequency ratio, maximum value is limited by `div_cnt`
    parameter unsigned DivCntMax = 8
) (
    input clk,
    input rst_n,

    input receiving_start,
    output reg receiving_done,
    output reg [11:0] data,
    input [2:0] addr,  //! must be assigned at the same time as `receiving_start`

    output reg adc_cs_n,
    output reg adc_sclk,
    output reg adc_din,
    input adc_dout
);

  reg en;
  reg [2:0] div_cnt;
  reg [5:0] receiving_seq_cnt;
  reg [2:0] addr_q;

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) addr_q <= 3'b0;
    else if (receiving_start) addr_q <= addr;
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      en <= 1'b0;
      adc_cs_n <= 1'b1;
    end else if (receiving_start) begin
      en <= 1'b1;
      adc_cs_n <= 1'b0;
    end else if (receiving_done) begin
      en <= 1'b0;
      adc_cs_n <= 1'b1;
    end
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) div_cnt <= 1'b0;
    else if (en)
      if (div_cnt == DivCntMax - 1) div_cnt <= 2'b0;
      else div_cnt <= div_cnt + 1'b1;
    else div_cnt <= 2'b0;
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) receiving_seq_cnt <= 1'b0;
    else if (en)
      if (div_cnt == DivCntMax - 1)
        if (receiving_seq_cnt == 31) receiving_seq_cnt <= 1'b0;
        else receiving_seq_cnt <= receiving_seq_cnt + 1'b1;
      else receiving_seq_cnt <= receiving_seq_cnt;
    else receiving_seq_cnt <= 1'b0;
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      adc_din <= 1'b0;
      adc_sclk <= 1'b1;  // ensure that before receiving_seq_cnt == 0, `adc_sclk` is 1
      data <= 12'b0;
      receiving_done <= 1'b0;
    end else if (en && div_cnt == DivCntMax - 1) begin
      case (receiving_seq_cnt)
        0: adc_sclk <= 1'b0;
        2, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30: adc_sclk <= 1'b0;

        4: begin
          adc_sclk <= 1'b0;
          adc_din  <= addr_q[2];
        end
        6: begin
          adc_sclk <= 1'b0;
          adc_din  <= addr_q[1];
        end
        8: begin
          adc_sclk <= 1'b0;
          adc_din  <= addr_q[0];
        end

        1, 3, 5, 7: adc_sclk <= 1'b1;
        9: begin
          adc_sclk <= 1'b1;
          data[11] <= adc_dout;
        end
        11: begin
          adc_sclk <= 1'b1;
          data[10] <= adc_dout;
        end
        13: begin
          adc_sclk <= 1'b1;
          data[9]  <= adc_dout;
        end
        15: begin
          adc_sclk <= 1'b1;
          data[8]  <= adc_dout;
        end
        17: begin
          adc_sclk <= 1'b1;
          data[7]  <= adc_dout;
        end
        19: begin
          adc_sclk <= 1'b1;
          data[6]  <= adc_dout;
        end
        21: begin
          adc_sclk <= 1'b1;
          data[5]  <= adc_dout;
        end
        23: begin
          adc_sclk <= 1'b1;
          data[4]  <= adc_dout;
        end
        25: begin
          adc_sclk <= 1'b1;
          data[3]  <= adc_dout;
        end
        27: begin
          adc_sclk <= 1'b1;
          data[2]  <= adc_dout;
        end
        29: begin
          adc_sclk <= 1'b1;
          data[1]  <= adc_dout;
        end
        31: begin
          adc_sclk <= 1'b1;
          data[0] <= adc_dout;
          receiving_done <= 1'b1;
        end
        default: ;
      endcase
    end else begin
      receiving_done <= 1'b0;
    end
  end

endmodule
