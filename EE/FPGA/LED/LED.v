module LED (
    input clk,
    input rst,
    output reg led,
    output reg beep
);

  reg [24-1:0] cnt;

  // initial begin
  //     cnt = 24'b0;
  // end

  always @(posedge clk, negedge rst) begin
    if (!rst) begin
      cnt <= 0;
    end else if (cnt == 15_249_999) begin
      cnt <= 0;
    end else begin
      cnt <= cnt + 1;
    end
  end

  // 控制LED循环闪动
  always @(posedge clk, negedge rst) begin
    if (!rst) begin
      led  <= 1;
      beep <= 0;
    end else if (cnt == 15_249_999) begin
      led <= ~led;
    end

    // if (cnt % 10000 == 0) begin
    //   beep <= ~beep;
    // end
  end

endmodule
