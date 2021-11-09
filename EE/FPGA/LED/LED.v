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
    if (!rst)
      cnt <= 24'd0;
    else if (cnt == 15_249_999)
      cnt <= 24'd0;
    else
      cnt <= cnt + 24'b1;
  end

  // 控制LED循环闪动
  always @(posedge clk, negedge rst) begin
    if (!rst) begin
      led <= 1'd1;
      beep <= 1'd0;
    end else if (cnt == 15_249_999) begin
      led <= ~led;
    end

    if (cnt % 10000 == 0) begin
      beep <= ~beep;
    end
  end

endmodule
