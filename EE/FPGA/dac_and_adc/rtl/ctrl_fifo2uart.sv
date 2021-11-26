module ctrl_fifo2uart (
    input clk,
    input rst_n,

    input uart_tx_done,
    output reg uart_en_send,
    output reg [7:0] uart_data,

    output reg fifo_rdreq,  //! sending `fifo_rdreq` behind the second `uart_tx_done`
    input [11:0] fifo_data,
    input fifo_full,
    input fifo_empty

);

  reg sel_fifo_data;

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) sel_fifo_data <= 0;
    else if (uart_tx_done) sel_fifo_data <= ~sel_fifo_data;  // use ~ to add 1 to th sel_fifo_data
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      uart_data <= '0;
    end else if (sel_fifo_data == 0) begin
      uart_data <= fifo_data[7:0];
    end else if (sel_fifo_data == 1) begin
      uart_data <= {{4{1'b0}}, fifo_data[11:8]};
    end
  end

  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      fifo_rdreq <= 0;
    end else if (uart_tx_done && sel_fifo_data == 1) begin
      fifo_rdreq <= 1;
    end else begin
      fifo_rdreq <= 0;
    end
  end

endmodule
