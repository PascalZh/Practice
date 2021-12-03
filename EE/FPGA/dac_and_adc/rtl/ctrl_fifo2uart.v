module ctrl_fifo2uart (
    input clk,
    input rst_n,
    input start_all,

    output reg fifo_rdreq,  //! sending `fifo_rdreq` after the second `uart_tx_done`
    input [11:0] fifo_data,
    input fifo_almost_full,
    input fifo_almost_empty,

    output reg uart_en_send,
    output reg [7:0] uart_data,
    input uart_tx_done,

    input adc_receiving_done,
    output reg adc_receiving_start
);

  localparam unsigned Idle = 2'b00, Receiving = 2'b01, Sending = 2'b11;
  reg [1:0] state;
  reg sel_fifo_data;

  //! driving sel_fifo_data, mux of uart_data
  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) sel_fifo_data <= 1'b0;
    else if (uart_tx_done) sel_fifo_data <= ~sel_fifo_data;  // use ~ to add 1 to th sel_fifo_data
  end

  //! connecting fifo_data to uart_data by a mux
  always @(*) begin
    if (sel_fifo_data == 1'b0) begin
      uart_data <= fifo_data[7:0];
    end else begin
      uart_data <= {{4{1'b0}}, fifo_data[11:8]};
    end
  end

  //! sending rdreq to fifo at the second uart_tx_done
  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      fifo_rdreq <= 1'b0;
    end else if (uart_tx_done && sel_fifo_data == 1) begin
      fifo_rdreq <= 1'b1;
    end else begin
      fifo_rdreq <= 1'b0;
    end
  end

  //! uart_en_send: driving by state, set high whenever state == Sending
  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      uart_en_send <= 1'b0;
    end else if (state == Sending) begin
      uart_en_send <= 1'b1;
    end else begin
      uart_en_send <= 1'b0;
    end
  end

  //! driving adc_receiving_start: set high after adc_receiving_done when state == Receiving
  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      adc_receiving_start <= 1'b0;
    end else if (start_all) begin
      adc_receiving_start <= 1'b1;
    end else if (state == Receiving && adc_receiving_done == 1'b1) begin
      adc_receiving_start <= 1'b1;
    end else begin
      adc_receiving_start <= 1'b0;
    end
  end

  //! state machine
  always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
      state <= Idle;
    end else begin
      case (state)
        Idle: begin
          if (start_all) state <= Receiving;
        end
        Receiving: begin
          if (adc_receiving_done && fifo_almost_full) state <= Sending;
        end
        Sending: begin
          if (uart_tx_done && fifo_almost_empty) state <= Receiving;
        end
        default: state <= Idle;
      endcase
    end
  end

endmodule
