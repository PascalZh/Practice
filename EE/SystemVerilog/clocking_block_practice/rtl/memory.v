module memory (
  input start,
  input write,
  input [7:0] addr,
  input [15:0] data,
  input read,
  output reg [15:0] dout
);

  reg [15:0] mem [0:255];
  always @(posedge start) begin
    if (write) begin
      mem[addr] <= data;
    end
  end
  
  always @(posedge start) begin
    if (read) begin
        dout <= mem[addr];
    end
  end
  
endmodule