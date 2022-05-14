`timescale 1ns/100ps

interface mem_if(input bit clk);
  logic start, write;
  logic [7:0] addr;
  logic [15:0] data;
  logic [15:0] dout;

  clocking cb @(posedge clk);
    output write, start, addr, data;
    input dout;
  endclocking

  modport TEST(clocking cb);
endinterface //mem_if

program automatic test_memory (
  mem_if.TEST memif
);

  initial begin
    @(memif.cb) begin
      memif.cb.start <= 1'b0;
      memif.cb.write <= 1'b0;
    end
    @(memif.cb) begin
      memif.cb.start <= 1'b1;
      memif.cb.write <= 1'b1;
      memif.cb.addr <= 7'b1;
      memif.cb.data <= 16'b1;
    end
    
    repeat(10) @(memif.cb);
  end

endprogram

module top;


  bit clk = 1;
  always #5 clk = ~clk;

  mem_if memif(clk);
 
  memory mem_0(.start(memif.start), .write(memif.write), .addr(memif.addr), .data(memif.data), .dout(memif.dout));
  test_memory test_memory_0(.*);

  initial begin
    $monitor("memif.start: %b", memif.start);
  end

endmodule
