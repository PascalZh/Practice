module key_detect (
    input key,
    input clk,
    input rst_n,
    output reg [3:0] led
);

reg [1:0]state;
localparam IDLE = 2'b00, WAIT_DOWN = 2'b01, DOWN = 2'b10, WAIT_UP = 2'b11;

reg [19:0] cnt;
reg en_cnt, cnt_full;

reg key_pre;
always @(posedge clk) begin
    key_pre <= key;
end

wire pedge, nedge;
assign pedge = !key_pre && key;
assign nedge = key_pre && !key;

always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
        state <= IDLE;
        led <= 4'b0;
    end
    else begin
        case(state)
        IDLE:
            if (nedge) begin
                state <= WAIT_DOWN;
                en_cnt <= 1;
            end
        WAIT_DOWN:
            if (pedge) begin
                state <= IDLE;
                en_cnt <= 0;
            end
            else if (cnt_full) begin
                state <= DOWN;
                en_cnt <= 0;
            end
        DOWN:
            if (pedge) begin
                state <= WAIT_UP;
                en_cnt <= 1;
            end
        WAIT_UP:
            if (nedge) begin
                state <= DOWN;
                en_cnt <= 0;
            end
            else if (cnt_full) begin
                state <= IDLE;
                en_cnt <= 0;
                // key is correctly detected here
                if (led == 4'b1111)
                    led <= 0;
                else
                    led <= led + 1;
            end
        default:
            begin
                state <= IDLE;
                en_cnt <= 0;
            end
        endcase
    end
end

always @(posedge clk, negedge rst_n) begin
    if (!rst_n) begin
        cnt <= 0;
        cnt_full <= 0;
    end
    else if (en_cnt) begin
        cnt <= cnt + 1;
        if (cnt == 20'd20 - 2)
            cnt_full <= 1;
    end
    else begin
        cnt <= 0;
        cnt_full  <= 0;
    end
end
    
endmodule
