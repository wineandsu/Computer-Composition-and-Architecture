/* verilator lint_off LATCH */
/* verilator lint_off UNOPTFLAT */
module mul_32(
   input logic [63:0] num1,
   input logic [63:0] num2,
   input logic clk,
   input logic rst_n,
   input logic  [2:0] ctrlsig, 
   input logic  if_re,
   output  logic [63:0]  res,
   output  logic  finish
);
logic [31:0] temp;
logic [5:0]   shift_cnt;
logic [31:0] temp1;
logic [31:0] temp2;
logic [31:0] num1_w;
logic [31:0] num2_w;

assign num1_w=num1[31:0];
assign num2_w=num2[31:0];
    
always_comb begin
    if(ctrlsig>=3) begin
        temp1=(num1_w[31])? (~(num1_w-1)) : num1_w;
        temp2=(num2_w[31])? (~(num2_w-1)) : num2_w; 
    end 
    else begin
        temp1=0;
        temp2=0;
    end
end

typedef enum logic [1:0] { INIT, DOING } state_t;
state_t state, state_nxt;
logic [34:0] count, count_nxt;
    localparam logic [34:0] MULT_DELAY = {
        2'b0, 
        1'b1,
        32'b0
    };
    localparam logic [34:0] SI_MULT_DELAY = {
        1'b0,
        1'b1,
        33'b0
    };
    always_ff @(posedge clk) begin
        if (~rst_n) begin
            {state, count} <= '0;
        end 
        else if(finish==0) begin
            {state, count} <= {state_nxt, count_nxt};
        end
        
        else if(ctrlsig==0) begin
            state<=INIT;
        end
    end
assign finish=(state_nxt==INIT);

always_comb begin
        {state_nxt, count_nxt} = {state, count}; // default
        unique case(state)
            INIT: begin
                if (ctrlsig!=0) begin
                    state_nxt = DOING;
                    count_nxt = (ctrlsig>=3)?SI_MULT_DELAY:MULT_DELAY;
                end
            end
            DOING: begin
                count_nxt = {1'b0, count_nxt[34:1]};
                if (count_nxt == '0) begin
                    state_nxt = INIT;
                end
            end
            default : begin
            end
        endcase
    end

logic [64:0] p, p_nxt; 
assign res=if_re? {{32{p[63]}},p[63:32]}:{{32{p[31]}},p[31:0]};

    always_comb begin
        p_nxt = p;
        unique case(state)
            INIT: begin
                p_nxt = (ctrlsig>=3)? {33'b0 , temp1}:{33'b0, num1_w}; // Change '0 to 65'b0
            end
            DOING: begin
            //mul
              if(ctrlsig==1)  begin
                if (p_nxt[0]) begin
                    p_nxt[64:32] = p_nxt[64:32] + num2_w;
                end
                p_nxt = {1'b0, p_nxt[64:1]};
              end
            //divu
              else if(ctrlsig==2) begin
                p_nxt={p_nxt[63:0],1'b0};
                if(p_nxt[63:32]>=num2_w) begin
                    p_nxt[63:32]-=num2_w;
                    p_nxt[0]=1'b1;
                end
              end
            //div
            else if(ctrlsig==3) begin
                if(count_nxt!=1) begin
                p_nxt={p_nxt[63:0],1'b0};
                  if(p_nxt[63:32]>=temp2) begin
                    p_nxt[63:32]-=temp2;
                    p_nxt[0]=1'b1;
                   end
                end
                else begin
                   if(if_re) begin
                    temp=p_nxt[63:32];
                     if((num1_w[31]==1)) begin
                        p_nxt[63:32]=~(temp[31:0])+1;
                      end
                   end
                   else begin
                    temp=p_nxt[31:0];
                    if((num1_w[31]^num2_w[31])&&(p_nxt[31:0]!=0)&&(num1_w!=0)&&(num2_w!=0)) begin
                        p_nxt[31:0]={1'b1,~temp[30:0]}+1;
                    end
                end
            end
            end
            end
            default : begin
            end
        endcase

    end

    always_ff @(posedge clk) begin
        if (~rst_n) begin
            p <= '0;
        end 
        else if(finish==0) begin
            p <= p_nxt;
        end     
    end


endmodule