/* verilator lint_off LATCH */
/* verilator lint_off UNOPTFLAT */
module mul_64(
   input logic [63:0] num1,
   input logic [63:0] num2,
   input logic clk,
   input logic rst_n,
   input logic  [2:0] ctrlsig, 
   input logic  if_re,
   output  logic [63:0]  res,
   output  logic  finish
);
logic [127:0] save_reg;
logic [63:0] temp;
logic [6:0]   shift_cnt;
logic [63:0] temp1;
logic [63:0] temp2;
    
always_comb begin
    if(ctrlsig>=3) begin
        temp1=(num1[63])? (~(num1-1)) : num1;
        temp2=(num2[63])? (~(num2-1)) : num2; 
    end 
    else begin
        temp1=0;
        temp2=0;
    end
end

typedef enum logic [1:0] { INIT, DOING } state_t;
state_t state, state_nxt;
logic [66:0] count, count_nxt;
    localparam logic [66:0] MULT_DELAY = {
        2'b0, 
        1'b1,
        64'b0
    };
    localparam logic [66:0] SI_MULT_DELAY = {
        1'b0,
        1'b1,
        65'b0
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
                count_nxt = {1'b0, count_nxt[66:1]};
                if (count_nxt == '0) begin
                    state_nxt = INIT;
                end
            end
            default : begin
            end
        endcase
    end

logic [128:0] p, p_nxt; 
assign res=if_re? p[127:64]:p[63:0];

    always_comb begin
        p_nxt = p;
        unique case(state)
            INIT: begin
                p_nxt = (ctrlsig>=3)? {65'b0 , temp1}:{65'b0, num1}; // Change '0 to 65'b0
            end
            DOING: begin
            //mul
              if(ctrlsig==1)  begin
                if (p_nxt[0]) begin
                    p_nxt[128:64] = p_nxt[127:64] + num2;
                end
                p_nxt = {1'b0, p_nxt[128:1]};
              end
            //divu
              else if(ctrlsig==2) begin
                p_nxt={p_nxt[127:0],1'b0};
                if(p_nxt[127:64]>=num2) begin
                    p_nxt[127:64]-=num2;
                    p_nxt[0]=1'b1;
                end
              end
            //div
            else if(ctrlsig==3) begin
                if(count_nxt!=1) begin
                p_nxt={p_nxt[127:0],1'b0};
                  if(p_nxt[127:64]>=temp2) begin
                    p_nxt[127:64]-=temp2;
                    p_nxt[0]=1'b1;
                   end
                end
                else begin                   
                   if(if_re) begin
                    temp=p_nxt[127:64];
                     if((num1[63]==1)) begin
                        p_nxt[127:64]=~(temp[63:0])+1;
                      end
                   end
                   else begin
                    temp=p_nxt[63:0];
                    if((num1[63]^num2[63])&&(p_nxt[63:0]!=0)&&(num1!=0)&&(num2!=0)) begin
                        p_nxt[63:0]={1'b1,~temp[62:0]}+1;
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