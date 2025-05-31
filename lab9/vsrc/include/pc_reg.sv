`ifdef VERILATOR
`include "include/my_defines.sv"
`else
`include "my_defines.svh"
`endif

module pc_reg import common::*; (
    input  logic clk,
    input  logic rst_n,
    
    input  logic if_jum,
    input  ibus_resp_t ins_in,
    input  logic [`PC_ADDRES-1:0] pc_in,
    input  logic block,
    input logic pc_trans_fi,
    //input logic [63:0] pc_trans,
    input logic if_trans,

    output logic [31:0] instr,
    output logic [`PC_ADDRES-1:0] curr_pc,
    output logic [63:0] pc_wait,
    input logic in_trans,
    input logic [63:0] read_data
);  
logic [63:0] npc;
logic [31:0] fet_ins;
logic [31:0] cur_ins;
assign npc=if_jum?pc_in:64'(pc_wait+64'h4);

always_ff@(posedge clk) begin
    if(!rst_n) begin
      pc_wait<=PCINIT;
    end
    else if(block&&(~in_trans)) begin
     pc_wait<=npc;
    end
end

always_ff @(posedge clk) begin
    if (!rst_n) begin
        curr_pc <= PCINIT;
    end 
    else if (block==1) begin        
        if(if_trans) begin
            if(pc_trans_fi) begin
                curr_pc<={7'b0,read_data[54:10],pc_wait[11:0]};
                //curr_pc<=0;
            end
        end
        else begin
        curr_pc <= npc;
        end
    end 
end


always_ff@(posedge clk) begin
  if(block==1 | !rst_n) begin
    fet_ins<=32'b0;
  end
  else if(ins_in.data_ok==1&&in_trans==0) begin
    fet_ins<=ins_in.data;
  end
end
assign instr=fet_ins;

/*
always_ff@(posedge clk) begin
    if(block==1 | !rst_n) begin
        fet_ins<=32'b0;
    end
    else if(ins_in.data_ok==1) begin
        fet_ins<=ins_in.data;
    end
    else begin
        fet_ins<=instr;
    end
end
assign instr=(ins_in.data_ok==0)?fet_ins:ins_in.data;
*/
endmodule
