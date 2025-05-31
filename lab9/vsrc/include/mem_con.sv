`ifdef VERILATOR
`include "include/my_defines.sv"
`include"include/common.sv"
`else
`include "my_defines.svh"
`endif
module mem_con import common::*;(
    input logic      branch_in,
    input logic      memread_in,
    input logic      memwrite_in,
    input logic      memtoreg_in,
    input logic      regwrite_in,

    input logic   [1:0]    aluop,

    input logic   [`PC_ADDRES-1:0]    pc_in,
    input logic reset,
    input logic   [`PC_ADDRES-1:0]    write_data,
    input logic   [3:0]     align,
    
    input logic             branch_sign,
    input logic   [`PC_ADDRES-1:0]    result,
    input logic   [4:0]     rd_addr_in,

    output logic   [4:0]    rd_addr_out,
    output logic   [`PC_ADDRES-1:0]   pc_out,
    output logic            if_jum,
    //output logic  [`PC_ADDRES-1:0]    read_data,
    output logic  [`PC_ADDRES-1:0]    rd_data,

    output logic      memtoreg_out,
    output logic      regwrite_out,
    input logic [63:0] ab_reg_data_in,
    output logic [63:0] ab_reg_data_out,
    input logic ab_reg_write_in,       
    output logic ab_reg_write_out,    
    input logic [4:0] ab_reg_rd_in,         
    output logic [4:0] ab_reg_rd_out       
);

logic  [`PC_ADDRES-1:0]     ram [0:255];
logic  [`PC_ADDRES-1:0]     ram_data_o;
logic  [`PC_ADDRES-1:0]     ram_data_i;

assign if_jum = branch_in && branch_sign  ; 
assign rd_addr_out    =   rd_addr_in   ;
assign ab_reg_data_out=ab_reg_data_in;
assign ab_reg_write_out=ab_reg_write_in;;
assign ab_reg_rd_out = ab_reg_rd_in;

always_comb begin
    if(reset) begin
        pc_out=PCINIT;
    end
    else begin
        pc_out=pc_in;
    end
end


always_comb begin 
    if(memtoreg_in == 1'b0)begin
        rd_data =   result  ;
    end
    else begin 
        rd_data =  `PC_ADDRES'b0   ;
    end
end


assign memtoreg_out  =   memtoreg_in  ;
assign regwrite_out  =   regwrite_in  ;

endmodule 
