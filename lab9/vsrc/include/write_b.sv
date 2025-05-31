`ifdef VERILATOR
`include "include/my_defines.sv"
`include "include/common.sv"
`else
`include "my_defines.svh"
`endif
module write_b import common::*;(
    input logic mem_to_reg,
    input logic reg_write,
    input logic sig,

    input logic [4:0]   write_addr_i,
    input logic [`PC_ADDRES-1:0]  write_data,
    input logic [`PC_ADDRES-1:0]  read_data,
    
    output  logic [4:0]   write_addr_o,
    output  logic [`PC_ADDRES-1:0]  write_data_out,
    output  logic         regwrite_out,
    input   logic         if_mem_zero,
    input   msize_t       mem_size  ,
    input   logic [2:0]   addr_low,
    input logic [63:0] ab_reg_data_in,
    output logic [63:0] ab_reg_data_out,
    input logic ab_reg_write_in,       
    output logic ab_reg_write_out,    
    input logic [4:0] ab_reg_rd_in,         
    output logic [4:0] ab_reg_rd_out 
);
logic sign_bit;

always_comb begin
    write_addr_o = write_addr_i;
     ab_reg_data_out=ab_reg_data_in;
    ab_reg_write_out=ab_reg_write_in;
     ab_reg_rd_out = ab_reg_rd_in;

    if(mem_to_reg==0)
    regwrite_out = reg_write;
    else
    regwrite_out = sig&reg_write;

    if (reg_write && (!mem_to_reg) && (!(write_addr_i == 5'b0))) begin 
        write_data_out = write_data;
    end
    else if (reg_write && (mem_to_reg) && (!(write_addr_i == 5'b0))) begin
        //to do
        sign_bit='x;
        write_data_out='x;
        case(mem_size) 
        MSIZE8: begin
          write_data_out = read_data;
        end
        MSIZE4: begin
        case(addr_low[2])
        1'b0: begin
            sign_bit=if_mem_zero? 1'b0: read_data[31];
            write_data_out={{32{sign_bit}},read_data[31:0]};
        end
        1'b1: begin
            sign_bit=if_mem_zero? 1'b0: read_data[63];
            write_data_out={{32{sign_bit}},read_data[63:32]};
        end
        endcase
        end
        MSIZE2: begin
        case(addr_low[2:1])
        2'b00: begin
            sign_bit=if_mem_zero? 1'b0: read_data[15];
            write_data_out={{48{sign_bit}},read_data[15:0]};
        end
        2'b01: begin
            sign_bit=if_mem_zero? 1'b0: read_data[31];
            write_data_out={{48{sign_bit}},read_data[31:16]};
        end
        2'b10: begin
            sign_bit=if_mem_zero? 1'b0: read_data[47];
            write_data_out={{48{sign_bit}},read_data[47:32]};
        end
        2'b11: begin
            sign_bit=if_mem_zero? 1'b0: read_data[63];
            write_data_out={{48{sign_bit}},read_data[63:48]};
        end
        endcase
        end
        MSIZE1: begin
        case(addr_low)
        3'b000: begin
            sign_bit=if_mem_zero ? 1'b0: read_data[7];
            write_data_out={{56{sign_bit}},read_data[7:0]};
        end
        3'b001: begin
            sign_bit=if_mem_zero ? 1'b0: read_data[15];
            write_data_out={{56{sign_bit}},read_data[15:8]};
        end
        3'b010: begin
            sign_bit=if_mem_zero ? 1'b0: read_data[23];
            write_data_out={{56{sign_bit}},read_data[23:16]};
        end
        3'b011: begin
            sign_bit=if_mem_zero ? 1'b0: read_data[31];
            write_data_out={{56{sign_bit}},read_data[31:24]};
        end
        3'b100: begin
            sign_bit=if_mem_zero ? 1'b0: read_data[39];
            write_data_out={{56{sign_bit}},read_data[39:32]};
        end
        3'b101: begin
            sign_bit=if_mem_zero ? 1'b0: read_data[47];
            write_data_out={{56{sign_bit}},read_data[47:40]};
        end
        3'b110: begin
            sign_bit=if_mem_zero ? 1'b0: read_data[55];
            write_data_out={{56{sign_bit}},read_data[55:48]};
        end
        3'b111: begin
            sign_bit=if_mem_zero ? 1'b0: read_data[63];
            write_data_out={{56{sign_bit}},read_data[63:56]};
        end
        endcase
        end
        default: begin
        end
        endcase
    end
    else begin
        write_data_out = 64'b0;
    end
end

endmodule
