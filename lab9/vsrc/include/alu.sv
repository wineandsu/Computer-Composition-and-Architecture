`ifdef VERILATOR
`include "include/common.sv"
`include "include/my_defines.sv"
`include "include/mul_64.sv"
`include "include/mul_32.sv"
`else
`include "my_defines.svh"
`endif
/* verilator lint_off LATCH */
/* verilator lint_off CASEINCOMPLETE */
/* verilator lint_off WIDTH */
/* verilator lint_off UNOPTFLAT */
module alu import common::*;(
    input logic       [`PC_ADDRES-1:0]                      pc_in,
    input logic       [`REG_WIDTH-1:0]                      rs1_data_in,
    input logic       [`REG_WIDTH-1:0]                      rs2_data_in,
    input logic       [4:0]                       rd_addr_in,
    input logic       [3:0]                       funct,
    input logic       [`REG_WIDTH-1:0]                      imm,
    input logic       [6:0]                       opcode,

    input logic                                   branch,
    input logic                                   ifmemread,
    input logic                                   mem_to_reg,
    input logic                                   mem_write,
    input logic                                   alu_src_jud,
    input logic                                   reg_write,
    input logic       [1: 0]                      aluop,
    
    output logic                              branch_sign,
    output logic  [`REG_WIDTH-1:0]                      result,
    output logic  [4:0]                       rd_addr_out,

    output logic                              branch_out,
    output logic                              memread_out,
    output logic                              memwrite_out,

    output logic                              memtoreg_out,
    output logic                              regwrite_out,
    output logic  [`PC_ADDRES-1:0]                      pc_out,
    output logic  [`REG_WIDTH-1:0]                      write_data,
    output logic  [3:0]                       align,
    output logic  [1:0]                       aluop_o,
    output logic finish,
    input  logic divjud,
    input logic clk,
    input logic rst_n,
    output  logic mul_fi,
    output  logic [7:0] mem_wr_loc,
    output  msize_t mem_size,
    output logic  if_mem_zero,
    input logic [63:0] ab_reg_data,
    output logic [63:0] ab_res,
    output logic [4:0] ab_rd_out,
    input logic[31:0] ins_in,
    output logic if_call,
    input logic [63:0] mtvec,
    input logic [4:0] ab_rd_in,
    input logic ab_reg_write_in,
    output logic ab_reg_write_out
);

logic            cond_beq    ;
logic            cond_bge    ;
logic            cond_bgeu   ;
logic    [`REG_WIDTH-1:0]  alu_src1;
logic    [`REG_WIDTH-1:0]  alu_src2;
logic [63:0] temp;
logic [63:0] num1;
logic [63:0] num2;
logic [31:0] temp32;
logic [31:0] temp32_sign;
logic [2:0]  ctr;
logic [63:0] mul_res;
logic if_re;

logic [2:0] ctr_32;
logic if_re_32;
logic [63:0] mul_res_32;
logic mul_fi_64;
logic mul_fi_32;

assign mul_fi=mul_fi_32 && mul_fi_64;

assign      cond_beq    =   (rs1_data_in == rs2_data_in)    ;
assign      cond_bge    =   ($signed(rs1_data_in) >= $signed(rs2_data_in))    ;
assign      cond_bgeu   =   rs1_data_in >= rs2_data_in    ;

assign      alu_src1    =   rs1_data_in  ;
assign      alu_src2    =   (alu_src_jud == 1'b0)? rs2_data_in : imm  ;



always_comb begin 
    rd_addr_out   =   rd_addr_in   ;
    ab_rd_out = ab_rd_in;
    ab_reg_write_out = ab_reg_write_in;

    branch_out    =   branch      ;
    memread_out   =   ifmemread     ;
    memwrite_out  =   mem_write    ;
                              
    memtoreg_out  =   mem_to_reg    ;
    regwrite_out  =   reg_write&&mul_fi    ;

    branch_sign =   1'b0        ;
    result      =   64'b0       ;
    pc_out        =   pc_in        ;
    write_data  =   64'b0       ;
    align       =   4'b0        ;
    aluop_o     =   aluop       ;
    finish=0;
    mem_wr_loc=0;
    if_call=0;

    ctr=3'b00;
    if_re=0;
    ctr_32=0;
    if_re_32=0;
    ab_res=64'b0;
    mem_size=MSIZE8;
    if_mem_zero=0;

    case(aluop)
        2'b10:begin
            case(funct[2:0])
                `ADD:begin 
                    if(opcode == `INSTR_TYPE_I)begin 
                        result  =   alu_src1+alu_src2;
                    end
                    else if(opcode==`INSTR_TYPE_SRW) begin   
                        if(divjud==0) begin   
                        temp32 = (funct[3] == 1'b0) ? (alu_src1[31:0] + alu_src2[31:0]) : (alu_src1[31:0] - alu_src2[31:0]);
                        result = {{32{temp32[31]}},temp32}; 
                        end
                        else begin                 //mulw
                          ctr=1;
                          if_re=0;
                          result={{32{mul_res[31]}},mul_res[31:0]};
                        end
                    end
                    else if(opcode == `INSTR_TYPE_SRIW) begin
                         temp32 = alu_src1[31:0] + alu_src2[31:0];
                         result = {{32{temp32[31]}},temp32}; 
                    end
                    else if(divjud==1) begin           //mul
                       ctr=3'b01;
                       if_re=0;
                       result=mul_res;
                    end
                    else begin 
                        result  =   (funct[3] == 1'b0)?  (alu_src1 + alu_src2) : (alu_src1 - alu_src2)  ;
                    end
                end
                `XOR:begin 
                    if(divjud==0) begin
                    result  =   alu_src1 ^ alu_src2     ;
                    end
                    else if(opcode==`INSTR_TYPE_SRW) begin //divw
                        ctr_32=3;
                        if_re_32=0;
                        result=mul_res_32;
                    end
                    else begin   //div
                        ctr=3;
                        if_re=0;
                        result=mul_res;
                    end
                end
                `OR:begin 
                    if(divjud==0) begin
                    result  =   alu_src1 | alu_src2     ;
                    end
                     else if(opcode==`INSTR_TYPE_SRW) begin //remw
                        ctr_32=3;
                        if_re_32=1;
                        result=mul_res_32;
                    end
                    else begin            //rem
                        ctr=3;
                        if_re=1;
                        result=mul_res;
                    end
                end 
                `AND:begin 
                    if(divjud==0) begin
                    result  =   alu_src1 & alu_src2     ;
                    end
                    else if(opcode==`INSTR_TYPE_SRW) begin //remuw
                        ctr_32=2;
                        if_re_32=1;
                        result=mul_res_32;
                    end
                    else begin             //remu
                     ctr=2;
                     if_re=1;
                     result=mul_res;
                    end
                end 
                `SLL:begin                     //big problem
                    if(opcode==`INSTR_TYPE_SRIW||opcode==`INSTR_TYPE_SRW) begin
                       temp = alu_src1 << alu_src2[4:0];
                       result={{32{temp[31]}},temp[31:0]};
                    end
                    else begin
                    result  =   alu_src1 << alu_src2[5:0]     ;
                    end
                end 
                `SRL:begin
                   if(opcode==`INSTR_TYPE_SRW||opcode==`INSTR_TYPE_SRIW) begin
                     if(funct[3]==1'b1) begin
                        temp32_sign={32{alu_src1[31]}};
                        temp32=(alu_src1[31:0]>>alu_src2[4:0])|(temp32_sign<<(32-alu_src2[4:0]));
                        result={{32{temp32[31]}},temp32};
                    end
                    else if(divjud==1) begin     //divuw
                        ctr_32=2;
                        if_re_32=0;
                        result=mul_res_32;
                    end
                    else  begin                       //attention there may be mistakes
                      temp32 = alu_src1[31:0]>>alu_src2[4:0];
                      result={{32{temp32[31]}},temp32};
                    end
                   end

                   else if(divjud==1) begin         //divu
                     if_re=0;
                     ctr=2;
                     result=mul_res;
                   end

                   else begin   
                       if(funct[3]==1'b1) begin
                        temp={64{alu_src1[63]}};
                        result = (alu_src1>>alu_src2[5:0])|(temp<<(64-alu_src2[5:0]));
                        end
                       else begin
                        result = alu_src1>>alu_src2[5:0];
                       end
                    end 
                end

                `SLT:begin 
                    result  =   (signed'(alu_src1) < signed'(alu_src2))? 64'b1 : 64'b0  ;
                end 
                `SLTU:begin
                    result  =   (alu_src1 < alu_src2)? 64'b1 : 64'b0  ; 
                end
                        default:begin;
        end
            endcase
            finish=1;
        end 
        2'b01:begin
            case(funct[2:0])
                3'b000:begin
                    pc_out  =   (cond_beq == 1'b1)? (pc_in+imm) : pc_in  ;
                    branch_sign  =   (cond_beq == 1'b1)? 1'b1 : 1'b0   ;
                end
                3'b001:begin
                    pc_out  =   (cond_beq == 1'b0)? (pc_in+imm) : pc_in  ;
                    branch_sign  =   (cond_beq == 1'b0)? 1'b1 : 1'b0   ;     
                end 
                3'b101:begin
                    pc_out  =   (cond_bge == 1'b1)? (pc_in+imm) : pc_in  ;
                    branch_sign  =   (cond_bge == 1'b1)? 1'b1 : 1'b0   ;
                end 
                3'b100:begin
                    pc_out  =   (cond_bge == 1'b0)? (pc_in+imm) : pc_in  ;
                    branch_sign  =   (cond_bge == 1'b0)? 1'b1 : 1'b0   ;
                end
                3'b111:begin
                    pc_out  =   (cond_bgeu == 1'b1)? (pc_in+imm) : pc_in  ;
                    branch_sign  =   (cond_bgeu == 1'b1)? 1'b1 : 1'b0   ;
                end 
                3'b110:begin
                    pc_out  =   (cond_bgeu == 1'b0)? (pc_in+imm) : pc_in  ;
                    branch_sign  =   (cond_bgeu == 1'b0)? 1'b1 : 1'b0   ;
                end
                default:begin;
                end
            endcase 
            finish=1;
        end
        2'b11:begin
            case(opcode)
                `INSTR_TYPE_J:begin 
                    result  =   pc_in + 64'h4    ;
                    pc_out    =   pc_in + imm      ;
                    branch_sign = 1'b1;
                end
                `INSTR_TYPE_JR:begin 
                    result  =   pc_in + 64'h4    ;
                    pc_out    =   (alu_src1 + imm)&(~64'h1)      ;
                    branch_sign = 1'b1;
                end
                `INSTR_TYPE_U:begin 
                    result  =   imm     ;
                end 
                `INSTR_TYPE_UPC:begin 
                    result  =   pc_in + imm    ;
                end
                `INSTR_TYPE_IE: begin
                    case(funct[2:0]) 
                    3'b010: begin      //csrrs
                        result=ab_reg_data;
                        ab_res=rs1_data_in|ab_reg_data;
                    end
                    3'b111: begin      //csrrci
                        result=ab_reg_data;
                        ab_res=(~imm)&ab_reg_data;
                    end
                    3'b110: begin      //csrrsi
                        result=ab_reg_data;
                        ab_res=(imm)|ab_reg_data;
                    end
                    3'b001: begin       //csrrw
                        result=ab_reg_data;
                        ab_res=rs1_data_in;
                    end
                    3'b101: begin      //csrrwi
                        result=ab_reg_data;
                        ab_res=(imm);
                    end
                    3'b011: begin     //csrrc
                        result=ab_reg_data;
                        ab_res=(~rs1_data_in)&ab_reg_data;
                    end
                    3'b000: begin
                        if(ins_in[31:25]==7'b0011000)  begin   
                        //mret
                        pc_out=ab_reg_data;
                        branch_sign=1;
                        result=0;
                        ab_res=0;
                        if_call=0;
                        end
                        else if(ins_in[31:20]==0) begin
                        //ecall
                          if(mtvec[1:0]==0) begin
                            pc_out=mtvec;
                          end
                          else begin
                            pc_out={mtvec[63:2],2'b0}+6'b100000;
                          end
                        branch_sign=1;
                        result=0;
                        ab_res=0;
                        if_call=1;
                        end
                    end
                    default begin
                    end
                endcase
                end
                        default:begin;
        end
            endcase
            finish=1;
        end
        2'b00:begin
            result  =   alu_src1 + imm  ;
            write_data = alu_src2   ;  
            align[3] = opcode[5]    ;
            align[2:0] = funct[2:0] ;
            finish=1;
            if(opcode==`INSTR_TYPE_S) begin
            write_data=0;
            case(funct[2:0])
            3'b000 : begin
             mem_size=MSIZE1;
             case(result[2:0])
              3'b000: begin
                mem_wr_loc=8'b00000001;
                write_data[7:0] = alu_src2[7:0];
              end
              3'b001: begin
                mem_wr_loc=8'b00000010;
                write_data[15:8] = alu_src2[7:0];
              end
              3'b010: begin
                mem_wr_loc=8'b00000100;
                write_data[23:16] = alu_src2[7:0];
              end
              3'b011: begin
                mem_wr_loc=8'b00001000;
                write_data[31:24] = alu_src2[7:0];
              end
              3'b100: begin
                mem_wr_loc=8'b00010000;
                write_data[39:32] = alu_src2[7:0];
              end
              3'b101: begin
                mem_wr_loc=8'b00100000;
                write_data[47:40] = alu_src2[7:0];
              end
              3'b110: begin
                mem_wr_loc=8'b01000000;
                write_data[55:48] = alu_src2[7:0];
              end
              3'b111: begin
                mem_wr_loc=8'b10000000;
                write_data[63:56] = alu_src2[7:0];
              end
              default :begin
              end
            endcase
            end

            3'b001 :begin 
             mem_size=MSIZE2; 
            case(result[2:1]) 
            2'b00: begin
             mem_wr_loc=8'b00000011;
             write_data[15:0] = alu_src2[15:0];
            end
            2'b01: begin
             mem_wr_loc=8'b00001100;
              write_data[31:16] = alu_src2[15:0];
            end
            2'b10: begin
             mem_wr_loc=8'b00110000;
              write_data[47:32] = alu_src2[15:0];
            end
            2'b11: begin
             mem_wr_loc=8'b11000000;
              write_data[63:48] = alu_src2[15:0];
            end
            default begin
            end
            endcase
            end

            3'b010 :begin
            mem_size=MSIZE4;
            case(result[2])
            1'b0: begin
            mem_wr_loc=8'b00001111;
             write_data[31:0] = alu_src2[31:0];
            end
            1'b1: begin
            mem_wr_loc=8'b11110000;
            write_data[63:32] = alu_src2[31:0];
            end
            default :begin
            end
            endcase
            end

            3'b011: begin
            mem_wr_loc=8'b11111111;
            mem_size=MSIZE8;
            write_data = alu_src2;
            end

            default :begin
            end
            endcase
            end

            else begin
            mem_wr_loc=8'b00000000;
            case(funct[2:0]) 
            3'b011: begin
            mem_size=MSIZE8;
            if_mem_zero=0;
            end
            3'b000 : begin
            mem_size=MSIZE1;
            if_mem_zero=0;
            end
            3'b100 : begin
            mem_size=MSIZE1;
            if_mem_zero=1;
            end
            3'b001 :begin
            mem_size=MSIZE2;
            if_mem_zero=0;
            end
            3'b101 :begin
            mem_size=MSIZE2;
            if_mem_zero=1;
            end
            3'b010 :begin
            mem_size=MSIZE4;
            if_mem_zero=0;
            end
            3'b110 :begin
            mem_size=MSIZE4;
            if_mem_zero=1;
            end
            default :begin
            end
            endcase
            end

        end
        default:begin;
        end
    endcase
end

mul_64 mul_1(
    .num1(alu_src1),
    .num2(alu_src2),
    .clk(clk),
    .rst_n(rst_n),
    .ctrlsig(ctr),
    .if_re(if_re),
    .res(mul_res),
    .finish(mul_fi_64)   
  );

mul_32 mul_2(
    .num1(alu_src1),
    .num2(alu_src2),
    .clk(clk),
    .rst_n(rst_n),
    .ctrlsig(ctr_32),
    .if_re(if_re_32),
    .res(mul_res_32),
    .finish(mul_fi_32)  
);

endmodule 
