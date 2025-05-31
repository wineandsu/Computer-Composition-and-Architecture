`ifdef VERILATOR
`include "include/my_defines.sv"
`else
`include "my_defines.svh"
`endif
/* verilator lint_off LATCH */
module decode_ (
    input logic                                   rst_n,

    input  logic                                  mem_siga,

    input logic       [`PC_ADDRES-1:0]                      pc_in,
    input logic       [31:0]                      instr_in,

    output logic  [`PC_ADDRES-1:0]                      pc_out,

    input logic   [`REG_WIDTH-1:0]                rs1_data_in,
    input logic   [`REG_WIDTH-1:0]                rs2_data_in,

    output   logic   [`REG_ADDR_WIDTH-1:0]    rs1_addr_out,
    output   logic   [`REG_ADDR_WIDTH-1:0]    rs2_addr_out,
    input    logic   finish,


    output   logic   [`REG_WIDTH-1:0]         rs1_data_out,
    output   logic   [`REG_WIDTH-1:0]         rs2_data_out,
    
    output   logic   [`REG_ADDR_WIDTH-1:0]    rd_addr_out,



    output  logic                             branch,
    output  logic                             ifmemread,
    output  logic                             mem_to_reg,
    output  logic                             mem_write,
    output  logic                             alu_src_jud,
    output  logic                             reg_write,

    output  logic    [`ALUOP_WIDTH-1: 0]      aluop,

    output  logic    [`REG_WIDTH-1:0]         imm,
    output         [3:0]                    funct,
    

    output      [6:0]                opcode_o,
    output      logic                divjud,
    output      logic                ab_reg_write,
    output      logic[4:0]           ab_reg_addr,
    output      logic [4:0]          ab_reg_rd,

    input      logic[63:0]            ab_reg_data_in,
    output     logic[63:0]            ab_reg_data_out,
    output     logic if_ret,
    output     logic[31:0]  ins_out
);


always_comb begin 
    if(!rst_n)begin
        pc_out    =   64'h0;
    end 
    else begin 
        pc_out    =   pc_in;
    end
end

 
logic    [6:0]   opcode  =   instr_in[6:0] ;
logic    [4:0]   rd      =   instr_in[11:7]     ;
logic    [2:0]   funct3  =   instr_in[14:12] ;
logic    [4:0]   rs1     =   instr_in[19:15]    ;    
logic    [4:0]   rs2     =   instr_in[24:20]    ;
logic    [6:0]   funct7  =   instr_in[31:25] ;

assign  opcode_o = opcode;

assign    funct  =  {funct7[5],funct3};
logic memwrite_;
logic memread_;
assign ab_reg_data_out=ab_reg_data_in;
assign ins_out=instr_in;

always_comb begin  
    //pc_out    =   pc_in;

    rs1_addr_out      =   `REG_ADDR_WIDTH'h0;
    rs2_addr_out      =   `REG_ADDR_WIDTH'h0;

    rs1_data_out      =   `REG_WIDTH'h0;
    rs2_data_out      =   `REG_WIDTH'h0;
    
    rd_addr_out       =   `REG_ADDR_WIDTH'h0;
    imm             =   64'h0;

    branch          =   1'b0;
    ifmemread         =   1'b0;
    mem_write        =   1'b0;
    alu_src_jud          =   1'b0;
    reg_write        =   1'b0;
    aluop           =   2'b00;
    divjud          =0;
    if_ret=0;
    ab_reg_addr=4'b0;
    ab_reg_write=0;
    ab_reg_rd=0;
    mem_to_reg=0;

    case(opcode) 
        `INSTR_TYPE_R:begin
                rs1_addr_out = rs1;
                rs2_addr_out = rs2;
                rs1_data_out = rs1_data_in;
                rs2_data_out = rs2_data_in;
                rd_addr_out  = rd;
                branch     = 1'b0;
                ifmemread    = 1'b0;
                mem_to_reg   = 1'b0;
                mem_write   = 1'b0;
                alu_src_jud     = 1'b0;
                reg_write   = 1'b1;
                aluop      = 2'b10;
                divjud = funct7[0];
                imm        = 64'h0;
            end
            `INSTR_TYPE_I:begin
                rs1_addr_out = rs1;
                rs2_addr_out = `REG_ADDR_WIDTH'h0;
                rs1_data_out = rs1_data_in;
                rs2_data_out = `REG_WIDTH'h0;
                rd_addr_out  = rd;
                branch     = 1'b0;
                ifmemread    = 1'b0;
                mem_to_reg   = 1'b0;
                mem_write   = 1'b0;
                alu_src_jud     = 1'b1;
                reg_write   = 1'b1;
                aluop      = 2'b10;
                divjud =0;
                imm        = {{52{instr_in[31]}},instr_in[31:20]};
            end
            `INSTR_TYPE_B:begin
                rs1_addr_out = rs1;
                rs2_addr_out = rs2;
                rs1_data_out = rs1_data_in;
                rs2_data_out = rs2_data_in;
                rd_addr_out  = 5'b0;
                branch     = 1'b1;
                ifmemread    = 1'b0;
                mem_to_reg   = 1'b0;
                mem_write   = 1'b0;
                alu_src_jud     = 1'b0;
                reg_write   = 1'b0;
                aluop      = 2'b01;
                divjud=0;
                imm        = {{51{instr_in[31]}},instr_in[31],instr_in[7],instr_in[30:25],instr_in[11:8],1'b0};
            end 
            `INSTR_TYPE_J:begin
                rs1_addr_out = `REG_ADDR_WIDTH'h0;
                rs2_addr_out = `REG_ADDR_WIDTH'h0;
                rs1_data_out = `REG_WIDTH'h0;
                rs2_data_out = `REG_WIDTH'h0;
                rd_addr_out  = rd;
                branch     = 1'b1;
                ifmemread    = 1'b0;
                mem_to_reg   = 1'b0;
                mem_write   = 1'b0;
                alu_src_jud     = 1'b0;
                reg_write   = 1'b1;
                aluop      = 2'b11;
                divjud=0;
                imm        = {{43{instr_in[31]}},instr_in[31],instr_in[19:12],instr_in[20],instr_in[30:21],1'b0};
            end
            `INSTR_TYPE_JR:begin
                rs1_addr_out = rs1;
                rs2_addr_out = `REG_ADDR_WIDTH'h0;
                rs1_data_out = rs1_data_in;
                rs2_data_out = `REG_WIDTH'h0;
                rd_addr_out  = rd;
                branch     = 1'b1;
                ifmemread    = 1'b0;
                mem_to_reg   = 1'b0;
                mem_write   = 1'b0;
                alu_src_jud     = 1'b0;
                reg_write   = 1'b1;
                aluop      = 2'b11;
                divjud=0;
                imm={{52{instr_in[31]}},instr_in[31:20]};
            end
            `INSTR_TYPE_U:begin
                rs1_addr_out = `REG_ADDR_WIDTH'h0;
                rs2_addr_out = `REG_ADDR_WIDTH'h0;
                rs1_data_out = `REG_WIDTH'h0;
                rs2_data_out = `REG_WIDTH'h0;
                rd_addr_out  = rd;
                branch     = 1'b0;
                ifmemread    = 1'b0;
                mem_to_reg   = 1'b0;
                mem_write   = 1'b0;
                alu_src_jud     = 1'b0;
                reg_write   = 1'b1;
                aluop      = 2'b11;
                divjud=0;
                imm        = {{32{instr_in[31]}},instr_in[31:12],12'b0};
            end
            `INSTR_TYPE_UPC:begin
                rs1_addr_out = `REG_ADDR_WIDTH'h0;
                rs2_addr_out = `REG_ADDR_WIDTH'h0;
                rs1_data_out = `REG_WIDTH'h0;
                rs2_data_out = `REG_WIDTH'h0;
                rd_addr_out  = rd;
                branch     = 1'b1;
                ifmemread    = 1'b0;
                mem_to_reg   = 1'b0;
                mem_write   = 1'b0;
                alu_src_jud     = 1'b0;
                reg_write   = 1'b1;
                aluop      = 2'b11;
                divjud=0;
                imm        = {{32{instr_in[31]}},instr_in[31:12],12'b0};
            end
            `INSTR_TYPE_IL:begin
                rs1_addr_out = rs1;
                rs2_addr_out = `REG_ADDR_WIDTH'h0;
                rs1_data_out = rs1_data_in;
                rs2_data_out = `REG_WIDTH'h0;
                rd_addr_out  = rd;
                branch     = 1'b0;
                ifmemread    = 1'b1;
                mem_to_reg   = 1'b1;
                mem_write   = 1'b0;
                alu_src_jud     = 1'b1;
                reg_write   = 1'b1;
                aluop      = 2'b00;
                divjud=0;
                imm        = {{52{instr_in[31]}},instr_in[31:20]};
            end
            `INSTR_TYPE_S:begin
                rs1_addr_out = rs1;
                rs2_addr_out = rs2;
                rs1_data_out = rs1_data_in;
                rs2_data_out = rs2_data_in;
                rd_addr_out  = rd;
                branch     = 1'b0;
                ifmemread    = 1'b0;
                mem_to_reg   = 1'b0;
                mem_write   = 1'b1;
                alu_src_jud     = 1'b0;
                reg_write   = 1'b0;
                aluop      = 2'b00;
                divjud=0;
                imm        = {{52{instr_in[31]}},instr_in[31:25],instr_in[11:7]};
            end
           
            `INSTR_TYPE_SRW:begin
                rs1_addr_out = rs1;
                rs2_addr_out = rs2;
                rs1_data_out = rs1_data_in;
                rs2_data_out = rs2_data_in;
                rd_addr_out  = rd;
                branch     = 1'b0;
                ifmemread    = 1'b0;
                mem_to_reg   = 1'b0;
                mem_write   = 1'b0;
                alu_src_jud   = 1'b0;
                reg_write   = 1'b1;
                aluop      = 2'b10;
                divjud = funct7[0];
                imm        = 64'h0;  
            end

            `INSTR_TYPE_SRIW:  begin
            rs1_addr_out = rs1;
                rs2_addr_out = `REG_ADDR_WIDTH'h0;
                rs1_data_out = rs1_data_in;
                rs2_data_out = `REG_WIDTH'h0;
                rd_addr_out  = rd;
                branch     = 1'b0;
                ifmemread    = 1'b0;
                mem_to_reg   = 1'b0;
                mem_write   = 1'b0;
                alu_src_jud     = 1'b1;
                reg_write   = 1'b1;
                aluop      = 2'b10;
                divjud=0;
                imm        = {{52{instr_in[31]}},instr_in[31:20]};
            end

            `INSTR_TYPE_IE:begin
                rs1_addr_out = rs1;
                rs2_addr_out = 0;
                rs1_data_out = rs1_data_in;
                rs2_data_out = 0;
                rd_addr_out  = rd;
                branch     = 1'b1;
                ifmemread    = 1'b0;
                mem_to_reg   = 1'b0;
                mem_write   = 1'b0;
                alu_src_jud     = 1'b0;
                reg_write   = 1'b1;
                aluop      = 2'b11;
                divjud = 0;
                imm        = {{59'b0},rs1};
                case(instr_in[31:20]) 
                `MSTATUS: begin
                    ab_reg_addr=1;
                    ab_reg_write=1'b1;
                    if_ret=0;
                end
               `MIE: begin
                    ab_reg_addr=2;
                    ab_reg_write=1'b1;
                    if_ret=0;
                end
                `MTVEC: begin
                    ab_reg_addr=3;
                    ab_reg_write=1'b1;
                    if_ret=0;
                end
                `MSCRATCH: begin
                    ab_reg_addr=4;
                    ab_reg_write=1'b1;
                    if_ret=0;
                end
                `MEPC: begin
                    ab_reg_addr=5;
                    ab_reg_write=1'b1;
                    if_ret=0;
                end
                `MCAUSE: begin
                    ab_reg_addr=6;
                    ab_reg_write=1'b1;
                    if_ret=0;
                end
                `MTVAL: begin
                    ab_reg_addr=7;
                    ab_reg_write=1'b1;
                    if_ret=0;
                end
                `MIP: begin
                    ab_reg_addr=8;
                    ab_reg_write=1'b1;
                    if_ret=0;
                end
                `SATP: begin
                    ab_reg_addr=9;
                    ab_reg_write=1'b1;
                    if_ret=0;
                end
                `RET: begin
                    ab_reg_addr=5;
                    ab_reg_write=0;
                    if_ret=1;
                end

                default begin
                    ab_reg_addr=0;
                    ab_reg_write=1'b0;
                end           
            endcase  
            ab_reg_rd=ab_reg_addr;    
            end
         default:begin;    
        end
        endcase 
end

    
endmodule 
