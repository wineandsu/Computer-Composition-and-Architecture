`ifndef __CORE_SV
`define __CORE_SV	
`ifdef VERILATOR
`include "include/common.sv"
`include "include/alu.sv"
`include "include/my_defines.sv"
`include "include/decode_.sv"
`include "include/mem_con.sv"
`include "include/pc_reg.sv"
`include "include/reg_file.sv"
`include "include/write_b.sv"
`include "include/abreg.sv"
`else
`include"my_defines.svh"
`endif
module core import common::*;(
	input  logic       clk, reset,
	output ibus_req_t  ireq,
	input  ibus_resp_t iresp,
	output dbus_req_t  dreq,
	input  dbus_resp_t dresp,
	input  logic       trint, swint, exint
);
	/* verilator lint_off UNOPTFLAT */
logic                    if_jum;

logic    [31:0]          instr;
logic    [`PC_ADDRES-1:0]          curr_pc;
logic    [`PC_ADDRES-1:0]          de_pc_out;
logic    [`PC_ADDRES-1:0]          pc_out_mem;

logic    [`PC_ADDRES-1:0]          read_data;
logic    [4:0]           wb_write_addr;
logic    [`PC_ADDRES-1:0]          wb_write_data;
logic    [4:0]           rs1_addr_p_de;
logic    [4:0]           rs2_addr_p_de;
logic    [`PC_ADDRES-1:0]          rs1_data_i_de ;
logic    [`PC_ADDRES-1:0]          rs2_data_i_de ;

logic    [`PC_ADDRES-1:0]          rs1_data_o_de;
logic    [`PC_ADDRES-1:0]          rs2_data_o_de;
logic    [4:0]           rd_addr_o_de;
logic                    de_branch  ;   
logic                    de_memread ;   
logic                    de_memtoreg;    
logic                    de_memwrite;    
logic                    de_alusrc  ;    
logic                    de_regwrite;    
logic    [1:0]           de_aluop   ;   
logic    [`PC_ADDRES-1:0]          de_imm     ;   
logic    [3:0]           de_funct   ;
logic    [6:0]           de_opcode  ;

logic                    mem_regwrite;
logic                    wb_regwrite;

logic                    alu_regwrite    ;
logic                    alu_branch_sign ;  
logic    [`PC_ADDRES-1:0]          alu_result      ;       
logic    [4:0]           alu_rd_addr_o   ;         
logic                    alu_branch_o    ; 
logic                    alu_memread_o   ; 
logic                    alu_memwrite_o  ;     
logic                    alu_memtoreg_o  ;       
logic    [`PC_ADDRES-1:0]          alu_pc_out     ;         
logic    [`PC_ADDRES-1:0]          alu_write_data  ;       
logic    [3:0]           alu_align       ;
logic    [1:0]           alu_aluop       ;

logic    [4:0]           mem_rd_addr_o   ;
logic    [`PC_ADDRES-1:0]          rd_data;
logic                    mem_memtoreg_o;
logic [63:0] reg_f_out [0:31];
logic [63:0] ab_reg_f_out [0:31];
logic finish;
logic divjud;
logic [7:0] mem_wr_loc;
logic if_mem_zero;
logic myskip;

logic mul_fi;

//inetrupt
logic [63:0] ab_reg_data;
logic [4:0] ab_reg_addr;
logic [4:0] ab_reg_rd;
logic [63:0] de_ab_reg_data_out;
logic ab_reg_write;
logic [63:0] ab_res;
logic [4:0] ab_wb_addr;
logic if_ret;
logic [1:0] pri_mode;
logic [1:0] re_mode;
logic [1:0] temp_mode;
logic [3:0] satp_mode;
logic [3:0] trans;
logic if_trans;
logic [63:0] pc_wait;
logic [63:0] trans_res;
logic [63:0] cur_mem;
logic [63:0] req_addr;
logic [3:0] cnt;
logic in_trans;
logic pc_trans_fi;
msize_t mem_size;
logic [63:0] L2;
logic [63:0] L1;
logic [63:0] L0;
logic [63:0] de_pc_in;
logic [31:0] de_ins_out;
logic [1:0] pri_mode_nxt;
logic if_call;
logic block;
logic [63:0] pc_trans;
logic [63:0] mem_ab_res;
logic [63:0] wb_ab_res;
logic alu_ab_write;
logic mem_ab_write;
logic wb_ab_write;
logic [4:0] alu_ab_rd;
logic [4:0] mem_ab_rd;
logic [4:0] wb_ab_rd;
logic pc_if_trans;
logic my_finish; 

/*
always_ff @(posedge clk) begin
    if (reset) begin
        pc_out_mem=PCINIT;
    end
end*/
/*
always_comb begin
    if(if_call) begin
        pri_mode_nxt=3;
    end
    else begin
        pri_mode_nxt=0;
    end
end*/

always_ff @(posedge clk) begin
    if (reset) begin
        pri_mode<=3;
    end
    else if(if_ret) begin
        pri_mode<=re_mode;
    end 
    else if(if_call) begin
        pri_mode<=3;
    end
    else begin
        ;
    end
end
always_ff @(posedge clk) begin
  if(reset) begin
    my_finish=1'b0;
  end
  else if(iresp.data_ok) begin
    my_finish=1;
  end
  else begin
    my_finish=0;
  end
end


assign  ireq.addr=curr_pc;
//assign ireq.valid=1;
assign ireq.valid=1;
/*
assign dreq.valid=alu_memwrite_o||alu_memread_o;
assign dreq.addr=alu_result;*/
assign dreq.data=alu_write_data;
assign dreq.size=(in_trans)?MSIZE8:mem_size;

assign myskip=(alu_memwrite_o||alu_memread_o)&&(~alu_result[31]);
//assign temp_mode=(if_ret)? re_mode:pri_mode;
//assign trans=(iresp.data_ok)?ab_reg_f_out[9][63:60]:satp_mode;
assign trans=(my_finish)?ab_reg_f_out[9][63:60]:satp_mode;
assign L2={52'b0,{pc_wait[38:30],3'b0}};
assign L1={52'b0,{pc_wait[29:21],3'b0}};
assign L0={52'b0,{pc_wait[20:12],3'b0}};
//assign pc_trans={7'b0,read_data[54:10],pc_wait[11:0]};

assign de_pc_in=pc_wait;
always_comb begin
  if(if_ret) begin
    temp_mode=re_mode;
  end
  else if(if_call) begin
    temp_mode=3;
  end
  else begin
    temp_mode=pri_mode;
  end
end

always_comb begin
    if(temp_mode!=3&&trans==8) begin
        pc_if_trans=1;
    end
    else begin
        pc_if_trans=0;
    end
end
always_ff@(posedge clk) begin
     if(temp_mode!=3&&trans==8) begin
        if_trans<=1;
    end
    else begin
        if_trans<=0;
    end
end

always_comb begin
    if(in_trans) begin
        dreq.valid=1;
        if(cnt==0) begin
         dreq.addr={8'b0,ab_reg_f_out[9][43:0],12'b0}+L2;
         //dreq.addr={ab_reg_f_out[9][43:0],12'b0}+L2;
        end
        else if(cnt==1) begin
         dreq.addr=req_addr+L1;
        end
        else if(cnt==2) begin
         dreq.addr=req_addr+L0;   //error
        end
        else begin
         dreq.addr=req_addr+L0;
        end
    end
    else begin
       dreq.valid=alu_memwrite_o||alu_memread_o;
       dreq.addr=alu_result;
    end
end

always_ff@(posedge clk) begin
  if(reset) begin
    req_addr<=0;
    cnt<=0;
  end
  else if(in_trans) begin
    if(dresp.data_ok) begin
    cnt<=cnt+1;
    if(cnt<2) begin
      req_addr<={8'b0,read_data[53:10],12'b0};
    end
    end
  end
  else if(block&&(~in_trans))begin
    cnt<=0;
  end
end
//assign pc_trans_fi=(cnt>=3);
assign pc_trans_fi=(cnt>=3);
assign in_trans=(if_trans)&&(cnt<=3);

always_comb begin
    if(dresp.data_ok==1 )begin
       read_data=dresp.data;
    end
    else begin
        read_data=64'b0;
    end
end

always_comb begin
    if(alu_memwrite_o==1)begin
        dreq.strobe=mem_wr_loc;
    end
    else if(alu_memread_o==1||in_trans) begin
        dreq.strobe=8'b0;
    end
    else begin
        dreq.strobe=8'b0;
    end
end



pc_reg  pc_reg_1(
    .clk                (clk     ),
    .rst_n              (~reset   ),
    .ins_in             (iresp)    ,        
    .if_jum             (if_jum   ),
    .pc_in               (pc_out_mem),    
    .block              (block),  
    .pc_trans_fi        (pc_trans_fi),
    //.pc_trans           (pc_trans),
    //.pc_trans           ({dresp.data[54:10],pc_wait[11:0]}),
    .if_trans           (pc_if_trans),                        
    .instr              (instr   ),
    .curr_pc            (curr_pc ),
    .pc_wait            (pc_wait),
    .in_trans           (in_trans),
    .read_data          (read_data)
);

decode_ decode_1(
    .rst_n              (~reset     ),
    .mem_siga           (dresp.data_ok),
                                  
    .pc_in               (de_pc_in  ),
    .instr_in            (instr     ),
                                   
    .pc_out               (de_pc_out   ),
                                   
    .rs1_data_in         (rs1_data_i_de),
    .rs2_data_in         (rs2_data_i_de),
                                   
    .rs1_addr_out         (rs1_addr_p_de),
    .rs2_addr_out         (rs2_addr_p_de),
    .finish             (finish),
                                
                                   
    .rs1_data_out         (rs1_data_o_de),
    .rs2_data_out         (rs2_data_o_de),
                                  
    .rd_addr_out          (rd_addr_o_de),                       
                                   
    .branch             (de_branch    ),
    .ifmemread            (de_memread   ),
    .mem_to_reg           (de_memtoreg  ),
    .mem_write           (de_memwrite  ),
    .alu_src_jud             (de_alusrc    ),
    .reg_write           (de_regwrite  ),
    .aluop              (de_aluop     ),
                                   
    .imm                (de_imm       ),
    .funct              (de_funct     ),
                                   
    .opcode_o             (de_opcode    ),
    .divjud          (divjud),
    .ab_reg_write    (ab_reg_write),
    .ab_reg_addr      (ab_reg_addr),
    .ab_reg_rd          (ab_reg_rd),
    .ab_reg_data_in    (ab_reg_data),
    .ab_reg_data_out   (de_ab_reg_data_out),
    .if_ret             (if_ret),
    .ins_out            (de_ins_out)
);

reg_file reg_file_1(
    .clk                (clk     ),
    .rst_n              (~reset   ),
                                 
    .reg_write          (wb_regwrite),
    .rd                 (wb_write_addr),
    .rs1                (rs1_addr_p_de     ),
    .rs2                (rs2_addr_p_de     ),
    .reg_data           (wb_write_data),
                                 
    .rs1_data           (rs1_data_i_de),
    .rs2_data           (rs2_data_i_de),
    .reg_f_o            (reg_f_out),
    .finish             (finish)
);

abreg abreg(
    .clk                (clk     ),
    .rst_n              (~reset   ),
                                 
    .reg_write          (wb_ab_write),
    .rd                 (wb_ab_rd),
    .rs1                (ab_reg_addr     ),
    .reg_data           (wb_ab_res),
                                 
    .rs1_data           (ab_reg_data),
    .reg_f_o            (ab_reg_f_out),
    .if_ret             (if_ret),
    .re_mode            (re_mode),
    .satp_mode              (satp_mode),
    .finish             (finish),
    .if_call            (if_call),
    .pc_in              (pc_wait)
);


alu  alu_1(
    .pc_in               (de_pc_out    ),
    .rs1_data_in        (rs1_data_o_de),
    .rs2_data_in        (rs2_data_o_de),
    .rd_addr_in          (rd_addr_o_de),
    .funct              (de_funct      ),
    .imm                (de_imm        ),
    .opcode             (de_opcode     ),
                                    
    .branch             (de_branch     ),
    .ifmemread          (de_memread    ),
    .mem_to_reg         (de_memtoreg   ),
    .mem_write          (de_memwrite   ),
    .alu_src_jud            (de_alusrc     ),
    .reg_write          (de_regwrite   ),
    .aluop              (de_aluop      ),
                                    
    .branch_sign        (alu_branch_sign),
    .result             (alu_result     ),
    .rd_addr_out        (alu_rd_addr_o  ),
                                    
    .branch_out           (alu_branch_o   ),
    .memread_out          (alu_memread_o  ),
    .memwrite_out         (alu_memwrite_o ),
                                    
    .memtoreg_out         (alu_memtoreg_o ),
    .regwrite_out         (alu_regwrite   ),
    .pc_out               (alu_pc_out    ),
    .write_data         (alu_write_data ),
    .align              (alu_align      ),
    .aluop_o            (alu_aluop     ),
    .finish             (finish),
    .divjud             (divjud),
    .clk                (clk),
    .rst_n              (~reset),
    .mul_fi          (mul_fi),
    .mem_wr_loc      (mem_wr_loc),
    .mem_size        (mem_size),
    .if_mem_zero     (if_mem_zero),
    .ab_reg_data      (de_ab_reg_data_out),
    .ab_res           (ab_res),
    .ab_rd_out        (alu_ab_rd),
    .ins_in            (de_ins_out),
    .if_call           (if_call),
    .mtvec             (ab_reg_f_out[3]),
    .ab_rd_in             (ab_reg_rd),
    .ab_reg_write_in        (ab_reg_write),
    .ab_reg_write_out      (alu_ab_write)
);

mem_con  mem_1(
    .branch_in           (alu_branch_o    ),
    .memread_in          (alu_memread_o   ),
    .memwrite_in         (alu_memwrite_o  ),
                                    
    .memtoreg_in         (alu_memtoreg_o  ),
    .regwrite_in         (alu_regwrite),
                                    
    .aluop              (alu_aluop       ),
                                    
    .pc_in               (alu_pc_out         ),
    .reset              (reset),
    .write_data         (alu_write_data  ),
    .align              (alu_align       ),
                                    
    .branch_sign        (alu_branch_sign),
    .result             (alu_result     ),
    .rd_addr_in          (alu_rd_addr_o      ),
                                    
    .rd_addr_out        (mem_rd_addr_o  ),
    .pc_out               (pc_out_mem   ),
    .if_jum              (if_jum      ),
   // .read_data          (read_data  ),
    .rd_data            (rd_data    ),
                                    
    .memtoreg_out         (mem_memtoreg_o ),
    .regwrite_out         (mem_regwrite ) ,
    .ab_reg_data_in       (ab_res),
    .ab_reg_data_out      (mem_ab_res),
    .ab_reg_write_in       (alu_ab_write),
    .ab_reg_write_out      (mem_ab_write),
    .ab_reg_rd_in          (alu_ab_rd),
    .ab_reg_rd_out         (mem_ab_rd) 
);


write_b  write_b_1(
    .mem_to_reg           (mem_memtoreg_o),
    .reg_write           (mem_regwrite),
    .sig                 (dresp.data_ok),
                                     
    .write_addr_i       (mem_rd_addr_o),
    .write_data         (rd_data  ),
    .read_data          (read_data   ),
                                     
    .write_addr_o       (wb_write_addr),
    .write_data_out       (wb_write_data),
    .regwrite_out         (wb_regwrite  ),
    .if_mem_zero          (if_mem_zero),
    .mem_size             (mem_size),
    .addr_low             (alu_result[2:0]),
    .ab_reg_data_in       (mem_ab_res),
    .ab_reg_data_out      (wb_ab_res),
    .ab_reg_write_in       (mem_ab_write),
    .ab_reg_write_out      (wb_ab_write),
    .ab_reg_rd_in          (mem_ab_rd),
    .ab_reg_rd_out         (wb_ab_rd) 
);

//assign block=((iresp.data_ok&&(~dreq.valid)&&mul_fi)||dresp.data_ok);
assign block=((my_finish&&(~dreq.valid)&&mul_fi)||dresp.data_ok);
`ifdef VERILATOR
	DifftestInstrCommit DifftestInstrCommit(
		.clock              (clk),
		.coreid             (0),
		.index              (0),
		.valid              (block&&(~in_trans)),
		//.pc                 (ireq.addr),
        .pc                 (de_pc_in),
		.instr              (instr),
		.skip               (myskip),
		.isRVC              (0),
		.scFailed           (0),
		.wen                (wb_regwrite),    
		.wdest              ({3'b0,wb_write_addr}),    
		.wdata              (wb_write_data)     
	);

	DifftestArchIntRegState DifftestArchIntRegState (
		.clock              (clk),
		.coreid             (0),
		.gpr_0              (reg_f_out[0]),
		.gpr_1              (reg_f_out[1]),
		.gpr_2              (reg_f_out[2]),
		.gpr_3              (reg_f_out[3]),
		.gpr_4              (reg_f_out[4]),
		.gpr_5              (reg_f_out[5]),
		.gpr_6              (reg_f_out[6]),
		.gpr_7              (reg_f_out[7]),
		.gpr_8              (reg_f_out[8]),
		.gpr_9              (reg_f_out[9]),
		.gpr_10             (reg_f_out[10]),
		.gpr_11             (reg_f_out[11]),
		.gpr_12             (reg_f_out[12]),
		.gpr_13             (reg_f_out[13]),
		.gpr_14             (reg_f_out[14]),
		.gpr_15             (reg_f_out[15]),
		.gpr_16             (reg_f_out[16]),
		.gpr_17             (reg_f_out[17]),
		.gpr_18             (reg_f_out[18]),
		.gpr_19             (reg_f_out[19]),
		.gpr_20             (reg_f_out[20]),
		.gpr_21             (reg_f_out[21]),
		.gpr_22             (reg_f_out[22]),
		.gpr_23             (reg_f_out[23]),
		.gpr_24             (reg_f_out[24]),
		.gpr_25             (reg_f_out[25]),
		.gpr_26             (reg_f_out[26]),
		.gpr_27             (reg_f_out[27]),
		.gpr_28             (reg_f_out[28]),
		.gpr_29             (reg_f_out[29]),
		.gpr_30             (reg_f_out[30]),
		.gpr_31             (reg_f_out[31])
	);

    DifftestTrapEvent DifftestTrapEvent(
		.clock              (clk),
		.coreid             (0),
		.valid              (0),
		.code               (0),
		.pc                 (0),
		.cycleCnt           (0),
		.instrCnt           (0)
	);

	DifftestCSRState DifftestCSRState(
		.clock              (clk),
		.coreid             (0),
        .priviledgeMode     (temp_mode),
		//.priviledgeMode     (ab_reg_f_out[1][12:11]),
		.mstatus            (ab_reg_f_out[1]),
		.sstatus            (0 /* mstatus & 64'h800000030001e000 */),
		.mepc               (ab_reg_f_out[5]),
		.sepc               (0),
		.mtval              (ab_reg_f_out[7]),
		.stval              (0),
		.mtvec              (ab_reg_f_out[3]),
		.stvec              (0),
		.mcause             (ab_reg_f_out[6]),
		.scause             (0),
		.satp               (ab_reg_f_out[9]),
		.mip                (ab_reg_f_out[8]),
		.mie                (ab_reg_f_out[2]),
		.mscratch           (ab_reg_f_out[4]),
		.sscratch           (0),
		.mideleg            (0),
		.medeleg            (0)
	);
`endif
endmodule
`endif