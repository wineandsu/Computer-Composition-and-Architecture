`ifdef VERILATOR
`include "include/my_defines.sv"
`else
`include "my_defines.svh"
`endif
module abreg (
    input    logic    clk,
    input    logic    rst_n,

    input  logic      reg_write,
    input  logic [4:0] rd,
    input  logic [4:0] rs1,

    input  logic [`REG_WIDTH-1:0] reg_data,

    output logic [`REG_WIDTH-1:0] rs1_data,
    output logic [63:0] reg_f_o [0:31],
    input  logic if_ret,
    output logic [1:0] re_mode,
    output logic [3:0] satp_mode,
    input  logic finish,
    input logic if_call,
    input logic [63:0] pc_in
);

logic [63:0] reg_f [0:31];

//assign rs1_data = reg_f[rs1];
always_comb begin
    if (rs1 == 5'b0) 
        rs1_data = 64'b0;
    else 
        rs1_data = reg_f[rs1];    
end

assign re_mode = reg_f[1][12:11];
assign satp_mode=reg_f[9][63:60];


always_ff @(posedge clk) begin
    if(!rst_n) begin
        foreach (reg_f[i]) 
            reg_f[i] <= 64'b0;
        reg_f[6][1]<=1'b1;
        reg_f[5][31]<=1'b1;
    end
    else begin 
        reg_f<=reg_f_o;
    end
end 

for (genvar i = 0; i < 32; i++) begin
    always_comb begin
        //reg_f_o[i] = reg_f[i];
    if(if_ret==1&&i==1) begin
        reg_f_o[1]={reg_f[1][63:13],2'b0,reg_f[1][10:8],1'b1,reg_f[1][6:4],reg_f[1][7],reg_f[1][2:0]};
       /* reg_f_o[i][3]=reg_f[1][7];
        reg_f_o[i][7]=1;
        reg_f_o[i][12:11]=0;*/
    end
    else if(if_call&&i==1) begin
        reg_f_o[1]={reg_f[1][63:4],2'b0,reg_f[1][1:0]};
        //reg_f_o[i][3:2]=0;
    end
    else if(if_call&&i==5) begin
        reg_f_o[i]=pc_in;
    end
    else if(if_call&&i==6) begin
        reg_f_o[i]=64'h8;
    end
    /*
    else if(if_call) begin
        if(i==1) begin
            reg_f_o[i][3:2]=0;
        end
        else if(i==5) begin
         reg_f_o[i]=pc_in;
        end
        else if(i==6) begin
            reg_f_o[i]=8;
        end
        else begin
            ;
        end
    end */
    else if(reg_write==1&&i==rd) begin
        reg_f_o[rd] = reg_data;
    end
    else begin
        reg_f_o[i] = reg_f[i];
    end
    end
end
    
endmodule
