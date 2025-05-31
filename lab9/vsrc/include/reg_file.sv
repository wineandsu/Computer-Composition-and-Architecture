`ifdef VERILATOR
`include "include/my_defines.sv"
`else
`include "my_defines.svh"
`endif
module reg_file (
    input    logic    clk,
    input    logic    rst_n,

    input  logic      reg_write,
    input  logic [4:0] rd,
    input  logic [4:0] rs1,
    input  logic [4:0] rs2,
    input  logic [`REG_WIDTH-1:0] reg_data,

    output logic [`REG_WIDTH-1:0] rs1_data,
    output logic [`REG_WIDTH-1:0] rs2_data,
    output logic [63:0] reg_f_o [0:31],
    input  logic finish
);

logic [63:0] reg_f [0:31];

// First register for reading
always_comb begin
    if (rs1 == 5'b0) 
        rs1_data = 64'b0;
    else 
        rs1_data = reg_f[rs1];    
end

// Second register for reading
always_comb begin
    if (rs2 == 5'b0)
        rs2_data = 64'b0;
    else 
        rs2_data = reg_f[rs2];
end

always_ff @(posedge clk) begin
    if(!rst_n) begin
        foreach (reg_f[i]) 
            reg_f[i] <= 64'b0;
    end
    else begin 
        reg_f<=reg_f_o;
    end
end 

for (genvar i = 0; i < 32; i++) begin
    always_comb begin
    if(reg_write==1&&i==rd) begin
        reg_f_o[rd] = reg_data;
    end
    else begin
       reg_f_o[i] = reg_f[i];
    end
    end
end
    
endmodule
