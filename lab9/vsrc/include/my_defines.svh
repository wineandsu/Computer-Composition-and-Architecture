`define     REG_WIDTH           64
`define     PC_ADDRES           64
`define     REG_ADDR_WIDTH      5
`define     INSTR_WIDTH         32
`define     ALUCON_WIDTH        4


`define     INSTR_TYPE_R        7'b0110011
`define     INSTR_TYPE_I        7'b0010011
`define     INSTR_TYPE_IL       7'b0000011
`define     INSTR_TYPE_S        7'b0100011
`define     INSTR_TYPE_B        7'b1100011
`define     INSTR_TYPE_J        7'b1101111
`define     INSTR_TYPE_JR       7'b1100111
`define     INSTR_TYPE_U        7'b0110111
`define     INSTR_TYPE_UPC      7'b0010111
`define     INSTR_TYPE_IE       7'b1110011
`define     INSTR_TYPE_SRW      7'b0111011
`define     INSTR_TYPE_SRIW     7'b0011011

`define     ALUOP_WIDTH         2

`define     ADD                 3'b000   
`define     XOR                 3'b100
`define     OR                  3'b110
`define     AND                 3'b111
`define     SLL                 3'b001
`define     SRL                 3'b101   
`define     SLT                 3'b010
`define     SLTU                3'b011

`define     MSTATUS  12'h300
`define     MIE     12'h304
`define     MTVEC     12'h305
`define     MSCRATCH  12'h340
`define     MEPC    12'h341
`define     MCAUSE     12'h342
`define     MTVAL    12'h343
`define     MIP     12'h344
`define     SATP     12'h180
`define     RET     12'h302




