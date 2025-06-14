module pipereg
    import common::*; #(
    parameter type T = logic,
    parameter T INIT = '0
)(
    input logic clk, reset,
    input T in, 
    output T out,
    input logic flush, en
);
    always_ff @(posedge clk) begin
        if (reset | flush) begin
            out <= INIT; 
        end else if (en) begin
            out <= in;
        end
    end
    
endmodule
