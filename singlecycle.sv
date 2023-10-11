module singlecycle 
    import mypkg::*;
    (
    input  logic [31:0] io_sw_i   ,
    output logic [31:0] io_lcd_o  , 
    output logic [31:0] io_ledg_o ,
    output logic [31:0] io_ledr_o ,
    output logic [31:0] io_hex0_o ,
    output logic [31:0] io_hex1_o ,
    output logic [31:0] io_hex2_o ,
    output logic [31:0] io_hex3_o ,
    output logic [31:0] io_hex4_o ,
    output logic [31:0] io_hex5_o ,
    output logic [31:0] io_hex6_o ,
    output logic [31:0] io_hex7_o ,
    output logic [31:0] pc_debug_o,

                                  
    input logic         clk_i    ,
    input logic         rst_ni   
);

    logic [31:0] nxt_pc   ;
    logic [31:0] pc       ;
    logic [31:0] pc_four  ;
    logic [31:0] instr    ;
    logic [31:0] rs1_data ;
    logic [31:0] rs2_data ;
    logic [31:0] imm      ;
    logic [31:0] operand_a;
    logic [31:0] operand_b;
    logic [31:0] alu_data ;
    logic [31:0] ld_data  ;
    logic [31:0] wb_data  ;
    
    //control unit output
    logic       br_less    ; 
    logic       br_equal   ;
    logic       br_sel     ;
    logic       rd_wren    ;
    logic [2:0] imm_sel    ;
    logic       br_unsigned;
    logic       op_a_sel   ;
    logic       op_b_sel   ;
    logic [3:0] alu_op     ;
    logic [2:0] ld_type    ;
    logic [3:0] pstrb      ;
    logic       mem_wren   ;
    logic [1:0] wb_sel     ;

    
    mux2 Mux_PC (
        .in0_i (pc_four ),
        .in1_i (alu_data),
        .sel_i (br_sel  ),
        .out_o (nxt_pc  )
    );

    pc PC (
        .clk_i  (clk_i ),
        .rst_ni (rst_ni),
        .pc_i   (nxt_pc),
        .pc_o   (pc    )
    );

    assign pc_debug_o = pc;

    pc_four PC4 (
        .pc_i      (pc     ),
        .pc_four_o (pc_four)
    );

    inst_memory Instr_Mem (
        .clk_i    (clk_i   ),
        .rst_ni   (rst_ni  ),
        .paddr_i  (pc[12:0]),
        .prdata_o (instr   ) 
    );

    regfile RegFile (
        .clk_i      (clk_i       ),
        .rst_ni     (rst_ni      ),
        .rs1_addr_i (instr[19:15]),
        .rs2_addr_i (instr[24:20]),
        .rd_addr_i  (instr[11: 7]),
        .rd_data_i  (wb_data     ),
        .rd_wren_i  (rd_wren     ),
        .rs1_data_o (rs1_data    ),
        .rs2_data_o (rs2_data    )        
    );

    immgen ImmGen (
        .instr_i   (instr  ),
        .imm_sel_i (imm_sel),      
        .imm_o     (imm    )
    );

    brcomp BRC (
        .rs1_data_i    (rs1_data   ),
        .rs2_data_i    (rs2_data   ),
        .br_unsigned_i (br_unsigned),
        .br_less_o     (br_less    ),
        .br_equal_o    (br_equal   )
    );

    mux2 MuxA (
        .in0_i (rs1_data ),
        .in1_i (pc       ),
        .sel_i (op_a_sel ),
        .out_o (operand_a)
    );

    mux2 MuxB (
        .in0_i (rs2_data ),
        .in1_i (imm      ),
        .sel_i (op_b_sel ),
        .out_o (operand_b)
    );

    alu ALU (
        .operand_a_i (operand_a),
        .operand_b_i (operand_b),
        .alu_op_i    (alu_op   ),
        .alu_data_o  (alu_data )
    );
    
    lsu LSU (
        .clk_i       (clk_i      ),
        .addr_i      (alu_data   ),
        .st_data_i   (rs2_data   ),
        .st_en_i     (mem_wren   ),
        .io_sw_i     (io_sw_i    ),
        .ld_type_i   (ld_type    ),
        .pstrb_i     (pstrb      ),
        .ld_data_o   (ld_data    ),
        .io_lcd_o    (io_lcd_o   ),
        .io_ledg_o   (io_ledg_o  ),
        .io_ledr_o   (io_ledr_o  ),
        .io_hex0_o   (io_hex0_o  ),
        .io_hex1_o   (io_hex1_o  ),
        .io_hex2_o   (io_hex2_o  ),
        .io_hex3_o   (io_hex3_o  ),
        .io_hex4_o   (io_hex4_o  ),
        .io_hex5_o   (io_hex5_o  ),
        .io_hex6_o   (io_hex6_o  ),
        .io_hex7_o   (io_hex7_o  )
    );

    mux3 Mux3 (
        .in0_i (alu_data),
        .in1_i (ld_data ),
        .in2_i (pc_four ),
        .sel_i (wb_sel  ),
        .out_o (wb_data)
    );

	ctrl_unit ControlUnit (
	        .instr_i       (instr      ),
	        .br_less_i     (br_less    ),
	        .br_equal_i    (br_equal   ),
	        .br_sel_o      (br_sel     ),
	        .rd_wren_o     (rd_wren    ),
	        .imm_sel_o     (imm_sel    ),
	        .br_unsigned_o (br_unsigned),
	        .op_a_sel_o    (op_a_sel   ),
	        .op_b_sel_o    (op_b_sel   ),
	        .alu_op_o      (alu_op     ),
	        .ld_type_o     (ld_type    ),
	        .pstrb_o       (pstrb      ),
	        .mem_wren_o    (mem_wren   ),
	        .wb_sel_o      (wb_sel     )
    );
	   
endmodule
