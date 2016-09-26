module main_tb;

   reg clock;

   initial begin
      clock = 1;
      $dumpfile("trace.vcd");
      $dumpvars(0, m0);
      #1000000 $finish;
   end

   always begin
      #10 clock = !clock;
   end

   main m0(clock);

endmodule // main_tb

module main(clock);

   input clock;

   wire [7:0] d_acc;

   wire [7:0] dbus;

   wire [15:0] iabus;
   wire [15:0] rabus;

   wire [15:0] imem_out;

   wire c_latch_rm, c_emit_rm_dbus;
   control ctl(
               .clk(clock),
               .dbus(dbus),
               .imem_out(imem_out),

               .alu_ez(alu_ez),
               .alu_sign(alu_sign),
               .alu_carry(alu_carry),

               .c_alu_mode(c_alu_mode),
               .c_alu_z_a(c_alu_z_a),
               .c_alu_z_b(c_alu_z_b),
               .c_alu_z_cin(c_alu_z_cin),
               .c_alu_inv_a(c_alu_inv_a),
               .c_alu_inv_b(c_alu_inv_b),
               .c_alu_inv_cin(c_alu_inv_cin),

               .c_latch_acc(c_latch_acc),
               .c_latch_pc(c_latch_pc),
               .c_latch_pc_hs(c_latch_pc_hs),
               .c_latch_aluo(c_latch_aluo),
               .c_latch_carry(c_latch_carry),
               .c_latch_ra_l(c_latch_ra_l),
               .c_latch_ra_h(c_latch_ra_h),
               .c_latch_rm(c_latch_rm),

               .c_emit_hs_dbus_pca_in_a(c_emit_hs_dbus_pca_in_a),
               .c_emit_iabus_pca_in_b(c_emit_iabus_pca_in_b),
               .c_emit_pch_dbus(c_emit_pch_dbus),
               .c_emit_pcl_dbus(c_emit_pcl_dbus),
               .c_emit_aluo_dbus(c_emit_aluo_dbus),
               .c_emit_aluo_reg_dbus(c_emit_aluo_reg_dbus),
               .c_emit_rm_dbus(c_emit_rm_dbus),
               .c_emit_ra_h_dbus(c_emit_ra_h_dbus)
               );

   wire c_latch_acc, c_latch_aluo, c_latch_carry;
   wire c_emit_aluo_dbus, c_emit_aluo_reg_dbus;
   wire alu_ez, alu_sign, alu_carry;

   wire [1:0] c_alu_mode;
   wire c_alu_z_a, c_alu_z_b, c_alu_z_cin,
        c_alu_inv_a, c_alu_inv_b, c_alu_inv_cin;
   datapath dp(
               .c_latch_acc(c_latch_acc),
               .c_latch_aluo_reg(c_latch_aluo),
               .c_latch_carry_reg(c_latch_carry),
               .c_emit_aluo_dbus(c_emit_aluo_dbus),
               .c_emit_aluo_reg_dbus(c_emit_aluo_reg_dbus),

               .dbus(dbus),
               .d_acc(d_acc),

               .alu_mode(c_alu_mode),
               .alu_z_a(c_alu_z_a),
               .alu_z_b(c_alu_z_b),
               .alu_z_cin(c_alu_z_cin),
               .alu_inv_a(c_alu_inv_a),
               .alu_inv_b(c_alu_inv_b),
               .alu_inv_cin(c_alu_inv_cin),

               .alu_ez(alu_ez),
               .alu_sign(alu_sign),
               .alu_carry(alu_carry)
               );

   wire c_latch_pc, c_latch_pc_hs;
   wire c_emit_hs_dbus_pca_in_a, c_emit_iabus_pca_in_b;
   wire c_emit_pch_dbus, c_emit_pcl_dbus;
   program_counter pc(
                      .c_latch_pc(c_latch_pc),
                      .c_latch_pc_hs(c_latch_pc_hs),
                      .c_emit_hs_dbus_pca_in_a(c_emit_hs_dbus_pca_in_a),
                      .c_emit_iabus_pca_in_b(c_emit_iabus_pca_in_b),
                      .c_emit_pch_dbus(c_emit_pch_dbus),
                      .c_emit_pcl_dbus(c_emit_pcl_dbus),

                      .dbus(dbus),
                      .iabus(iabus)
                      );

   wire c_latch_ra_l, c_latch_ra_h;
   wire c_emit_ra_h_dbus;
   ram_addressing ram_addr(
                           .c_latch_ra_l(c_latch_ra_l),
                           .c_latch_ra_h(c_latch_ra_h),
                           .c_emit_ra_h_dbus(c_emit_ra_h_dbus),

                           .dbus(dbus),
                           .rabus(rabus)
                           );

   dram dmem(rabus, dbus, c_latch_rm, c_emit_rm_dbus, clock);
   irom imem(iabus, imem_out);

endmodule

module control(
               .clk(clock),
               .dbus(dbus),
               .imem_out(imem_out),

               .alu_carry(alu_carry),
               .alu_sign(alu_sign),
               .alu_ez(alu_zero),

               .c_alu_mode(c_alu_mode),
               .c_alu_z_a(c_alu_z_a),
               .c_alu_z_b(c_alu_z_b),
               .c_alu_z_cin(c_alu_z_cin),
               .c_alu_inv_a(c_alu_inv_a),
               .c_alu_inv_b(c_alu_inv_b),
               .c_alu_inv_cin(c_alu_inv_cin),

               .c_latch_acc(c_latch_acc),
               .c_latch_pc(c_latch_pc),
               .c_latch_pc_hs(c_latch_pc_hs),
               .c_latch_aluo(c_latch_aluo),
               .c_latch_carry(c_latch_carry),
               .c_latch_ra_l(c_latch_ra_l),
               .c_latch_ra_h(c_latch_ra_h),
               .c_latch_rm(c_latch_rm),

               .c_emit_hs_dbus_pca_in_a(c_emit_hs_dbus_pca_in_a),
               .c_emit_iabus_pca_in_b(c_emit_iabus_pca_in_b),
               .c_emit_pch_dbus(c_emit_pch_dbus),
               .c_emit_pcl_dbus(c_emit_pcl_dbus),
               .c_emit_aluo_dbus(c_emit_aluo_dbus),
               .c_emit_aluo_reg_dbus(c_emit_aluo_reg_dbus),
               .c_emit_rm_dbus(c_emit_rm_dbus),
               .c_emit_ra_h_dbus(c_emit_ra_h_dbus)
               );

   output wire [1:0] c_alu_mode;
   output wire c_alu_z_a, c_alu_z_b, c_alu_z_cin,
               c_alu_inv_a, c_alu_inv_b, c_alu_inv_cin;
   output wire c_latch_acc, c_latch_pc, c_latch_pc_hs,
               c_latch_aluo, c_latch_carry,
               c_latch_ra_l, c_latch_ra_h,
               c_latch_rm;
   output wire c_emit_hs_dbus_pca_in_a, c_emit_iabus_pca_in_b,
               c_emit_pch_dbus, c_emit_pcl_dbus,
               c_emit_aluo_dbus, c_emit_aluo_reg_dbus,
               c_emit_rm_dbus, c_emit_ra_h_dbus;

   input clock;
   inout [7:0] dbus;
   input [15:0] imem_out;

   // State outputs

   // Instruction register
   reg [15:0]   ir;
   wire [7:0]   ir_instr, ir_aux;
   assign ir_instr = ir[15:8];
   assign ir_aux = ir[7:0];
   wire         c_latch_ir;
   always @(posedge c_latch_ir) ir <= imem_out;
   wire         c_emit_irh_dbus;
   assign dbus = c_emit_irh_dbus ? ir_aux : 8'bz;

   // State machine state
   wire [3:0]   c_next;
   reg [3:0]    c_state;
   initial begin
      ir <= 16'b0;
      branch_state <= 0;
      c_state = 4'b0000;
   end
   always @(negedge clock) c_state <= c_next;

   // Microcode memory
   reg [3:0]    branch_state;
   input        alu_carry, alu_sign, alu_zero;
   always @(negedge clock) branch_state <= {1'b0, alu_carry, alu_sign, alu_zero};

   wire [15:0]  mc_addr;
   assign mc_addr = {ir_instr, c_state, branch_state};

   wire [31:0]  mc_out;
   mcrom microcode(.addr(mc_addr), .data(mc_out));

   // Microcode fanout
   assign c_latch_acc = clock && mc_out[0];
   assign c_latch_pc = clock && mc_out[1];
   assign c_latch_pc_hs = clock && mc_out[2];
   assign c_latch_ir = clock && mc_out[3];
   assign c_latch_aluo = clock && mc_out[4];
   assign c_latch_carry = clock && mc_out[5];
   assign c_latch_ra_l = clock && mc_out[6];
   assign c_latch_ra_h = clock && mc_out[7];
   assign c_latch_rm = mc_out[8]; // Special case, ram is clocked

   assign c_emit_hs_dbus_pca_in_a = mc_out[9];
   assign c_emit_iabus_pca_in_b = mc_out[10];
   assign c_emit_pch_dbus = mc_out[11];
   assign c_emit_pcl_dbus = mc_out[12];
   assign c_emit_aluo_dbus = mc_out[13];
   assign c_emit_aluo_reg_dbus = mc_out[14];
   assign c_emit_irh_dbus = mc_out[15];
   assign c_emit_rm_dbus = mc_out[16];
   assign c_emit_ra_h_dbus = mc_out[17];

   assign c_next = mc_out[23:20];

   assign c_alu_mode = mc_out[25:24];
   assign c_alu_z_a = mc_out[26];
   assign c_alu_z_b = mc_out[27];
   assign c_alu_z_cin = mc_out[28];
   assign c_alu_inv_a = mc_out[29];
   assign c_alu_inv_b = mc_out[30];
   assign c_alu_inv_cin = mc_out[31];

endmodule

module condition_control(
                         .instr_cond_bits(instr_cond_bits),

                         .alu_carry(alu_carry),
                         .alu_sign(alu_sign),
                         .alu_zero(alu_zero),

                         .do_jmp(do_jmp)
                         );
   input [3:0] instr_cond_bits;
   input       alu_carry;
   input       alu_sign;
   input       alu_zero;

   reg do_jmp_noninv;
   always @(*) begin
      case (instr_cond_bits[2:0])
        3'b000: do_jmp_noninv <= 1;
        3'b001: do_jmp_noninv <= alu_zero;
        3'b010: do_jmp_noninv <= (!alu_sign) && (!alu_zero);
        3'b011: do_jmp_noninv <= alu_sign;
        3'b100: do_jmp_noninv <= alu_carry;
      endcase
   end

   // If the invert bit is set, invert!
   output do_jmp;
   assign do_jmp = instr_cond_bits[3] ? ~do_jmp_noninv : do_jmp_noninv;

endmodule

module ram_addressing(
                      .c_latch_ra_l(c_latch_ra_l),
                      .c_latch_ra_h(c_latch_ra_h),
                      .c_emit_ra_h_dbus(c_emit_ra_h_dbus),

                      .dbus(dbus),
                      .rabus(rabus)
                      );
   inout [7:0] dbus;

   // Load addressing registers from dbus
   input       c_latch_ra_l, c_latch_ra_h;
   // Addressing registers
   reg [7:0] ra_l;
   reg [7:0] ra_h;
   initial begin
      ra_l = 8'b0;
      ra_h = 8'b0;
   end

   wire       addr_register_space;
   assign addr_register_space = (ra_l[7:4] == 0);

   wire       addr_io_space;
   assign addr_io_space = ((!addr_register_space) && (ra_h[7:3] == 5'b11111));

   output wire [15:0] rabus;
   assign rabus = addr_register_space ? {ra_h, ra_l} : {8'b00000000, ra_l};

   output wire        c_emit_ra_h_dbus;
   assign dbus = c_emit_ra_h_dbus ? ra_h : 8'bz;

   always @(posedge c_latch_ra_l) begin
      ra_l <= dbus;
   end
   always @(posedge c_latch_ra_h) begin
      ra_h <= dbus;
   end

endmodule

module program_counter(
                       .c_latch_pc(c_latch_pc),
                       .c_latch_pc_hs(c_latch_pc_hs),
                       .c_emit_hs_dbus_pca_in_a(c_emit_hs_dbus_pca_in_a),
                       .c_emit_iabus_pca_in_b(c_emit_iabus_pca_in_b),
                       .c_emit_pch_dbus(c_emit_pch_dbus),
                       .c_emit_pcl_dbus(c_emit_pcl_dbus),

                       .dbus(dbus),
                       .iabus(reg_pc_out)
);
   input c_latch_pc, c_latch_pc_hs, c_emit_hs_dbus_pca_in_a, c_emit_iabus_pca_in_b;
   input c_emit_pch_dbus, c_emit_pcl_dbus;

   inout [7:0] dbus;

   wire [15:0]        pca_in_a;
   assign pca_in_a = c_emit_hs_dbus_pca_in_a ? {reg_pc_hs_out, dbus} : 16'b1;

   wire [15:0]        pca_in_b;
   assign pca_in_b = c_emit_iabus_pca_in_b ? reg_pc_out : 0;

   assign dbus = c_emit_pch_dbus ? reg_pc_out[16:8] : 8'bz;
   assign dbus = c_emit_pcl_dbus ? reg_pc_out[7:0] : 8'bz;

   output wire [15:0] reg_pc_out;
   defparam reg_pc.REG_WIDTH = 16;
   register reg_pc(.out_enable(1), .latch(c_latch_pc),
                   .din(pca_in_a + pca_in_b), .dout(reg_pc_out));

   wire [7:0]         reg_pc_hs_out;
   register reg_pc_hs(.out_enable(1), .latch(c_latch_pc_hs),
                      .din(dbus), .dout(reg_pc_hs_out));

endmodule

module datapath(
                .c_latch_acc(c_latch_acc),
                .c_latch_aluo_reg(c_latch_aluo_reg),
                .c_latch_carry_reg(c_latch_carry_reg),
                .c_emit_aluo_dbus(c_emit_aluo_dbus),
                .c_emit_aluo_reg_dbus(c_emit_aluo_reg_dbus),

                .dbus(dbus),
                .d_acc(reg_acc_out),

                .alu_mode(alu_mode),
                .alu_z_a(alu_z_a),
                .alu_z_b(alu_z_b),
                .alu_z_cin(alu_z_cin),
                .alu_inv_a(alu_inv_a),
                .alu_inv_b(alu_inv_b),
                .alu_inv_cin(alu_inv_cin),

                .alu_ez(alu_ez),
                .alu_sign(alu_sign),
                .alu_carry(reg_carry_out),
                );

   input c_latch_acc, c_latch_aluo_reg, c_latch_carry_reg, c_emit_aluo_dbus, c_emit_aluo_reg_dbus;
   input [1:0] alu_mode;
   input alu_z_a, alu_z_b, alu_z_cin, alu_inv_a, alu_inv_b, alu_inv_cin;

   inout [7:0] dbus;

   reg [7:0]  reg_alu_o;

   wire [7:0] alu_o_bus;
   wire       alu_c_out;

   output     alu_ez, alu_sign;

   alu c_alu(
             .c_in(reg_carry_out),
             .c_out(alu_c_out),

             .in_a(reg_acc_out),
             .in_b(dbus),
             .out(alu_o_bus),

             .mode(alu_mode),
             .zero_a(alu_z_a),
             .zero_b(alu_z_b),
             .zero_cin(alu_z_cin),
             .invert_a(alu_inv_a),
             .invert_b(alu_inv_b),
             .invert_cin(alu_inv_cin),

             .ez(alu_ez),
             .sign(alu_sign)
             );

   assign dbus = c_emit_aluo_dbus ? alu_o_bus : 8'bz;

   output wire [7:0] reg_acc_out;
   register reg_acc(.out_enable(1), .latch(c_latch_acc), .din(alu_o_bus), .dout(reg_acc_out));

   register reg_aluo(.out_enable(c_emit_aluo_reg_dbus), .latch(c_latch_aluo_reg),
                     .din(alu_o_bus), .dout(dbus));

   defparam reg_carry.REG_WIDTH = 1;
   output wire       reg_carry_out;
   register reg_carry(.out_enable(1), .latch(c_latch_carry_reg),
                      .din(alu_c_out), .dout(reg_carry_out));

endmodule

module dram(address, data, write_enable, out_enable, clk);
   input [15:0] address;
   inout [7:0]  data;
   input        write_enable;
   input        out_enable;
   input        clk;

   reg [7:0]    memory [0:65535];

   reg [7:0]   data_input;

   assign data = out_enable ? memory[address] : 8'bz;

   integer     i;
   initial begin
      for (i=0; i<65535; i=i+1) memory[i] <= 8'b0;
   end

   always @(posedge clk) begin
      if (write_enable) begin
         memory[address] <= data;
      end
   end

endmodule // ram

module irom(address, data);
   input [15:0] address;
   output [15:0] data;

   reg [15:0]     memory [65535:0];

   assign data = memory[address];

   initial begin
      $readmemh("programs/pong.tbh", memory);
   end

endmodule // imem

module mcrom(
             .addr(address),
             .data(data)
             );

   input [15:0] address;
   output [31:0] data;

   reg [31:0]    memory [65535:0];

   assign data = memory[address];

   initial begin
      $readmemh("microcode/hex", memory);
   end

endmodule

module register(
                .out_enable(out_enable),
                .latch(latch),

                .din(din),
                .dout(dout)
                );
   initial begin
      data <= 0;
   end

   parameter REG_WIDTH = 8;

   input out_enable, latch;

   input [REG_WIDTH-1:0] din;

   output reg [REG_WIDTH-1:0] data;
   always @(posedge latch) data <= din;

   output [REG_WIDTH-1:0]     dout;
   assign dout = out_enable ? data : {REG_WIDTH {1'bz}};
endmodule

module alu(
           .c_in(c_in),
           .c_out(c_out),

           .in_a(in_a),
           .in_b(in_b),
           .out(out),

           .mode(mode),
           .zero_a(zero_a),
           .zero_b(zero_b),
           .zero_cin(zero_cin),
           .invert_a(invert_a),
           .invert_b(invert_b),
           .invert_cin(invert_cin),

           .ez(ez),
           .sign(sign),
  );

   input [7:0] in_a, in_b;
   output reg [7:0] out;

   input            c_in;
   output           c_out;

   input [1:0]   mode;
   input         zero_a, zero_b, zero_cin, invert_a, invert_b, invert_cin;

   wire [7:0]    a_zeroed;
   wire [7:0]    b_zeroed;
   wire          cin_zeroed;
   assign a_zeroed = zero_a ? in_a : 0;
   assign b_zeroed = zero_b ? in_b : 0;
   assign cin_zeroed = zero_cin ? c_in : 0;

   wire [7:0]    a_real;
   wire [7:0]    b_real;
   wire          cin_real;
   assign a_real = invert_a ? ~a_zeroed : a_zeroed;
   assign b_real = invert_b ? ~b_zeroed : b_zeroed;
   assign cin_real = invert_cin ? ~cin_zeroed : cin_zeroed;

   always @(*) begin
      case (mode)
        2'b00: out = a_real + b_real;
        2'b01: out = a_real & b_real;
        2'b10: out = a_real | b_real;
        2'b11: out = a_real ^ b_real;
        default: out = 0;
      endcase // case (mode)
   end

   output        ez;
   assign ez = (out == 0);

   output        sign;
   assign sign = out[7];

endmodule
