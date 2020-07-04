
// UART in a CPLD for my microcoded 6502 computer.
// By JuanGg on JUNE 2020
// https://juangg-projects.blogspot.com/
// http://forum.6502.org/viewtopic.php?f=12&t=5811

// UART "core" from here: https://www.nandland.com/vhdl/modules/module-uart-serial-port-rs232.html
// with a couple changes. Thank you!

module uart_combined
	(
	//Bus
	inout [7:0] data_bus,
	input [15:0] addr_bus,
	input r_w, phi2, reset, v_a, 
	output db_en, db_dir,
	//Test
	output [7:0] leds,
	//Rx
	input rx_serial,
	//Tx
	output tx_serial,
	//Misc
	input clock_50M,
	output IRQ,
	//Flow control
	output DTR
	);

	wire rx_dv;
	wire tx_dv;
	wire tx_active;
	wire [7:0] rx_byte;
	wire [7:0] tx_byte;

	//INSTANTIATE RX/TX MODULES
	uart_rx UART_RX_INST
    (.i_Clock(clock_50M),
     .i_Rx_Serial(rx_serial),
     .o_Rx_DV(rx_dv),
     .o_Rx_Byte(rx_byte)
     );
   
	uart_tx UART_TX_INST
    (.i_Clock(clock_50M),
     .i_Tx_DV(tx_dv),
     .i_Tx_Byte(tx_byte),
     .o_Tx_Active(tx_active),
     .o_Tx_Serial(tx_serial),
     .o_Tx_Done()
     );
	  
	 uart_decoding UART_DEC_INST
		(
		.data_bus(data_bus),
		.addr_bus(addr_bus),
		.r_w(r_w), 
		.phi2(phi2), 
		.reset(reset), 
		.v_a(v_a), 
		.db_en(db_en), 
		.db_dir(db_dir),

		.leds(leds),

		.rx_byte(rx_byte),
		.rx_dv(rx_dv),

		.tx_byte(tx_byte),
		.tx_active(tx_active),
		.tx_dv(tx_dv),
		.IRQ(IRQ),
		.clock_50M(clock_50M),
		.DTR(DTR)
		);


endmodule




module uart_decoding
	(
	//Bus
	inout [7:0] data_bus,
	input [15:0] addr_bus,
	input r_w, phi2, reset, v_a, 
	output db_en, db_dir,
	//Test
	output [7:0] leds,
	//Rx
	input [7:0] rx_byte,
	input rx_dv,
	//Tx
	output [7:0] tx_byte,
	input tx_active,
	output tx_dv,
	//IRQ
	output IRQ,
	//Sync
	input clock_50M,
	//Flow control
	output DTR
	);
	
	//PARAMETERS
	parameter READ = 1'b1;
	parameter WRITE = 1'b0;
	parameter TEST_ADDR = 16'h9abc;
	parameter DATA_ADDR = 16'h9000;
	parameter SR_ADDR = 16'h9001;
	
	//REGS
	//Test
	//reg [7:0] r_leds;
	
	//Bus
	reg r_db_en;
	reg [7:0] r_data_bus;
	
	//Rx
	reg [7:0] r_rx_byte;
	reg r_rx_valid;
	reg r_rx_overrun;
	reg r_reset_rx_flags;
	reg r_rx_new_byte;
	
	//Tx
	reg r_tx_dv0; //For synchronization purposes
	reg r_tx_dv;
	reg r_tx_ready0; //For syncronization purposes
	reg r_tx_ready;
	reg [7:0] r_tx_byte;

	//IRQ
	reg r_ie_tx;
	reg r_ie_rx;
	
	//There were timign problems, syncronization across two clock domains,
	//system clock (phi2) and local clock (clock_50M).
	always @(negedge clock_50M)
	begin
		r_tx_ready0 <= ~tx_active;
		r_tx_ready <= r_tx_ready0;
		
		r_tx_dv <= r_tx_dv0;
	end
	
	//Save received byte and set rx flags.
	always @(posedge rx_dv)
	begin
		r_rx_byte <= rx_byte;
	end
	
	always @(posedge r_reset_rx_flags, posedge rx_dv, negedge reset)
	begin
			if (r_reset_rx_flags | ~reset)
				begin
					r_rx_valid <= 1'b0;
					r_rx_overrun <= 1'b0;
				end
			else
				begin
				if (~r_rx_valid)
					r_rx_valid <= 1'b1;
				else
					r_rx_overrun <= 1'b1;
				end
	end	
	
	always @(*)
	begin
		//BUS
		if((addr_bus == TEST_ADDR || addr_bus == DATA_ADDR
			|| addr_bus == SR_ADDR) & phi2)
			
			r_db_en <= 1'b0; //Enable bus transceiver
			
		else
			r_db_en <= 1'b1; //Disable bus transceiver
	end
	
	//READ DATA
	always @(posedge (phi2 & v_a))
	begin
		if (r_w == READ)
		begin
			if (addr_bus == DATA_ADDR)
			begin
				r_data_bus <= r_rx_byte;
				r_reset_rx_flags <= 1'b1;
			end
			else if (addr_bus == SR_ADDR)
			begin
				r_data_bus[0] <= r_rx_valid;
				r_data_bus[1] <= r_tx_ready;
				r_data_bus[2] <= r_rx_overrun;
				r_data_bus[7:3] <= 0;
				//r_data_bus[6] <= r_ie_rx;
				//r_data_bus[7] <= r_ie_tx;
			end
			else
				r_reset_rx_flags <= 1'b0;
		end
	end
	
	//WRITE DATA
	always @(negedge phi2, negedge reset)
	begin
	
		if (~reset)
		begin
			r_ie_tx <= 0;
			r_ie_rx <= 0;
		end
		
		else if(r_w == WRITE)
		begin
			if (addr_bus == DATA_ADDR)
			begin
				r_tx_byte <= data_bus;
				r_tx_dv0 <= 1'b1; //Will be cleared on next negedge of phi2.
			end
			else if (addr_bus == SR_ADDR)
			begin
				r_ie_tx <= data_bus[7];
				r_ie_rx <= data_bus[6];
			end
		end	
		else
			r_tx_dv0 <= 1'b0; //Clear transmit trigger pulse.
			
		
	end
	

	
	//BUS
	assign db_dir = r_w;
	assign db_en = r_db_en;
	assign data_bus = (db_dir == READ && db_en == 1'b0) ? r_data_bus: 8'hZZ;
	//TEST
	assign leds[0] = r_rx_valid;
	assign leds[1] = r_tx_ready;
	assign leds[2] = r_rx_overrun;
	assign leds[5:3] = 0;
	assign leds[6] = r_ie_rx;
	assign leds[7] = r_ie_tx;
	
	//TX
	assign tx_byte = r_tx_byte; 
	assign tx_dv = r_tx_dv;

	//IRQ
	assign IRQ = r_ie_tx & r_tx_ready | r_ie_rx & r_rx_valid;
	
	//Flow control
	assign DTR = r_rx_valid;
	
endmodule





//////////////////////////////////////////////////////////////////////
// File Downloaded from http://www.nandland.com
//////////////////////////////////////////////////////////////////////
// This file contains the UART Receiver.  This receiver is able to
// receive 8 bits of serial data, one start bit, one stop bit,
// and no parity bit.  When receive is complete o_rx_dv will be
// driven high for one clock cycle.
// 
// Set Parameter CLKS_PER_BIT as follows:
// CLKS_PER_BIT = (Frequency of i_Clock)/(Frequency of UART)
// Example: 10 MHz Clock, 115200 baud UART
// (10000000)/(115200) = 87
  
// 50E6/1200 = 41666

module uart_rx 
  (
   input        i_Clock,
   input        i_Rx_Serial,
   output       o_Rx_DV, //High for one clock cycle when receive complete.
   output [7:0] o_Rx_Byte
   );
    
  parameter CLKS_PER_BIT = 41666; //87;
  parameter s_IDLE         = 3'b000;
  parameter s_RX_START_BIT = 3'b001;
  parameter s_RX_DATA_BITS = 3'b010;
  parameter s_RX_STOP_BIT  = 3'b011;
  parameter s_CLEANUP      = 3'b100;
   
  reg           r_Rx_Data_R = 1'b1; //Receive bit is double-buffered.
  reg           r_Rx_Data   = 1'b1; //Held here.
   
  reg [15:0]    r_Clock_Count = 0; //reg [7:0]     r_Clock_Count = 0;
  reg [2:0]     r_Bit_Index   = 0; //8 bits total
  reg [7:0]     r_Rx_Byte     = 0;
  reg           r_Rx_DV       = 0;
  reg [2:0]     r_SM_Main     = 0;
   
  // Purpose: Double-register the incoming data.
  // This allows it to be used in the UART RX Clock Domain.
  // (It removes problems caused by metastability)
  always @(posedge i_Clock)
    begin
      r_Rx_Data_R <= i_Rx_Serial;
      r_Rx_Data   <= r_Rx_Data_R;
    end
   
   
  // Purpose: Control RX state machine
  always @(posedge i_Clock)
    begin
       
      case (r_SM_Main)
        s_IDLE :
          begin
            r_Rx_DV       <= 1'b0;
            r_Clock_Count <= 0;
            r_Bit_Index   <= 0;
             
            if (r_Rx_Data == 1'b0)          // Start bit detected
              r_SM_Main <= s_RX_START_BIT;
            else
              r_SM_Main <= s_IDLE;
          end
         
        // Check middle of start bit to make sure it's still low
        s_RX_START_BIT :
          begin
            if (r_Clock_Count == (CLKS_PER_BIT-1)/2)
              begin
                if (r_Rx_Data == 1'b0)
                  begin
                    r_Clock_Count <= 0;  // reset counter, found the middle
                    r_SM_Main     <= s_RX_DATA_BITS;
                  end
                else
                  r_SM_Main <= s_IDLE;
              end
            else
              begin
                r_Clock_Count <= r_Clock_Count + 1;
                r_SM_Main     <= s_RX_START_BIT;
              end
          end // case: s_RX_START_BIT
         
         
        // Wait CLKS_PER_BIT-1 clock cycles to sample serial data
        s_RX_DATA_BITS :
          begin
            if (r_Clock_Count < CLKS_PER_BIT-1)
              begin
                r_Clock_Count <= r_Clock_Count + 1;
                r_SM_Main     <= s_RX_DATA_BITS;
              end
            else
              begin
                r_Clock_Count          <= 0;
                r_Rx_Byte[r_Bit_Index] <= r_Rx_Data;
                 
                // Check if we have received all bits
                if (r_Bit_Index < 7)
                  begin
                    r_Bit_Index <= r_Bit_Index + 1;
                    r_SM_Main   <= s_RX_DATA_BITS;
                  end
                else
                  begin
                    r_Bit_Index <= 0;
                    r_SM_Main   <= s_RX_STOP_BIT;
                  end
              end
          end // case: s_RX_DATA_BITS
     
     
        // Receive Stop bit.  Stop bit = 1
        s_RX_STOP_BIT :
          begin
            // Wait CLKS_PER_BIT-1 clock cycles for Stop bit to finish
            if (r_Clock_Count < CLKS_PER_BIT-1)
              begin
                r_Clock_Count <= r_Clock_Count + 1;
                r_SM_Main     <= s_RX_STOP_BIT;
              end
            else
              begin
                r_Rx_DV       <= 1'b1;
                r_Clock_Count <= 0;
                r_SM_Main     <= s_CLEANUP;
              end
          end // case: s_RX_STOP_BIT
     
         
        // Stay here 1 clock
        s_CLEANUP :
          begin
            r_SM_Main <= s_IDLE;
            r_Rx_DV   <= 1'b0;
          end
         
         
        default :
          r_SM_Main <= s_IDLE;
         
      endcase
    end   
   
  assign o_Rx_DV   = r_Rx_DV;
  assign o_Rx_Byte = r_Rx_Byte;
   
endmodule // uart_rx




//////////////////////////////////////////////////////////////////////
// File Downloaded from http://www.nandland.com
//////////////////////////////////////////////////////////////////////
// This file contains the UART Transmitter.  This transmitter is able
// to transmit 8 bits of serial data, one start bit, one stop bit,
// and no parity bit.  When transmit is complete o_Tx_done will be
// driven high for one clock cycle.
//
// Set Parameter CLKS_PER_BIT as follows:
// CLKS_PER_BIT = (Frequency of i_Clock)/(Frequency of UART)
// Example: 10 MHz Clock, 115200 baud UART
// (10000000)/(115200) = 87
  
// 50E6/1200 = 41666
module uart_tx 
  (
   input       i_Clock,
   input       i_Tx_DV,		//Pulse high to begin transmission.
   input [7:0] i_Tx_Byte, 	//Byte to be transmitted.
   output      o_Tx_Active,	//High when transmitting.
   output reg  o_Tx_Serial, //Serial output.
   output      o_Tx_Done //High for one clock cycle when done.
   );
	
  parameter CLKS_PER_BIT = 41666; //87;
  parameter s_IDLE         = 3'b000;
  parameter s_TX_START_BIT = 3'b001;
  parameter s_TX_DATA_BITS = 3'b010;
  parameter s_TX_STOP_BIT  = 3'b011;
  parameter s_CLEANUP      = 3'b100;
   
  reg [2:0]    r_SM_Main     = 0;
  reg [15:0]    r_Clock_Count = 0; //reg [7:0]    r_Clock_Count = 0;
  reg [2:0]    r_Bit_Index   = 0;
  reg [7:0]    r_Tx_Data     = 0;
  reg          r_Tx_Done     = 0;
  reg          r_Tx_Active   = 0;
     
  always @(posedge i_Clock)
    begin
       
      case (r_SM_Main)
        s_IDLE :
          begin
            o_Tx_Serial   <= 1'b1;         // Drive Line High for Idle
            r_Tx_Done     <= 1'b0;		//High for one clock cycle when done.
            r_Clock_Count <= 0;
            r_Bit_Index   <= 0;
             
            if (i_Tx_DV == 1'b1)
              begin
                r_Tx_Active <= 1'b1;
                r_Tx_Data   <= i_Tx_Byte;
                r_SM_Main   <= s_TX_START_BIT;
              end
            else
              r_SM_Main <= s_IDLE;
          end // case: s_IDLE
         
         
        // Send out Start Bit. Start bit = 0
        s_TX_START_BIT :
          begin
            o_Tx_Serial <= 1'b0;
             
            // Wait CLKS_PER_BIT-1 clock cycles for start bit to finish
            if (r_Clock_Count < CLKS_PER_BIT-1)
              begin
                r_Clock_Count <= r_Clock_Count + 1;
                r_SM_Main     <= s_TX_START_BIT;
              end
            else
              begin
                r_Clock_Count <= 0;
                r_SM_Main     <= s_TX_DATA_BITS;
              end
          end // case: s_TX_START_BIT
         
         
        // Wait CLKS_PER_BIT-1 clock cycles for data bits to finish         
        s_TX_DATA_BITS :
          begin
            o_Tx_Serial <= r_Tx_Data[r_Bit_Index];
             
            if (r_Clock_Count < CLKS_PER_BIT-1)
              begin
                r_Clock_Count <= r_Clock_Count + 1;
                r_SM_Main     <= s_TX_DATA_BITS;
              end
            else
              begin
                r_Clock_Count <= 0;
                 
                // Check if we have sent out all bits
                if (r_Bit_Index < 7)
                  begin
                    r_Bit_Index <= r_Bit_Index + 1;
                    r_SM_Main   <= s_TX_DATA_BITS;
                  end
                else
                  begin
                    r_Bit_Index <= 0;
                    r_SM_Main   <= s_TX_STOP_BIT;
                  end
              end
          end // case: s_TX_DATA_BITS
         
         
        // Send out Stop bit.  Stop bit = 1
        s_TX_STOP_BIT :
          begin
            o_Tx_Serial <= 1'b1;
             
            // Wait CLKS_PER_BIT-1 clock cycles for Stop bit to finish
            if (r_Clock_Count < CLKS_PER_BIT-1)
              begin
                r_Clock_Count <= r_Clock_Count + 1;
                r_SM_Main     <= s_TX_STOP_BIT;
              end
            else
              begin
                r_Tx_Done     <= 1'b1;
                r_Clock_Count <= 0;
                r_SM_Main     <= s_CLEANUP;
                r_Tx_Active   <= 1'b0;
              end
          end // case: s_Tx_STOP_BIT
         
         
        // Stay here 1 clock
        s_CLEANUP :
          begin
            r_Tx_Done <= 1'b1;
            r_SM_Main <= s_IDLE;
          end
         
         
        default :
          r_SM_Main <= s_IDLE;
         
      endcase
    end
 
  assign o_Tx_Active = r_Tx_Active;
  assign o_Tx_Done   = r_Tx_Done;
   
endmodule



//////////////////////////////////////////////////////////////////////
// File Downloaded from http://www.nandland.com
//////////////////////////////////////////////////////////////////////
 
// This testbench will exercise both the UART Tx and Rx.
// It sends out byte 0xAB over the transmitter
// It then exercises the receive by receiving byte 0x3F
`timescale 1ns/10ps
 
//`include "uart_tx.v"
//`include "uart_rx.v"
 
module uart_tb ();
 
  // Testbench uses a 10 MHz clock
  // Want to interface to 115200 baud UART
  // 10000000 / 115200 = 87 Clocks Per Bit.
  parameter c_CLOCK_PERIOD_NS = 100;
  parameter c_CLKS_PER_BIT    = 87;
  parameter c_BIT_PERIOD      = 8600;
   
  reg r_Clock = 0;
  reg r_Tx_DV = 0;
  wire w_Tx_Done;
  reg [7:0] r_Tx_Byte = 0;
  reg r_Rx_Serial = 1;
  wire [7:0] w_Rx_Byte;
   
 
  // Takes in input byte and serializes it 
  task UART_WRITE_BYTE;
    input [7:0] i_Data;
    integer     ii;
    begin
       
      // Send Start Bit
      r_Rx_Serial <= 1'b0;
      #(c_BIT_PERIOD);
      #1000;
       
       
      // Send Data Byte
      for (ii=0; ii<8; ii=ii+1)
        begin
          r_Rx_Serial <= i_Data[ii];
          #(c_BIT_PERIOD);
        end
       
      // Send Stop Bit
      r_Rx_Serial <= 1'b1;
      #(c_BIT_PERIOD);
     end
  endtask // UART_WRITE_BYTE
   
   
  uart_rx #(.CLKS_PER_BIT(c_CLKS_PER_BIT)) UART_RX_INST
    (.i_Clock(r_Clock),
     .i_Rx_Serial(r_Rx_Serial),
     .o_Rx_DV(),
     .o_Rx_Byte(w_Rx_Byte)
     );
   
  uart_tx #(.CLKS_PER_BIT(c_CLKS_PER_BIT)) UART_TX_INST
    (.i_Clock(r_Clock),
     .i_Tx_DV(r_Tx_DV),
     .i_Tx_Byte(r_Tx_Byte),
     .o_Tx_Active(),
     .o_Tx_Serial(),
     .o_Tx_Done(w_Tx_Done)
     );
 
   
  always
    #(c_CLOCK_PERIOD_NS/2) r_Clock <= !r_Clock;
 
   
  // Main Testing:
  initial
    begin
       
      // Tell UART to send a command (exercise Tx)
      @(posedge r_Clock);
      @(posedge r_Clock);
      r_Tx_DV <= 1'b1;
      r_Tx_Byte <= 8'hAB;
      @(posedge r_Clock);
      r_Tx_DV <= 1'b0;
      @(posedge w_Tx_Done);
       
      // Send a command to the UART (exercise Rx)
      @(posedge r_Clock);
      UART_WRITE_BYTE(8'h3F);
      @(posedge r_Clock);
             
      // Check that the correct command was received
      if (w_Rx_Byte == 8'h3F)
        $display("Test Passed - Correct Byte Received");
      else
        $display("Test Failed - Incorrect Byte Received");
       
    end
   
endmodule