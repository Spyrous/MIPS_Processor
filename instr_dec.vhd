----------------------------------------------------------------------------------------
-- Title      : Instruction Decode                                                       
-- Project    : MIPS Processor                                                         
----------------------------------------------------------------------------------------
-- File       : instr_dec.vhd                                                          
-- Author     : Spyros Chiotakis <spyros.chiotakis@gmail.com>                         
-- Company    :                                                                       
-- Created    : 2016-05-16                                                            
-- Last update: 2017-02-20
-- Platform   : Windows 10 Professional                                            
-- Standard   : VHDL'93/02                                                            
----------------------------------------------------------------------------------------
-- Description: Decodes the instruction coming from the instruction cache and forwards
--              the results to the execution stage
----------------------------------------------------------------------------------------
-- Copyright (c) 2016, Spyros Chiotakis                                               
-- All rights reserved.                                                               
--                                                                                   
-- Redistribution and use in source and binary forms, with or without                 
-- modification, are permitted provided that the following conditions are met:        
--                                                                                    
-- 1. Redistributions of source code must retain the above copyright notice,          
--    this list of conditions and the following disclaimer.                           
--                                                                                    
-- 2. Redistributions in binary form must reproduce the above copyright notice,       
--    this list of conditions and the following disclaimer in the documentation       
--    and/or other materials provided with the distribution.                          
--                                                                                    
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"        
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE          
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE         
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE          
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL  
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR         
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED  
-- AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT     
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS      
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                       
----------------------------------------------------------------------------------------

-----------------------------------------------------------------------
-- libraries                                                         --
-----------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

LIBRARY WORK;
USE WORK.MIPS_Instructions_Pack.ALL;


--*******************************************************************--
--                           E N T I T Y                             --
--*******************************************************************--
ENTITY instr_dec IS
    GENERIC (
        C_REG_FILE_SIZE : INTEGER := 32;
        C_ADDR_WIDTH    : INTEGER := 32;
        C_DATA_WIDTH    : INTEGER := 32
    );

    PORT (
        -- Global clock signal
        I_CLK : IN STD_LOGIC;
        -- Global reset signal active high
        I_RST : IN STD_LOGIC;

        -- Instruction to be decoded
        I_INSTR_TBD      : IN STD_LOGIC_VECTOR(C_DATA_WIDTH-1 DOWNTO 0);

        -- Program counter from fetch stage
        I_PC_PLUS4_DEC   : IN unsigned(C_ADDR_WIDTH-1 DOWNTO 0);
        -- Program counter select signal for fetch stage
        O_PC_SEL_DEC     : OUT STD_LOGIC;        


        -----------------------------------------
        -- Signals Received from Writeback Stage
        -----------------------------------------
        -- Determines the data to be written in the register specified by
        -- the writeback stage
        I_WRITE_DATA_DEC : IN STD_LOGIC_VECTOR(C_DATA_WIDTH-1 DOWNTO 0);
        -- Determines which register we write to from the writeback stage
        I_WRITE_REG_DEC  : IN STD_LOGIC_VECTOR(4 DOWNTO 0);

        
        ----------------------------
        -- Control Signals Received
        ----------------------------
        -- Controls if we write at a register after writeback stage
        I_REG_WRITE_DEC : IN STD_LOGIC;
        
        
        ------------------------
        -- Control Signals Sent
        ------------------------
        -- Controls if we write at a register after writeback stage
        O_REG_WRITE_DEC  : OUT STD_LOGIC;
        -- Chooses between ALU result or memory data
        -- to be written back at registers
        O_MEM_TO_REG_DEC : OUT STD_LOGIC;
        -- Controls writes at memory stage
        O_MEM_WRITE_DEC  : OUT STD_LOGIC;
        -- Controls memory reads at memory stage
        O_MEM_READ_DEC   : OUT STD_LOGIC;
        -- Controls if one of the ALU sources will be a register
        -- or the immediate field
        O_ALU_SRC_DEC    : OUT STD_LOGIC;
        -- Controls where the results from writeback stage go
        -- either RS register or RT
        O_REG_DST_DEC    : OUT STD_LOGIC;
        
        -- Determines which of the 32 registers
        -- will be written after writeback stage
        O_WRITE_REG_DEC  : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);

        O_PC_PLUS4_DEC   : OUT UNSIGNED(C_ADDR_WIDTH-1 DOWNTO 0);
        -- Decoded register number (0-31) of RT and RD
        -- Used for writeback stage to write to a register
        -- The result of writeback will go either to RT or RD
        -- dependINg on the instruction decoded
        O_RT_NUM_DEC     : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
        O_RD_NUM_DEC     : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
        
        -- Instruction opcode used for execution stage
        O_OPCODE_DEC : OUT STD_LOGIC_VECTOR(5 DOWNTO 0);
        
        ---------------------------------------------------------
        -- Register values used in the ALU at execution stage
        ---------------------------------------------------------
        -- RS (Source Operand)
        O_RS_VAL_DEC     : OUT STD_LOGIC_VECTOR(C_DATA_WIDTH-1 DOWNTO 0);
        -- RT (Second Operand)
        O_RT_VAL_DEC     : OUT STD_LOGIC_VECTOR(C_DATA_WIDTH-1 DOWNTO 0);        

        
        ---------------------------------------------------------
        -- Signals used at the ALU of the execution stage
        ---------------------------------------------------------        
        -- Shift Amount
        O_SHAMT_DEC  : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
        -- Function to be executed if instruction is R-Type
        O_FUNCT_DEC  : OUT STD_LOGIC_VECTOR(5 DOWNTO 0);
        -- Immediate Operand if instruction is I-Type
        O_IMM_DEC    : OUT STD_LOGIC_VECTOR(C_DATA_WIDTH-1 DOWNTO 0)
    );
end instr_dec;


--*******************************************************************--
--                     A R C H I T E C T U R E                       --
--*******************************************************************--
ARCHITECTURE behavioral OF instr_dec IS


-------------------------------------------------------------------------------
-- Signals
-------------------------------------------------------------------------------
TYPE T_REGISTER_FILE IS ARRAY(0 TO C_REG_FILE_SIZE-1) OF STD_LOGIC_VECTOR(C_DATA_WIDTH-1 DOWNTO 0);
SIGNAL s_reg_file : T_REGISTER_FILE;
-- Used to read the opcode and decide the fields to be decoded
SIGNAL s_opcode   : STD_LOGIC_VECTOR(5 DOWNTO 0);
-- The sign extended immediate field for I-Type INstructions
SIGNAL s_signed_imm : STD_LOGIC_VECTOR(C_DATA_WIDTH-1 DOWNTO 0);




--*******************************************************************--
--          B E G I N  F O R M A L  A R C H I T E C T U R E          --
--*******************************************************************--    
BEGIN
 
    s_opcode <= I_INSTR_TBD(31 DOWNTO 26);

    
    -----------------------------------------------------------------
    --                   Instruction Decode                 
    --                                                           
    --  Description:                                              
    --       Decodes the instruction from the cache memory depending
    --       on the type of the instruction(R/I/J-Type). The results
    --       are then forwarded for execution at the execution stage.
    -----------------------------------------------------------------
    instr_dec_PROC: PROCESS(I_CLK, I_RST)
    BEGIN
        IF (I_RST = '1') THEN
            -- Set all 32 registers to 0 except register 0 which is initiated to hex 0000_0000
            s_reg_file       <= (0 => x"00000010",
                                 1 => x"00000001",
                                 2 => x"00000010",
                                 3 => x"00000011",
                                 OTHERS => (OTHERS => '0')
                                 );
            
            s_signed_imm     <= (OTHERS => '0');
            O_PC_SEL_DEC     <= '0';
            O_IMM_DEC        <= (OTHERS => '0');
            O_PC_PLUS4_DEC   <= (OTHERS => '0');
            O_REG_WRITE_DEC  <= '0';
            O_MEM_TO_REG_DEC <= '0';
            O_MEM_WRITE_DEC  <= '0';
            O_ALU_SRC_DEC    <= '0';
            O_PC_SEL_DEC     <= '0';
            O_REG_DST_DEC    <= '0';
            O_WRITE_REG_DEC  <= (OTHERS => '0');

            O_RT_NUM_DEC     <= (OTHERS => '0');
            O_RD_NUM_DEC     <= (OTHERS => '0');
            O_RS_VAL_DEC     <= (OTHERS => '0');
            O_RT_VAL_DEC     <= (OTHERS => '0');
            
        ELSIF (RISING_EDGE(I_CLK)) THEN
           
            -- Forward pc + 4 to the execute stage
            O_PC_PLUS4_DEC  <= I_PC_PLUS4_DEC;
            
            -- Opcode is decoded by all types of instructions
            O_OPCODE_DEC    <= I_INSTR_TBD(31 DOWNTO 26);
            
            -- R-Type decoded signals
            O_SHAMT_DEC     <= I_INSTR_TBD(10 DOWNTO 6);
            O_FUNCT_DEC     <= I_INSTR_TBD(5  DOWNTO 0);

            -- R-Type and I-Type decoded signals
            O_RS_VAL_DEC    <= s_reg_file(TO_INTEGER(UNSIGNED(I_INSTR_TBD(25 DOWNTO 21))));
            O_RT_VAL_DEC    <= s_reg_file(TO_INTEGER(UNSIGNED(I_INSTR_TBD(20 DOWNTO 16))));

            -- I-Type immediate decoded field
            O_IMM_DEC       <= s_signed_imm;

            -- Destination number register
            O_RT_NUM_DEC    <= I_INSTR_TBD(25 DOWNTO 21);
            O_RD_NUM_DEC    <= I_INSTR_TBD(20 DOWNTO 16);

            -- Writeback Stage
            IF (I_REG_WRITE_DEC = '1') THEN
                s_reg_file(TO_INTEGER(UNSIGNED(I_WRITE_REG_DEC))) <= I_WRITE_DATA_DEC;
            END IF;
            
            -- If the MSB (Most Significant Bit) of our immediate field is '0' we extend
            -- with zeroes if it's '1' we extend ones
            IF (I_INSTR_TBD(15) = '0') THEN
                -- Concatenating 14 bits + immediate 16 bits + 2 zeroes by shifting to left
                s_signed_imm <= x"000" & "00" & I_INSTR_TBD(15 DOWNTO 0) & "00";                
            ELSE
                -- Concatenating 14 bits + immediate 16 bits + 2 zeroes by shifting to left
                s_signed_imm <= x"FFF" & "11" & I_INSTR_TBD(15 DOWNTO 0) & "00";               
            END IF;
            
            CASE (s_opcode) IS
                -- If instruction R-Type decode the following fields
                WHEN R_TYPE_OP =>                                 
                    -- Control signal configuration for R-Type
                    O_REG_WRITE_DEC    <= '1';
                    O_MEM_TO_REG_DEC   <= '0';
                    O_MEM_WRITE_DEC  <= '0';
                    O_ALU_SRC_DEC    <= '0';
                    O_PC_SEL_DEC       <= '0';
                    O_REG_DST_DEC    <= '1';
                    O_WRITE_REG_DEC  <= I_INSTR_TBD(15 DOWNTO 11);
                -- If instruction is I-Type decode the following fields
                WHEN ADDI_OP|ANDI_OP =>
                    -- Destination register found in Instruction[20-16]
                    O_REG_DST_DEC    <= '0';

                    
                WHEN BEQ_OP =>

                WHEN OTHERS =>
                    
            END CASE;
            
        END IF;
    END PROCESS;

   -- s_reg_file(TO_INTEGER(UNSIGNED(I_WRITE_REG_DEC))) <= I_WRITE_DATA_DEC WHEN I_REG_WRITE_DEC = '1';

--    writeback_PROC: PROCESS(I_REG_WRITE_DEC, I_WRITE_DATA_DEC,
--                            I_WRITE_REG_DEC, I_RST)
--    BEGIN
--        IF (I_REG_WRITE_DEC = '1') THEN
--            s_reg_file(TO_INTEGER(UNSIGNED(I_WRITE_REG_DEC))) <= I_WRITE_DATA_DEC;
--        ELSE
--        -- Do nothing
--        END IF;
--    END PROCESS;
    
end Behavioral;
