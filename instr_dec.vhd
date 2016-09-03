----------------------------------------------------------------------------------------
-- Title      : Instruction Decode                                                       
-- Project    : MIPS Processor                                                         
----------------------------------------------------------------------------------------
-- File       : instr_dec.vhd                                                          
-- Author     : Spyros Chiotakis <spyros.chiotakis@gmail.com>                         
-- Company    :                                                                       
-- Created    : 2016-05-16                                                            
-- Last update: 2016-09-03
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
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.MIPS_Instructions_Pack.all;


--*******************************************************************--
--                           E N T I T Y                             --
--*******************************************************************--
entity instr_dec is
    generic (
        REG_FILE_SIZE : integer := 32;
        ADDR_WIDTH    : integer := 32;
        DATA_WIDTH    : integer := 32
    );

    port (
        -- Global clock signal
        CLK_IN : in std_logic;
        -- Global reset signal active high
        RST_IN : in std_logic;

        -- Instruction to be decoded
        INSTR_TBD_IN      : in std_logic_vector(DATA_WIDTH-1 downto 0);

        -- Program counter from fetch stage
        PC_PLUS4_DEC_IN   : in unsigned(ADDR_WIDTH-1 downto 0);
        -- Program counter select signal for fetch stage
        PC_SEL_DEC_OUT    : out std_logic;
        -- Program counter branch address forwarded to fetch stage
        PC_BRANCH_DEC_OUT : out unsigned(ADDR_WIDTH-1 downto 0);
        
        -- Determines the data to be written in the register specified by
        -- the writeback stage
        WB_TO_DEC_DATA_IN : in std_logic_vector(DATA_WIDTH-1 downto 0);
        -- Determines which register we write to from the writeback stage
        WRITE_REG_DEC_IN  : in std_logic_vector(4 downto 0);

        
        ----------------------------
        -- Control Signals Received
        ----------------------------
        -- Controls if we write at a register after writeback stage
        REG_WRITE_DEC_IN : in std_logic;
        
        
        ------------------------
        -- Control Signals Sent
        ------------------------
        -- Controls if we write at a register after writeback stage
        REG_WRITE_DEC_OUT  : out std_logic;
        -- Chooses between ALU result or memory data
        -- to be written back at registers
        MEM_TO_REG_DEC_OUT : out std_logic;
        -- Controls reads or writes at memory stage
        MEM_WRITE_DEC_OUT  : out std_logic;
        -- Controls if one of the ALU sources will be a register
        -- or the immidiate field
        ALU_SRC_DEC_OUT    : out std_logic;
        -- Controls where the results from writeback stage go
        -- either RS register or RT
        REG_DST_DEC_OUT    : out std_logic;



        
        -- Instruction opcode used for execution stage
        OPCODE_OUT : out std_logic_vector(5 downto 0);

        -- Registers used for execution
        -- RS (Source Operand)
        RS_OUT     : out std_logic_vector(DATA_WIDTH-1 downto 0);
        -- RT (Second Operand)
        RT_OUT     : out std_logic_vector(DATA_WIDTH-1 downto 0);
        -- RD (Destination Operand)
        RD_OUT     : out std_logic_vector(DATA_WIDTH-1 downto 0);
        
        -- Shift Amount
        SHAMT_OUT  : out std_logic_vector(4 downto 0);
        -- Function to be executed if instruction is R-Type
        FUNCT_OUT  : out std_logic_vector(5 downto 0);
        -- Immediate Operand if instruction is I-Type
        IMM_OUT    : out std_logic_vector(31 downto 0)
    );
end instr_dec;


--*******************************************************************--
--                     A R C H I T E C T U R E                       --
--*******************************************************************--
architecture Behavioral of instr_dec is


-------------------------------------------------------------------------------
-- Signals
-------------------------------------------------------------------------------
type registerFile is array(0 to REG_FILE_SIZE-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
signal reg_file_s : registerFile;
-- Used to read the opcode and decide the fields to be decoded
signal opcode_s   : std_logic_vector(5 downto 0);
-- The sign extended immediate field for I-Type instructions
signal signed_imm_s : std_logic_vector(DATA_WIDTH-1 downto 0);




--*******************************************************************--
--          B E G I N  F O R M A L  A R C H I T E C T U R E          --
--*******************************************************************--    
begin
    
    -- Where to branch to relative to the current program counter
    PC_BRANCH_DEC_OUT <= PC_PLUS4_DEC_IN + to_integer(unsigned(signed_imm_s));


    
    -----------------------------------------------------------------
    --                   Instruction Decode                 
    --                                                           
    --  Description:                                              
    --       Decodes the instruction from the cache memory depending
    --       on the type of the instruction(R/I/J-Type). The results
    --       are then forwarded for execution at the execution stage.
    -----------------------------------------------------------------
    instr_dec_PROC: process(CLK_IN, RST_IN)
    begin
        if (RST_IN = '1') then
            -- Set all 32 registers to 0 except register 0 which is initiated to hex 11111111
            reg_file_s <= (0 => x"11111111",
                           others => (others => '0'));
            signed_imm_s <= (others => '0');
            
        elsif (rising_edge(CLK_IN)) then
           
            opcode_s <= INSTR_TBD_IN(31 downto 26);

            -- If the MSB (Most Significant Bit) of our immediate field is '0' we extend
            -- with zeroes if it's '1' we extend ones
            if (INSTR_TBD_IN(15) = '0') then
                -- Concatenating 14 bits + immediate 16 bits + 2 zeroes by shifting to left
                signed_imm_s <= x"000" & "00" & INSTR_TBD_IN(15 downto 0) & "00";                
            else
                -- Concatenating 14 bits + immediate 16 bits + 2 zeroes by shifting to left
                signed_imm_s <= x"FFF" & "11" & INSTR_TBD_IN(15 downto 0) & "00";               
            end if;
            
            case (opcode_s) is
                -- If instruction R-Type decode the following fields
                when R_TYPE_OP =>             
                    OPCODE_OUT <= INSTR_TBD_IN(31 downto 26);
                    RS_OUT     <= reg_file_s(to_integer(unsigned(INSTR_TBD_IN(25 downto 21))));
                    RT_OUT     <= reg_file_s(to_integer(unsigned(INSTR_TBD_IN(20 downto 16))));
                    RD_OUT     <= reg_file_s(to_integer(unsigned(INSTR_TBD_IN(15 downto 11))));
                    SHAMT_OUT  <= INSTR_TBD_IN(10 downto 6);
                    FUNCT_OUT  <= INSTR_TBD_IN(5  downto 0);


                    -- Control signal configuration for R-Type
                    REG_WRITE_DEC_OUT  <= '1';
                    MEM_TO_REG_DEC_OUT <= '0';
                    MEM_WRITE_DEC_OUT  <= '0';
                    ALU_SRC_DEC_OUT    <= '0';
                    
                        
                        
                -- If instruction is I-Type decode the following fields
                when ADDI_OP|ANDI_OP =>                    
                    OPCODE_OUT <= opcode_s;
                    RS_OUT     <= reg_file_s(to_integer(unsigned(INSTR_TBD_IN(25 downto 21))));
                    RT_OUT     <= reg_file_s(to_integer(unsigned(INSTR_TBD_IN(20 downto 16))));
                    IMM_OUT    <= x"0000" & INSTR_TBD_IN(15 downto 0);

                when BEQ_OP =>
                    OPCODE_OUT <= opcode_s;
                    RS_OUT     <= reg_file_s(to_integer(unsigned(INSTR_TBD_IN(25 downto 21))));
                    RT_OUT     <= reg_file_s(to_integer(unsigned(INSTR_TBD_IN(20 downto 16))));

                    
                when others =>
                    
            end case;
            
            if (REG_WRITE_DEC_IN = '1') then
                reg_file_s(to_integer(unsigned(WRITE_REG_DEC_IN))) <= WB_TO_DEC_DATA_IN;
            else
                -- Do nothing
            end if;
        end if;
    end process;
end Behavioral;
