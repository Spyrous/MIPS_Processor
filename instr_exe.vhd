----------------------------------------------------------------------------------------
-- Title      : Instruction Execute
-- Project    : MIPS Processor                                                         
----------------------------------------------------------------------------------------
-- File       : instr_exe.vhd                                                          
-- Author     : Spyros Chiotakis <spyros.chiotakis@gmail.com>                         
-- Company    :                                                                       
-- Created    : 2016-05-19                                                            
-- Last update: 2016-09-10
-- Platform   : Windows 10 Professional                                            
-- Standard   : VHDL'93/02                                                            
----------------------------------------------------------------------------------------
-- Description: Executes the instruction specified by the decode stage.             
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
use work.MIPS_Instructions_Pack.ALL;


--*******************************************************************--
--                           E N T I T Y                             --
--*******************************************************************--
entity instr_exe is
    generic (
        DATA_WIDTH : integer := 32;
        ADDR_WIDTH : integer := 32
    );
    
    port (
        -- Global clock signal
        CLK_IN : in std_logic;
        -- Global reset signal active high
        RST_IN : in std_logic;

        -- Opcode to be decoded from first 6 bits of the instruction
        OPCODE_EXE_IN : in std_logic_vector(5 downto 0);


        ----------------------------------------------
        -- Control Signals Received from Decode Stage
        ----------------------------------------------
        -- Controls if we write at a register after writeback stage
        REG_WRITE_EXE_IN  : in std_logic;
        -- Chooses between ALU result or memory data
        -- to be written back at registers
        MEM_TO_REG_EXE_IN : in std_logic;
        -- Controls reads or writes at memory stage
        MEM_WRITE_EXE_IN  : in std_logic;
        -- Controls if one of the ALU sources will be a register
        -- or the immidiate field
        ALU_SRC_EXE_IN    : in std_logic;
        -- Controls where the results from writeback stage go
        -- either RS register or RT
        REG_DST_EXE_IN    : in std_logic;
        -- Program counter from fetch stage
        PC_PLUS4_EXE_IN   : in unsigned(ADDR_WIDTH-1 downto 0);
        
                   
        ----------------------------------------
        -- Control Signals Sent to Memory Stage
        ----------------------------------------
        -- Controls if we write at a register after writeback stage
        REG_WRITE_EXE_OUT  : out std_logic;
        -- Chooses between ALU result or memory data
        -- to be written back at registers
        MEM_TO_REG_EXE_OUT : out std_logic;
        -- Controls reads or writes at memory stage
        MEM_WRITE_EXE_OUT  : out std_logic;
        -- Program counter select in fetch stage
        PC_SEL_EXE_OUT     : out std_logic;
        -- Program counter branch address forwarded to fetch stage
        PC_BRANCH_EXE_OUT : out unsigned(ADDR_WIDTH-1 downto 0);
        
    
        ------------------------------------------------
        -- Registers values and numbers received from
        -- decode stage
        ------------------------------------------------ 
        -- Decoded register number (0-31) of RT and RD
        -- Used for writeback stage to write to a register
        -- The result of writeback will go either to RT or RD
        -- depending on the instruction decoded
        RT_NUM_EXE_IN  : in std_logic_vector(4 downto 0);
        RD_NUM_EXE_IN  : in std_logic_vector(4 downto 0);
        -- RS (Source Operand)
        RS_VAL_EXE_IN  : in std_logic_vector(DATA_WIDTH-1 downto 0);
        -- RT (Second Operand)
        RT_VAL_EXE_IN  : in std_logic_vector(DATA_WIDTH-1 downto 0);
        
        ------------------------------------------------
        -- Signals received from decoded stage they are
        -- used at the ALU of the execution stage
        ------------------------------------------------  
        -- Shift Amount
        SHAMT_EXE_IN   : in std_logic_vector(4 downto 0);
        -- Function to be executed if instruction is R-Type
        FUNCT_EXE_IN   : in std_logic_vector(5 downto 0);
        -- Immediate Operand if instruction is I-Type
        IMM_EXE_IN     : in std_logic_vector(DATA_WIDTH-1 downto 0)

);
end instr_exe;



    
--*******************************************************************--
--                     A R C H I T E C T U R E                       --
--*******************************************************************--
architecture Behavioral of instr_exe is


-------------------------------------------------------------------------------
-- Signals
-------------------------------------------------------------------------------
signal alu_res_s : std_logic_vector(DATA_WIDTH-1 downto 0);



--*******************************************************************--
--          B E G I N  F O R M A L  A R C H I T E C T U R E          --
--*******************************************************************--        
begin

    -- Signals forwarded to memory stage
    REG_WRITE_EXE_OUT  <= REG_WRITE_EXE_IN;
    MEM_TO_REG_EXE_OUT <= MEM_TO_REG_EXE_IN;
    MEM_WRITE_EXE_OUT  <= MEM_WRITE_EXE_IN;

    PC_BRANCH_EXE_OUT  <= PC_PLUS4_EXE_IN + unsigned(IMM_EXE_IN(15 downto 0) & "00");   
    -----------------------------------------------------------------
    --                   Instruction Execute                 
    --                                                           
    --  Description:                                              
    --       Executes the instruction specified by opcode and funct
    --       fields. Also consumes some of the control signals and 
    --       generates results for the memory stage.
    -----------------------------------------------------------------
    instr_exe_PROC: process(CLK_IN)
    begin
        if (RST_IN = '1') then
            alu_res_s <= (others => '0');
        elsif (rising_edge(CLK_IN)) then
            -- If the opcode is of type R then check the
            -- funct field to execute an operation
            if (OPCODE_EXE_IN = R_TYPE_OP) then
                case FUNCT_EXE_IN is
                    -- Arithmetic Operations
                    when ADD_OP =>
                        alu_res_s <= std_logic_vector(unsigned(RS_VAL_EXE_IN) + unsigned(RT_VAL_EXE_IN));
                    when ADDU_OP =>
                        alu_res_s <= std_logic_vector(unsigned(RS_VAL_EXE_IN) + unsigned(RT_VAL_EXE_IN));
                    when SUB_OP =>
                        alu_res_s <= std_logic_vector(unsigned(RS_VAL_EXE_IN) - unsigned(RT_VAL_EXE_IN));
                    when SUBU_OP =>
                        alu_res_s <= std_logic_vector(unsigned(RS_VAL_EXE_IN) - unsigned(RT_VAL_EXE_IN));
                    -- Logic Operations
                    when AND_OP =>
                        alu_res_s <= RS_VAL_EXE_IN and RT_VAL_EXE_IN;
                    when OR_OP =>
                        alu_res_s <= RS_VAL_EXE_IN or RT_VAL_EXE_IN;
                    when XOR_OP =>
                        alu_res_s <= RS_VAL_EXE_IN xor RT_VAL_EXE_IN;
                    when NOR_OP =>
                        alu_res_s <= RS_VAL_EXE_IN nor RT_VAL_EXE_IN;
                    -- Shift Left Operations
                    when SLL_OP =>
                        alu_res_s <= std_logic_vector(unsigned(RT_VAL_EXE_IN) sll to_integer(unsigned(SHAMT_EXE_IN)));
                    when SLLV_OP =>
                        alu_res_s <= std_logic_vector(unsigned(RT_VAL_EXE_IN) sll to_integer(unsigned(RS_VAL_EXE_IN(4 downto 0))));
                    -- Shift Right Operations
                    when SRL_OP =>
                        alu_res_s <= std_logic_vector(unsigned(RT_VAL_EXE_IN) srl to_integer(unsigned(SHAMT_EXE_IN)));
                    when SRLV_OP =>
                        alu_res_s <= std_logic_vector(unsigned(RT_VAL_EXE_IN) srl to_integer(unsigned(RS_VAL_EXE_IN(4 downto 0))));
                    when SRA_OP =>
                        
                    when SRAV_OP =>

                        
                    -- Set on Less Than Operations
                    when SLT_OP =>
                        if (signed(RS_VAL_EXE_IN) < signed(RT_VAL_EXE_IN)) then
                            alu_res_s <= x"00000001";
                        else
                            alu_res_s <= x"00000000";
                        end if;
                    when SLTU_OP =>
                        if (unsigned(RS_VAL_EXE_IN) < unsigned(RT_VAL_EXE_IN)) then
                            alu_res_s <= x"00000001";
                        else
                            alu_res_s <= x"00000000";
                        end if;
                        
                    when others =>
                        
                end case;
             
            end if;
        -- If it's not R-Type use the opcode to execute
        -- the appropriate operation (I-Type or J-Type)
            case OPCODE_EXE_IN is
                when ADDI_OP  =>
                    alu_res_s <= std_logic_vector(unsigned(RS_VAL_EXE_IN)  +  unsigned(IMM_EXE_IN));
                when ADDIU_OP =>
                    alu_res_s <= std_logic_vector(unsigned(RS_VAL_EXE_IN)  +  unsigned(IMM_EXE_IN));
                when ANDI_OP  =>
                    alu_res_s <= std_logic_vector(unsigned(RS_VAL_EXE_IN) and unsigned(IMM_EXE_IN));
                when ORI_OP   =>
                    alu_res_s <= std_logic_vector(unsigned(RS_VAL_EXE_IN) or  unsigned(IMM_EXE_IN));
                when XORI_OP  =>
                    alu_res_s <= std_logic_vector(unsigned(RS_VAL_EXE_IN) xor unsigned(IMM_EXE_IN));
                when LW_OP    =>
                    alu_res_s <= std_logic_vector(unsigned(RS_VAL_EXE_IN)  +  unsigned(IMM_EXE_IN));
                when SW_OP    =>
                    alu_res_s <= std_logic_vector(unsigned(RS_VAL_EXE_IN)  +  unsigned(IMM_EXE_IN));
                when BEQ_OP =>
                    -- If RS = RT 
                    if ( RS_VAL_EXE_IN = RT_VAL_EXE_IN ) then
                        -- Branch
                        PC_SEL_EXE_OUT <= '1';
                    else
                        -- PC + 4
                        PC_SEL_EXE_OUT <= '0';
                    end if;
                when others =>

            end case;
                
        end if;
    end process;
end Behavioral;
