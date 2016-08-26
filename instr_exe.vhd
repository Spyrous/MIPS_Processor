----------------------------------------------------------------------------------------
-- Title      : Instruction Execute
-- Project    : MIPS Processor                                                         
----------------------------------------------------------------------------------------
-- File       : instr_exe.vhd                                                          
-- Author     : Spyros Chiotakis <spyros.chiotakis@gmail.com>                         
-- Company    :                                                                       
-- Created    : 2016-05-19                                                            
-- Last update: 2016-08-26
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
        DATA_WIDTH    : integer := 32
    );
    
    port (
        -- Global clock signal
        CLK_IN : in std_logic;
        -- Global reset signal active high
        RST_IN : in std_logic;

        
        OPCODE_IN : in std_logic_vector(5 downto 0);

        -- RS (Source Operand)
        RS_IN     : in std_logic_vector(DATA_WIDTH-1 downto 0);
        -- RT (Second Operand)
        RT_IN     : in std_logic_vector(DATA_WIDTH-1 downto 0);
        -- Shift Amount
        SHAMT_IN  : in std_logic_vector(4 downto 0);
        -- Function to be executed if instruction is R-Type
        FUNCT_IN  : in std_logic_vector(5 downto 0);
        -- Immediate Operand if instruction is I-Type
        IMM_IN    : in std_logic_vector(15 downto 0)

);
end instr_exe;



    
--*******************************************************************--
--                     A R C H I T E C T U R E                       --
--*******************************************************************--
architecture Behavioral of instr_exe is


-- Signals
signal alu_res_s : std_logic_vector(DATA_WIDTH-1 downto 0);
    
--*******************************************************************--
--          B E G I N  F O R M A L  A R C H I T E C T U R E          --
--*******************************************************************--        
begin

    process(CLK_IN)
    begin
        if (RST_IN = '1') then
            alu_res_s <= (others => '0');
        elsif (rising_edge(CLK_IN)) then
            -- If the opcode is of type R then check the
            -- funct field to execute an operation
            if (OPCODE_IN = R_TYPE_OP) then
                case FUNCT_IN is
                    -- Arithmetic Operations
                    when ADD_OP =>
                        alu_res_s <= std_logic_vector(unsigned(RS_IN) + unsigned(RT_IN));
                    when ADDU_OP =>
                        alu_res_s <= std_logic_vector(unsigned(RS_IN) + unsigned(RT_IN));
                    when SUB_OP =>
                        alu_res_s <= std_logic_vector(unsigned(RS_IN) - unsigned(RT_IN));
                    when SUBU_OP =>
                        alu_res_s <= std_logic_vector(unsigned(RS_IN) - unsigned(RT_IN));
                    -- Logic Operations
                    when AND_OP =>
                        alu_res_s <= RS_IN and RT_IN;
                    when OR_OP =>
                        alu_res_s <= RS_IN or RT_IN;
                    when XOR_OP =>
                        alu_res_s <= RS_IN xor RT_IN;
                    when NOR_OP =>
                        alu_res_s <= RS_IN nor RT_IN;
                    -- Shift Left Operations
                    when SLL_OP =>
                        alu_res_s <= std_logic_vector(unsigned(RT_IN) sll to_integer(unsigned(SHAMT_IN)));
                    when SLLV_OP =>
                        alu_res_s <= std_logic_vector(unsigned(RT_IN) sll to_integer(unsigned(RS_IN(4 downto 0))));
                    -- Shift Right Operations
                    when SRL_OP =>
                        alu_res_s <= std_logic_vector(unsigned(RT_IN) srl to_integer(unsigned(SHAMT_IN)));
                    when SRLV_OP =>
                        alu_res_s <= std_logic_vector(unsigned(RT_IN) srl to_integer(unsigned(RS_IN(4 downto 0))));
                    when SRA_OP =>
                        
                    when SRAV_OP =>

                        
                    -- Set on Less Than Operations
                    when SLT_OP =>
                        if (signed(RS_IN) < signed(RT_IN)) then
                            alu_res_s <= x"00000001";
                        else
                            alu_res_s <= x"00000000";
                        end if;
                    when SLTU_OP =>
                        if (unsigned(RS_IN) < unsigned(RT_IN)) then
                            alu_res_s <= x"00000001";
                        else
                            alu_res_s <= x"00000000";
                        end if;
                        
                    when others =>
                        
                end case;
             
            end if;
        -- If it's not R-Type use the opcode to execute
        -- the appropriate operation (I-Type or J-Type)
            case OPCODE_IN is
                when ADDI_OP  =>
                    alu_res_s <= std_logic_vector(unsigned(RS_IN) + unsigned(x"0000" & IMM_IN));
                when ADDIU_OP =>
                    alu_res_s <= std_logic_vector(unsigned(RS_IN) + unsigned(x"0000" & IMM_IN));
                when ANDI_OP  =>
                    alu_res_s <= std_logic_vector(unsigned(RS_IN) and unsigned(x"0000" & IMM_IN));
                when others =>

            end case;
                
        end if;
    end process;
end Behavioral;
