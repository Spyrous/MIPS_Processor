----------------------------------------------------------------------------------------
-- Title      : Instruction Execute
-- Project    : MIPS Processor                                                         
----------------------------------------------------------------------------------------
-- File       : instr_exe.vhd                                                          
-- Author     : Spyros Chiotakis <spyros.chiotakis@gmail.com>                         
-- Company    :                                                                       
-- Created    : 2016-05-19                                                            
-- Last update: 2017-02-21
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
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

LIBRARY WORK;
USE WORK.MIPS_Instructions_Pack.ALL;


--*******************************************************************--
--                           E N T I T Y                             --
--*******************************************************************--
ENTITY instr_exe is 
    GENERIC (
        C_DATA_WIDTH : INTEGER := 32;
        C_ADDR_WIDTH : INTEGER := 32
    );
    
    PORT (
        -- Global clock signal
        I_CLK : IN STD_LOGIC;
        -- Global reset signal active high
        I_RST : IN STD_LOGIC;

        -- Opcode to be decoded from first 6 bits of the instruction
        I_OPCODE_EXE : IN STD_LOGIC_VECTOR(5 DOWNTO 0);


        ----------------------------------------------
        -- Control Signals Received from Decode Stage
        ----------------------------------------------
        -- Controls if we write at a register after writeback stage
        I_REG_WRITE_EXE  : IN STD_LOGIC;
        -- Chooses between ALU result or memory data
        -- to be written back at registers
        I_MEM_TO_REG_EXE : IN STD_LOGIC;
        -- Controls memory writes at memory stage
        I_MEM_WRITE_EXE  : IN STD_LOGIC;
        -- Controls memory reads at memory stage
        I_MEM_READ_EXE   : IN STD_LOGIC;
        -- Controls if one of the ALU sources will be a register
        -- or the immidiate field
        I_ALU_SRC_EXE    : IN STD_LOGIC;
        -- Controls where the results from writeback stage go
        -- either RS register or RT
        I_REG_DST_EXE    : IN STD_LOGIC;
        -- Program counter from fetch stage
        I_PC_PLUS4_EXE   : IN UNSIGNED(C_ADDR_WIDTH-1 DOWNTO 0);
        -- DetermINes which of the 32 registers
        -- will be written after writeback stage
        I_WRITE_REG_EXE  : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
                   
        ----------------------------------------
        -- Control Signals Sent to Memory Stage
        ----------------------------------------
        -- Controls if we write at a register after writeback stage
        O_REG_WRITE_EXE  : OUT STD_LOGIC;
        -- Chooses between ALU result or memory data
        -- to be written back at registers
        O_MEM_TO_REG_EXE : OUT STD_LOGIC;
        -- Controls memory writes at memory stage
        O_MEM_WRITE_EXE  : OUT STD_LOGIC;
        -- Controls memory reads at memory stage
        O_MEM_READ_EXE   : OUT STD_LOGIC;
        -- Program counter select IN fetch stage
        O_PC_SEL_EXE     : OUT STD_LOGIC;
        -- Program counter branch address forwarded to fetch stage
        O_PC_BRANCH_EXE  : OUT UNSIGNED(C_ADDR_WIDTH-1 DOWNTO 0);
        -- ALU result
        O_ALU_RES_EXE    : OUT STD_LOGIC_VECTOR(C_DATA_WIDTH-1 DOWNTO 0);
        -- Store word instruction write data. It writes the contents of
        -- RT register at the memory stage if instruction is store word (SW)
        O_WRITE_DATA_EXE : OUT STD_LOGIC_VECTOR(C_DATA_WIDTH-1 DOWNTO 0);
        -- Determines which of the 32 registers
        -- will be written after writeback stage
        O_WRITE_REG_EXE  : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
                   
        
        ------------------------------------------------
        -- Registers values and numbers received from
        -- decode stage
        ------------------------------------------------ 
        -- Decoded register number (0-31) of RT and RD
        -- Used for writeback stage to write to a register
        -- The result of writeback will go either to RT or RD
        -- depending on the instruction decoded
        I_RT_NUM_EXE  : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        I_RD_NUM_EXE  : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        -- RS (Source Operand)
        I_RS_VAL_EXE  : IN STD_LOGIC_VECTOR(C_DATA_WIDTH-1 DOWNTO 0);
        -- RT (Second Operand)
        I_RT_VAL_EXE  : IN STD_LOGIC_VECTOR(C_DATA_WIDTH-1 DOWNTO 0);
        
        ------------------------------------------------
        -- Signals received from decoded stage they are
        -- used at the ALU of the execution stage
        ------------------------------------------------  
        -- Shift Amount
        I_SHAMT_EXE   : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        -- Function to be executed if instruction is R-Type
        I_FUNCT_EXE   : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
        -- Immediate Operand if instruction is I-Type
        I_IMM_EXE     : IN STD_LOGIC_VECTOR(C_DATA_WIDTH-1 DOWNTO 0)

);
END instr_exe;



    
--*******************************************************************--
--                     A R C H I T E C T U R E                       --
--*******************************************************************--
ARCHITECTURE behavioral OF instr_exe IS


-------------------------------------------------------------------------------
-- Signals
-------------------------------------------------------------------------------
signal test : STD_LOGIC_VECTOR(31 downto 0);


--*******************************************************************--
--          B E G I N  F O R M A L  A R C H I T E C T U R E          --
--*******************************************************************--        
BEGIN
    
    -----------------------------------------------------------------
    --                   Instruction Execute                 
    --                                                           
    --  Description:                                              
    --       Executes the instruction specified by opcode and funct
    --       fields. Also consumes some of the control signals and 
    --       generates results for the memory stage.
    -----------------------------------------------------------------
    instr_exe_PROC: PROCESS(I_CLK, I_RST)
    BEGIN
        IF (I_RST = '1') THEN
            test    <= (OTHERS => '0');
            O_PC_BRANCH_EXE  <= (OTHERS => '0');
            O_REG_WRITE_EXE  <= '0';
            O_MEM_TO_REG_EXE <= '0';
            O_MEM_WRITE_EXE  <= '0';
            O_WRITE_DATA_EXE <= (OTHERS => '0');           
            O_PC_SEL_EXE     <= '0';
            O_WRITE_REG_EXE  <= (OTHERS => '0');
            
        ELSIF (rising_edge(I_CLK)) THEN
            -- Signals forwarded to memory stage
            O_REG_WRITE_EXE  <= I_REG_WRITE_EXE;
            O_MEM_TO_REG_EXE <= I_MEM_TO_REG_EXE;
            O_MEM_WRITE_EXE  <= I_MEM_WRITE_EXE;
            O_WRITE_DATA_EXE <= I_RT_VAL_EXE;
            O_PC_BRANCH_EXE  <= I_PC_PLUS4_EXE + UNSIGNED(I_IMM_EXE(15 DOWNTO 0) & "00");   
            O_MEM_READ_EXE   <= I_MEM_READ_EXE;
            O_WRITE_REG_EXE  <= I_WRITE_REG_EXE;
            
            -- If the opcode is of type R then check the
            -- funct field to execute an operation
            IF (I_OPCODE_EXE = R_TYPE_OP) THEN
                O_PC_SEL_EXE <= '0';
                CASE I_FUNCT_EXE IS
                    -- Arithmetic Operations
                    WHEN ADD_OP =>
                        test <= STD_LOGIC_VECTOR(UNSIGNED(I_RS_VAL_EXE) + UNSIGNED(I_RT_VAL_EXE));
                    WHEN ADDU_OP =>
                        test <= STD_LOGIC_VECTOR(UNSIGNED(I_RS_VAL_EXE) + UNSIGNED(I_RT_VAL_EXE));
                    WHEN SUB_OP =>
                        test <= STD_LOGIC_VECTOR(UNSIGNED(I_RS_VAL_EXE) - UNSIGNED(I_RT_VAL_EXE));
                    WHEN SUBU_OP =>
                        test <= STD_LOGIC_VECTOR(UNSIGNED(I_RS_VAL_EXE) - UNSIGNED(I_RT_VAL_EXE));
                    -- Logic Operations
                    WHEN AND_OP =>
                        test <= I_RS_VAL_EXE AND I_RT_VAL_EXE;
                    WHEN OR_OP =>
                        test <= I_RS_VAL_EXE OR I_RT_VAL_EXE;
                    WHEN XOR_OP =>
                        test <= I_RS_VAL_EXE XOR I_RT_VAL_EXE;
                    WHEN NOR_OP =>
                        test <= I_RS_VAL_EXE NOR I_RT_VAL_EXE;
                    -- Shift Left Operations
                    WHEN SLL_OP =>
                        --test <= STD_LOGIC_VECTOR(UNSIGNED(I_RT_VAL_EXE) SLL TO_INTEGER(UNSIGNED(I_SHAMT_EXE)));
                        -- Alternative: std_logic_vector(shift_left(unsigned(accum), to_integer(unsigned(OP1))));
                        test <= STD_LOGIC_VECTOR(UNSIGNED(I_RS_VAL_EXE) + 5);
                    WHEN SLLV_OP =>
                        --test <= STD_LOGIC_VECTOR(UNSIGNED(I_RT_VAL_EXE) SLL TO_INTEGER(UNSIGNED(I_RS_VAL_EXE(4 DOWNTO 0))));
                    -- Shift Right Operations
                    WHEN SRL_OP =>
                        test <= STD_LOGIC_VECTOR(UNSIGNED(I_RT_VAL_EXE) SRL TO_INTEGER(UNSIGNED(I_SHAMT_EXE)));
                    WHEN SRLV_OP =>
                        test <= STD_LOGIC_VECTOR(UNSIGNED(I_RT_VAL_EXE) SRL TO_INTEGER(UNSIGNED(I_RS_VAL_EXE(4 DOWNTO 0))));
                    WHEN SRA_OP =>
                        
                    WHEN SRAV_OP =>

                        
                    -- Set on Less Than Operations
                    WHEN SLT_OP =>
                        IF (signed(I_RS_VAL_EXE) < signed(I_RT_VAL_EXE)) THEN
                            test <= x"00000001";
                        ELSE
                            test <= x"00000000";
                        END IF;
                    WHEN SLTU_OP =>
                        IF (UNSIGNED(I_RS_VAL_EXE) < UNSIGNED(I_RT_VAL_EXE)) THEN
                            test <= x"00000001";
                        ELSE
                            test <= x"00000000";
                        END IF;
                        
                    WHEN OTHERS =>
                        
                END CASE;
                
            --ELSE
            --    -- If it's not R-Type use the opcode to execute
            --    -- the appropriate operation (I-Type or J-Type)
            --    CASE I_OPCODE_EXE IS
            --        WHEN ADDI_OP  =>
            --            O_ALU_RES_EXE <= STD_LOGIC_VECTOR(UNSIGNED(I_RS_VAL_EXE)  +  UNSIGNED(I_IMM_EXE));
            --        WHEN ADDIU_OP =>
            --            O_ALU_RES_EXE <= STD_LOGIC_VECTOR(UNSIGNED(I_RS_VAL_EXE)  +  UNSIGNED(I_IMM_EXE));
            --        WHEN ANDI_OP  =>
            --            O_ALU_RES_EXE <= STD_LOGIC_VECTOR(UNSIGNED(I_RS_VAL_EXE) AND UNSIGNED(I_IMM_EXE));
            --        WHEN ORI_OP   =>
            --            O_ALU_RES_EXE <= STD_LOGIC_VECTOR(UNSIGNED(I_RS_VAL_EXE) OR  UNSIGNED(I_IMM_EXE));
            --        WHEN XORI_OP  =>
            --            O_ALU_RES_EXE <= STD_LOGIC_VECTOR(UNSIGNED(I_RS_VAL_EXE) XOR UNSIGNED(I_IMM_EXE));
            --        WHEN LW_OP    =>
            --            O_ALU_RES_EXE <= STD_LOGIC_VECTOR(UNSIGNED(I_RS_VAL_EXE)  +  UNSIGNED(I_IMM_EXE));
            --        WHEN SW_OP    =>
            --            O_ALU_RES_EXE <= STD_LOGIC_VECTOR(UNSIGNED(I_RS_VAL_EXE)  +  UNSIGNED(I_IMM_EXE));
            --        WHEN BEQ_OP =>
            --            -- If RS = RT 
            --            IF ( I_RS_VAL_EXE = I_RT_VAL_EXE ) THEN
            --                -- Branch
            --                O_PC_SEL_EXE <= '1';
            --            ELSE
            --                -- PC + 4
            --                O_PC_SEL_EXE <= '0';
            --            END IF;
            --        WHEN OTHERS =>

            --    END CASE;
            END IF;
        END IF;
    END PROCESS;

    O_ALU_RES_EXE <= test;
    
END behavioral;
