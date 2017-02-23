----------------------------------------------------------------------------------------
-- Title      : Instruction Fetch
-- Project    : MIPS Processor                                                         
----------------------------------------------------------------------------------------
-- File       : instr_fetch.vhd                                                          
-- Author     : Spyros Chiotakis <spyros.chiotakis@gmail.com>                         
-- Company    :                                                                       
-- Created    : 2016-05-16                                                            
-- Last update: 2017-02-20
-- Platform   : Windows 10 Professional                                            
-- Standard   : VHDL'93/02                                                            
----------------------------------------------------------------------------------------
-- Description: This stage fetches the instructions from instruction cache and forwards
--              them to the decode stage.
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


--*******************************************************************--
--                           E N T I T Y                             --
--*******************************************************************--
ENTITY instr_fetch IS
    GENERIC (
        C_CACHE_SIZE : INTEGER := 128;
        C_DATA_WIDTH : INTEGER := 32;
        C_ADDR_WIDTH : INTEGER := 32
    );
    PORT (
        I_CLK : IN STD_LOGIC;
        I_RST : IN STD_LOGIC;

        -- Program counter select in fetch stage
        I_PC_SEL_FE    : IN STD_LOGIC;
        -- Program counter branch address in fetch stage
        I_PC_BRANCH_FE : IN UNSIGNED(C_ADDR_WIDTH-1 DOWNTO 0);
        -- Program counter output to decode stage
        O_PC_PLUS4_FE    : OUT UNSIGNED(C_ADDR_WIDTH-1 DOWNTO 0);
        -- Instruction to be executed by the rest of the pipeline
        O_CACHE_INSTR_FE : OUT STD_LOGIC_VECTOR(C_DATA_WIDTH-1 DOWNTO 0)
    );
END instr_fetch;


--*******************************************************************--
--                     A R C H I T E C T U R E                       --
--*******************************************************************--
ARCHITECTURE Structural OF instr_fetch IS


    -------------------------------------------------------------------
    -- Signals                                                       --
    -------------------------------------------------------------------
    -- Connects the output of program counter to the input of I_Cache
    SIGNAL i_cache_ptr_s : UNSIGNED(C_ADDR_WIDTH-1 DOWNTO 0);


--*******************************************************************--
--          B E G I N  F O R M A L  A R C H I T E C T U R E          --
--*******************************************************************--
BEGIN

    -------------------------------------------------------------------
    -- Port Maps                                                     --
    -------------------------------------------------------------------
    program_counter: ENTITY WORK.prog_counter
        GENERIC MAP (
            C_ADDR_WIDTH => 32
        )
        PORT MAP (
            -- Global clock signal
            I_CLK => I_CLK,
            -- Global reset signal active high
            I_RST => I_RST,

            -- Program counter select in fetch stage
            I_PC_SEL_FE       => I_PC_SEL_FE,
            -- Program counter branch address in fetch stage
            I_PC_BRANCH_FE    => I_PC_BRANCH_FE,
            -- Program counter output to decode stage
            O_PC_PLUS4_FE    => O_PC_PLUS4_FE,
            -- Program counter output cache memory
            O_CACHE_PTR_FE => i_cache_ptr_s
        );

    instruction_cache: ENTITY WORK.I_Cache
        GENERIC MAP (
            C_CACHE_SIZE => 128,
            C_DATA_WIDTH => 32,
            C_ADDR_WIDTH => 32
        )
        PORT MAP (
            I_RST => I_RST,

            -- This pointer comes from the program counter and points in our memory
            I_CACHE_PTR => i_cache_ptr_s,

            -- Instruction that cache outputs depending on the pointer
            O_CACHE_INSTR => O_CACHE_INSTR_FE
        );

    
END Structural;
