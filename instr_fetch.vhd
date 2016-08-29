----------------------------------------------------------------------------------------
-- Title      : Instruction Fetch
-- Project    : MIPS Processor                                                         
----------------------------------------------------------------------------------------
-- File       : instr_fetch.vhd                                                          
-- Author     : Spyros Chiotakis <spyros.chiotakis@gmail.com>                         
-- Company    :                                                                       
-- Created    : 2016-05-16                                                            
-- Last update: 2016-08-29
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
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;



--*******************************************************************--
--                           E N T I T Y                             --
--*******************************************************************--
entity instr_fetch is
    generic (
        CACHE_SIZE : integer := 128;
        DATA_WIDTH : integer := 32;
        ADDR_WIDTH : integer := 32
    );
    Port (
        CLK_IN : in std_logic;
        RST_IN : in std_logic;

        CACHE_INSTR_DEC_OUT : out std_logic_vector(DATA_WIDTH-1 downto 0)
    );
end instr_fetch;


--*******************************************************************--
--                     A R C H I T E C T U R E                       --
--*******************************************************************--
architecture Structural of instr_fetch is


    -------------------------------------------------------------------
    -- Signals                                                       --
    -------------------------------------------------------------------
    -- Connects the output of program counter to the input of I_Cache
    signal i_cache_ptr_s : std_logic_vector(ADDR_WIDTH-1 downto 0);




    -------------------------------------------------------------------
    -- Components                                                    --
    -------------------------------------------------------------------
    component prog_counter
        generic (
            ADDR_WIDTH : integer := 32
            );
        
        port (
            -- Global clock signal
            CLK_IN : in std_logic;
            -- Global reset signal active high
            RST_IN : in std_logic;
            
            -- Program counter output
            PC_OUT : out std_logic_vector(ADDR_WIDTH-1 downto 0)
            );
    end component;

    component I_Cache
        generic (
            --  Cache size default is 128*32 = 4096 bytes or 4kB
            CACHE_SIZE : integer := 128;
            DATA_WIDTH : integer := 32;
            ADDR_WIDTH : integer := 32
        );
        port (
            RST_IN : in std_logic;

            -- This pointer comes from the program counter and points in our memory
            I_CACHE_PTR_IN : in std_logic_vector(ADDR_WIDTH-1 downto 0);

            -- Instruction that cache outputs depending on the pointer
            CACHE_INSTR_OUT : out std_logic_vector(DATA_WIDTH-1 downto 0) 
        );
    end component;


--*******************************************************************--
--          B E G I N  F O R M A L  A R C H I T E C T U R E          --
--*******************************************************************--
begin

    -------------------------------------------------------------------
    -- Port Maps                                                     --
    -------------------------------------------------------------------
    program_counter: prog_counter
        generic map (
            ADDR_WIDTH => 32
            )
        port map (
            -- Global clock signal
            CLK_IN => CLK_IN,
            -- Global reset signal active high
            RST_IN => RST_IN,
            
            -- Program counter output
            PC_OUT => i_cache_ptr_s
            );

    instruction_cache: I_Cache
        generic map (
            CACHE_SIZE => 128,
            DATA_WIDTH => 32,
            ADDR_WIDTH => 32
        )
        port map (
            RST_IN => RST_IN,

            -- This pointer comes from the program counter and points in our memory
            I_CACHE_PTR_IN => i_cache_ptr_s,

            -- Instruction that cache outputs depending on the pointer
            CACHE_INSTR_OUT => CACHE_INSTR_DEC_OUT
        );

    
end Structural;
