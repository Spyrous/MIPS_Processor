----------------------------------------------------------------------------------------
-- Title      : Instruction Cache
-- Project    : MIPS Processor                                                         
----------------------------------------------------------------------------------------
-- File       : I_Cache.vhd                                                          
-- Author     : Spyros Chiotakis <spyros.chiotakis@gmail.com>                         
-- Company    :                                                                       
-- Created    : 2016-05-15                                                            
-- Last update: 2016-09-05
-- Platform   : Windows 10 Professional                                            
-- Standard   : VHDL'93/02                                                            
----------------------------------------------------------------------------------------
-- Description: Contains the instructions to be executed by the processor         
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

--*******************************************************************--
--                           E N T I T Y                             --
--*******************************************************************--
entity I_Cache is
    generic (
        --  Cache size default is 128*32 = 4096 bytes or 4kB
        CACHE_SIZE : integer := 128;
        DATA_WIDTH : integer := 32;
        ADDR_WIDTH : integer := 32
        );
    port (
        RST_IN : in std_logic;

        -- This pointer comes from the program counter and points in our memory
        I_CACHE_PTR_IN : in unsigned(ADDR_WIDTH-1 downto 0);

        -- Instruction that cache outputs depending on the pointer
        CACHE_INSTR_OUT : out std_logic_vector(DATA_WIDTH-1 downto 0) 
);
end I_Cache;




--*******************************************************************--
--                     A R C H I T E C T U R E                       --
--*******************************************************************--
architecture Behavioral of I_Cache is


    type CACHE_MEMORY is array (0 to CACHE_SIZE-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
    signal INSTR_CACHE : CACHE_MEMORY;



--*******************************************************************--
--          B E G I N  F O R M A L  A R C H I T E C T U R E          --
--*******************************************************************--
begin

    process(I_CACHE_PTR_IN, RST_IN)
    begin
        if (RST_IN = '1') then
            INSTR_CACHE <= (4 => x"10000010",
                            8 => x"10000100",
                others => (others =>'0'));
            CACHE_INSTR_OUT <= x"00000000";
        else            
            CACHE_INSTR_OUT <= INSTR_CACHE(to_integer(I_CACHE_PTR_IN));
        end if;
    end process;
    
end Behavioral;
