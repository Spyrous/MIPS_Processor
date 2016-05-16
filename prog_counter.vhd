----------------------------------------------------------------------------------------
-- Title      : Program Counter                                                       
-- Project    : MIPS Processor                                                         
----------------------------------------------------------------------------------------
-- File       : prog_counter.vhd                                                          
-- Author     : Spyros Chiotakis <spyros.chiotakis@gmail.com>                         
-- Company    :                                                                       
-- Created    : 2016-05-15                                                            
-- Last update: 2016-05-16
-- Platform   : Windows 10 Professional                                            
-- Standard   : VHDL'93/02                                                            
----------------------------------------------------------------------------------------
-- Description: Points to the next instruction to be fetched by the instruction cache         
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
entity prog_counter is
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
end prog_counter;



--*******************************************************************--
--                     A R C H I T E C T U R E                       --
--*******************************************************************--
architecture Behavioral of prog_counter is


-------------------------------------------------------------------
-- Signals                                                       --
-------------------------------------------------------------------
signal pc_out_s : std_logic_vector(ADDR_WIDTH-1 downto 0);


    

--*******************************************************************--
--          B E G I N  F O R M A L  A R C H I T E C T U R E          --
--*******************************************************************--
begin

    
    ---------------------------------------------------------------
    --                 Program Counter Increment                 
    --                                                           
    --  Description:                                              
    --       The output points to the address of the instruction 
    --       memory to be fetched.                               
    ---------------------------------------------------------------
    pc_incr_PROC : process(CLK_IN)
    begin
        if (RST_IN = '1') then
            PC_OUT <= x"00000000";
            pc_out_s <= x"00000000";
        elsif (rising_edge(CLK_IN)) then
            PC_OUT   <= pc_out_s;
            pc_out_s <= std_logic_vector(unsigned(pc_out_s) + 1);
        end if;
    end process;

    
end Behavioral;
