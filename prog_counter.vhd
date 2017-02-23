----------------------------------------------------------------------------------------
-- Title      : Program Counter                                                       
-- Project    : MIPS Processor                                                         
----------------------------------------------------------------------------------------
-- File       : prog_counter.vhd                                                          
-- Author     : Spyros Chiotakis <spyros.chiotakis@gmail.com>                         
-- Company    :                                                                       
-- Created    : 2016-05-15                                                            
-- Last update: 2017-02-20
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
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;



--*******************************************************************--
--                           E N T I T Y                             --
--*******************************************************************--
ENTITY prog_counter IS
    GENERIC (
        C_ADDR_WIDTH : INTEGER := 32
    );
    
    PORT (
        -- Global clock signal
        I_CLK : IN STD_LOGIC;
        -- Global reset signal active high
        I_RST : IN std_logic;

        -- Program counter select in fetch stage
        I_PC_SEL_FE       : IN  STD_LOGIC;
        -- Program counter branch address in fetch stage
        I_PC_BRANCH_FE    : IN  UNSIGNED(C_ADDR_WIDTH-1 DOWNTO 0);
        O_CACHE_PTR_FE    : OUT UNSIGNED(C_ADDR_WIDTH-1 DOWNTO 0);
        -- Program counter output
        O_PC_PLUS4_FE     : OUT UNSIGNED(C_ADDR_WIDTH-1 DOWNTO 0)
    );
END prog_counter;



--*******************************************************************--
--                     A R C H I T E C T U R E                       --
--*******************************************************************--
ARCHITECTURE behavioral OF prog_counter IS


-------------------------------------------------------------------
-- Signals                                                       --
-------------------------------------------------------------------
SIGNAL s_pc_out : UNSIGNED(C_ADDR_WIDTH-1 DOWNTO 0);


    

--*******************************************************************--
--          B E G I N  F O R M A L  A R C H I T E C T U R E          --
--*******************************************************************--
BEGIN

    O_CACHE_PTR_FE   <= s_pc_out;
    O_PC_PLUS4_FE    <= s_pc_out;
    ---------------------------------------------------------------
    --                 Program Counter Increment                 
    --                                                           
    --  Description:                                              
    --       The output points to the address of the instruction 
    --       memory to be fetched.                               
    ---------------------------------------------------------------
    pc_incr_PROC : PROCESS(I_CLK, I_RST)
    BEGIN
        IF (I_RST = '1') THEN           
            s_pc_out <= x"00000000";
        ELSIF (RISING_EDGE(I_CLK)) THEN
            
            IF (I_PC_SEL_FE = '0') THEN
                s_pc_out <= s_pc_out + 4;
            ELSIF (I_PC_SEL_FE = '1') THEN
                s_pc_out <= I_PC_BRANCH_FE;
            END IF;
     
        END IF;
    END PROCESS;
 
END behavioral;
