----------------------------------------------------------------------------------------
-- Title      : Instruction Cache
-- Project    : MIPS Processor                                                         
----------------------------------------------------------------------------------------
-- File       : I_Cache.vhd                                                          
-- Author     : Spyros Chiotakis <spyros.chiotakis@gmail.com>                         
-- Company    :                                                                       
-- Created    : 2016-05-15                                                            
-- Last update: 2017-02-20
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
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

--*******************************************************************--
--                           E N T I T Y                             --
--*******************************************************************--
ENTITY I_Cache IS
    GENERIC (
        --  Cache size default is 16*32 = 492 bytes
        C_CACHE_SIZE : INTEGER := 16;
        C_DATA_WIDTH : INTEGER := 32;
        C_ADDR_WIDTH : INTEGER := 32
    );
    PORT (
        I_RST : IN STD_LOGIC;

        -- This pointer comes from the program counter and points in our memory
        I_CACHE_PTR  : IN UNSIGNED(C_ADDR_WIDTH-1 DOWNTO 0);

        -- Instruction that cache outputs depending on the pointer
        O_CACHE_INSTR : OUT STD_LOGIC_VECTOR(C_DATA_WIDTH-1 DOWNTO 0) 
    );
END I_Cache;




--*******************************************************************--
--                     A R C H I T E C T U R E                       --
--*******************************************************************--
ARCHITECTURE behavioral OF I_Cache IS
    
    TYPE T_CACHE_MEMORY IS ARRAY (0 TO C_CACHE_SIZE-1) OF STD_LOGIC_VECTOR(C_DATA_WIDTH-1 DOWNTO 0);
    --SIGNAL s_instr_cache : T_CACHE_MEMORY;
    CONSTANT c_instr_cache : T_CACHE_MEMORY := (
                -- Branch
                --4  => x"10000010",
                -- Branch
                --8  => x"10000100",
                -- R-Type Add
                12 => "00000000011000100000100000100000",
           OTHERS => (OTHERS =>'0')
        );

--*******************************************************************--
--          B E G I N  F O R M A L  A R C H I T E C T U R E          --
--*******************************************************************--
BEGIN

    PROCESS(I_CACHE_PTR, I_RST)
    BEGIN
        IF (I_RST = '1') THEN
           -- s_instr_cache <= (
           --     -- Branch
           --     4  => x"10000010",
           --     -- Branch
           --     8  => x"10000100",
           --     -- R-Type Add
           --     12 => x"00000020",
           --OTHERS => (OTHERS =>'0'));


            O_CACHE_INSTR <= x"00000000";
        ELSE            
            O_CACHE_INSTR <= c_instr_cache(TO_INTEGER(I_CACHE_PTR(6 DOWNTO 0)));
        END IF;
    END PROCESS;
    
END behavioral;
