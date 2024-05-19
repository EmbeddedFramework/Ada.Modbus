------------------------------------------------------------------------------
-- Copyright 2024, Gustavo Muro
-- All rights reserved
-- 
-- This file is part of EmbeddedFirmware.
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
-- 3. Neither the name of the copyright holder nor the names of its
--    contributors may be used to endorse or promote products derived from this
--    software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
------------------------------------------------------------------------------

with Interfaces;

package MB_Types is
   
   subtype Byte is Interfaces.Unsigned_8;   
   type Byte_Array is array (Positive range <>) of Byte;

   subtype Word is Interfaces.Unsigned_16 range 0 .. 16#FFFF#;
   type Word_Array is array (Positive range <>) of Word;
   
   subtype Address is Word range 0 .. 16#FFFF#;
   subtype Quantity is Word range 0 .. 16#FFFF#;
   
   subtype Holding_Register is Word;
   subtype Holding_Register_Array is Word_Array;
   
   -- Procedure to write 16 bits to Byte_Array
   procedure Write_Word (W     : in Word;
                         B_A   : out Byte_Array;
                         Index : in Positive);

   -- Procedure to write 16 bits to Byte_Array
   procedure Write_Multiples_Words (W_A      : in Word_Array;
                                    Index_WA : in Positive;
                                    Qty      : in Positive;
                                    B_A      : out Byte_Array;
                                    Index_BA : in Positive);
   

   -- Function to read 16 bits from Byte_Array
   function Read_Word (B_A   : in Byte_Array;
                       Index : in Positive) return Word;
   
   -- Procedure to read 16 bits from Byte_Array
   procedure Read_Multiples_Words (W_A      : out Word_Array;
                                    Index_WA : in Positive;
                                    Qty      : in Positive;
                                    B_A      : in Byte_Array;
                                    Index_BA : in Positive);
   
end MB_Types;
