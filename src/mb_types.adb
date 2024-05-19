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

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

package body MB_Types is

   procedure Write_16_Bits (W     : in Word;
                            B_A   : out Byte_Array;
                            Index : in Positive) is
   begin
      B_A(Index)     := Unsigned_8(W / 16#100#); -- Higher byte
      B_A(Index + 1) := Unsigned_8(W mod 16#100#); -- Lower byte
   end Write_16_Bits;

   procedure Write_Multiples_16_Bits (W_A : in Word_Array;
                                      Index_WA : in Positive;
                                      Qty      : in Positive;
                                      B_A      : out Byte_Array;
                                      Index_BA : in Positive) is
      Count_BA : Integer := 0;
   begin

      for I in Index_WA .. Qty + Index_WA - 1 loop
         Write_16_Bits (W_A (I), B_A, Index_BA + Count_BA);
         Count_BA := Count_BA + 2;
      end loop;

   end Write_Multiples_16_Bits;


   function Read_16_Bits (B_A   : in Byte_Array;
                          Index : in Positive)
                        return Interfaces.Unsigned_16 is
      Higher_Byte : Unsigned_16;
      Lower_Byte  : Unsigned_16;
   begin
      Higher_Byte := Unsigned_16 (B_A (Index));
      Lower_Byte  := Unsigned_16 (B_A (Index + 1));
      return Interfaces.Unsigned_16((Higher_Byte * 16#100#) + Lower_Byte);
   end Read_16_Bits;

end MB_Types;
