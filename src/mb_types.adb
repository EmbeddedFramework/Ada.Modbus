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

   procedure Write_16_Bits (Register : in Holding_Register;
                                     Buffer   : out Byte_Array;
                                     Index    : in Positive) is
   begin
      Buffer(Index)     := Unsigned_8(Register / 16#100#); -- Higher byte
      Buffer(Index + 1) := Unsigned_8(Register mod 16#100#); -- Lower byte
   end Write_16_Bits;

   function Read_16_Bits (Buffer : in Byte_Array;
                                   Index  : in Positive) return Holding_Register is
      Higher_Byte : Unsigned_16;
      Lower_Byte  : Unsigned_16;
   begin
      Higher_Byte := Unsigned_16 (Buffer (Index));
      Lower_Byte  := Unsigned_16 (Buffer (Index + 1));
      return Holding_Register((Higher_Byte * 16#100#) + Lower_Byte);
   end Read_16_Bits;

end MB_Types;
