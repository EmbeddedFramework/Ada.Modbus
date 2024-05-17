------------------------------------------------------------------------------
-- Copyright 2024, Gustavo Muro
-- Copyright (C) 2008, AdaCore
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

with AUnit.Assertions; use AUnit.Assertions;
with MB_Ascii;
with MB_Transport;
with Interfaces; use Interfaces;

package body Mb_Ascii_Test is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Modbus ASCII package");
   end Name;

   Buffer_Bin : constant MB_Types.Byte_Array :=
     (1 => 16#11#, 2 => 16#22#, 3 => 16#33#, 4 => 16#44#,
      5 => 16#AA#, 6 => 16#BB#, 7 => 16#1A#, 8 => 16#E2#);

   Buffer_Asc_Exp : constant MB_Types.Byte_Array :=
     (1 => Character'Pos (':'),
      2 => Character'Pos ('1'), 3 => Character'Pos ('1'),
      4 => Character'Pos ('2'), 5 => Character'Pos ('2'),
      6 => Character'Pos ('3'), 7 => Character'Pos ('3'),
      8 => Character'Pos ('4'), 9 => Character'Pos ('4'),
      10 => Character'Pos ('A'), 11 => Character'Pos ('A'),
      12 => Character'Pos ('B'), 13 => Character'Pos ('B'),
      14 => Character'Pos ('1'), 15 => Character'Pos ('A'),
      16 => Character'Pos ('E'), 17 => Character'Pos ('2'),
      18 => Character'Pos ('F'), 19 => Character'Pos ('5'),
      20 => 13, 21 => 10);

   Buffer_Asc : MB_Types.Byte_Array
     (1 .. (1 + (Buffer_Bin'Length + 1) * 2 + 2));

   Send_Count : MB_Transport.Msg_Length := 1;

   procedure SSend (Data : in Byte) is
   begin
      Buffer_Asc (Send_Count) := Data;
      Send_Count := Send_Count + 1;
   end SSend;

   function SRecv (Data : out Byte; Timeout : in Duration) return Boolean is
      pragma Unreferenced (Timeout);
      pragma Unreferenced (Data);
   begin
      return False;
   end SRecv;

   My_MB_Ascii : MB_Ascii.MB_Ascii_Type (SRecv'Access, SSend'Access);

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin

      MB_Ascii.Send (My_MB_Ascii, Buffer_Bin, Buffer_Bin'Length);

      for I in 1 .. Buffer_Asc_Exp'Length loop
         Assert (Buffer_Asc (I) = Buffer_Asc_Exp (I),
                 "Incorrect data sent at" & I'Image);
      end loop;

   end Run_Test;

end Mb_Ascii_Test;
