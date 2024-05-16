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

with MB_Transport;
with Ada.Real_Time; use Ada.Real_Time;
with Interfaces; use Interfaces;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body MB_Ascii is

   Start_Byte : constant MB_Types.Byte := 58;
   End1_Byte : constant MB_Types.Byte := 13;
   End2_Byte : constant MB_Types.Byte := 10;

   subtype Msg_Length is Integer range 0 .. MB_Ascii.Msg_Max_Length;
   subtype Nibble is Interfaces.Unsigned_8 range 0 .. 15;

   function Calc_LRC (Buffer : MB_Types.Byte_Array ;
                      Length : MB_Transport.Msg_Length) return MB_Types.Byte is

      Result : MB_Types.Byte := 0;
   begin
      for I in 1 .. Length loop
         Result := Result + Buffer(I);
      end loop;
      return -Result;
   end Calc_LRC;

   function Nibble_To_Char(N : Nibble) return MB_Types.Byte is
   begin
      if N < 10 then
         return MB_Types.Byte(Character'Pos('0') + Nibble'Pos(N));
      else
         return MB_Types.Byte(Character'Pos('A') + Nibble'Pos(N) - 10);
      end if;
   end Nibble_To_Char;

   overriding
   procedure Send (Self : in out MB_Ascii_Type ;
                   Buffer : MB_Types.Byte_Array ;
                   Length : MB_Transport.Msg_Length) is
      Ascii_Length : Msg_Length := Length;
      High : Nibble := 0;
      Low : Nibble := 0;

   begin

      Self.Buffer (Length + 1) := Calc_LRC(Buffer, Ascii_Length);
      Ascii_Length := Ascii_Length + 1;

      for I in reverse 1 .. Ascii_Length loop
         High := Nibble(Buffer(I) / 16);
         Low := Nibble(Buffer(I) mod 16);

         Self.Buffer (I * 2 + 0) := Nibble_To_Char(High);
         Self.Buffer (I * 2 + 1) := Nibble_To_Char(Low);
      end loop;

      Self.Buffer (1) := Start_Byte;
      Ascii_Length := Ascii_Length * 2 + 1;

      Ascii_Length := Ascii_Length + 1;
      Self.Buffer (Ascii_Length) := End1_Byte;

      Ascii_Length := Ascii_Length + 1;
      Self.Buffer (Ascii_Length) := End2_Byte;

      for I in 1 .. Ascii_Length loop
         Self.Serial_Send(Self.Buffer(I));
      end loop;


   end Send;

   overriding
   function Recv (Self :  in out MB_Ascii_Type ; Timeout : Time_Span) return MB_Transport.Msg_Length is
      pragma Unreferenced (Timeout);
      Result : MB_Types.Byte_Array (1 .. 0);
   begin
      Self.Buffer (1) := 16#12#;
      Self.Buffer (2) := 16#34#;
      Self.Buffer (3) := 16#AB#;
      Self.Buffer (4) := 16#CD#;
      return 4;
   end Recv;

end MB_Ascii;
