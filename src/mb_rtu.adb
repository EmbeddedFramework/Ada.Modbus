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
with MB_Types; use MB_Types;

package body MB_Rtu is

   subtype Msg_Length is Integer range 0 .. MB_Rtu.Msg_Max_Length;
   subtype CRC_Type is Interfaces.Unsigned_16 range 0 .. 16#FFFF#;

   -- ID + FNC_COD + CRC
   Min_Msg_Length : constant Msg_Length := 1 + 1 + 2;
   Limit_Baud     : constant Positive := 19200;


   Crc_HB_T1 : constant array(0 .. 127) of Byte :=
     ( 16#00#,16#C0#,16#C1#,16#01#,16#C3#,16#03#,16#02#,16#C2#,
       16#C6#,16#06#,16#07#,16#C7#,16#05#,16#C5#,16#C4#,16#04#,
       16#CC#,16#0C#,16#0D#,16#CD#,16#0F#,16#CF#,16#CE#,16#0E#,
       16#0A#,16#CA#,16#CB#,16#0B#,16#C9#,16#09#,16#08#,16#C8#,
       16#D8#,16#18#,16#19#,16#D9#,16#1B#,16#DB#,16#DA#,16#1A#,
       16#1E#,16#DE#,16#DF#,16#1F#,16#DD#,16#1D#,16#1C#,16#DC#,
       16#14#,16#D4#,16#D5#,16#15#,16#D7#,16#17#,16#16#,16#D6#,
       16#D2#,16#12#,16#13#,16#D3#,16#11#,16#D1#,16#D0#,16#10#,
       16#F0#,16#30#,16#31#,16#F1#,16#33#,16#F3#,16#F2#,16#32#,
       16#36#,16#F6#,16#F7#,16#37#,16#F5#,16#35#,16#34#,16#F4#,
       16#3C#,16#FC#,16#FD#,16#3D#,16#FF#,16#3F#,16#3E#,16#FE#,
       16#FA#,16#3A#,16#3B#,16#FB#,16#39#,16#F9#,16#F8#,16#38#,
       16#28#,16#E8#,16#E9#,16#29#,16#EB#,16#2B#,16#2A#,16#EA#,
       16#EE#,16#2E#,16#2F#,16#EF#,16#2D#,16#ED#,16#EC#,16#2C#,
       16#E4#,16#24#,16#25#,16#E5#,16#27#,16#E7#,16#E6#,16#26#,
       16#22#,16#E2#,16#E3#,16#23#,16#E1#,16#21#,16#20#,16#E0#);


   Crc_HB_T2 : constant array(0 .. 127) of Byte :=
     ( 16#A0#,16#60#,16#61#,16#A1#,16#63#,16#A3#,16#A2#,16#62#,
       16#66#,16#A6#,16#A7#,16#67#,16#A5#,16#65#,16#64#,16#A4#,
       16#6C#,16#AC#,16#AD#,16#6D#,16#AF#,16#6F#,16#6E#,16#AE#,
       16#AA#,16#6A#,16#6B#,16#AB#,16#69#,16#A9#,16#A8#,16#68#,
       16#78#,16#B8#,16#B9#,16#79#,16#BB#,16#7B#,16#7A#,16#BA#,
       16#BE#,16#7E#,16#7F#,16#BF#,16#7D#,16#BD#,16#BC#,16#7C#,
       16#B4#,16#74#,16#75#,16#B5#,16#77#,16#B7#,16#B6#,16#76#,
       16#72#,16#B2#,16#B3#,16#73#,16#B1#,16#71#,16#70#,16#B0#,
       16#50#,16#90#,16#91#,16#51#,16#93#,16#53#,16#52#,16#92#,
       16#96#,16#56#,16#57#,16#97#,16#55#,16#95#,16#94#,16#54#,
       16#9C#,16#5C#,16#5D#,16#9D#,16#5F#,16#9F#,16#9E#,16#5E#,
       16#5A#,16#9A#,16#9B#,16#5B#,16#99#,16#59#,16#58#,16#98#,
       16#88#,16#48#,16#49#,16#89#,16#4B#,16#8B#,16#8A#,16#4A#,
       16#4E#,16#8E#,16#8F#,16#4F#,16#8D#,16#4D#,16#4C#,16#8C#,
       16#44#,16#84#,16#85#,16#45#,16#87#,16#47#,16#46#,16#86#,
       16#82#,16#42#,16#43#,16#83#,16#41#,16#81#,16#80#,16#40#
      );

   Crc_LB_T1 : constant array(0 .. 127) of Byte :=
     ( 16#00#,16#C1#,16#81#,16#40#,16#01#,16#C0#,16#80#,16#41#,
       16#01#,16#C0#,16#80#,16#41#,16#00#,16#C1#,16#81#,16#40#,
       16#01#,16#C0#,16#80#,16#41#,16#00#,16#C1#,16#81#,16#40#,
       16#00#,16#C1#,16#81#,16#40#,16#01#,16#C0#,16#80#,16#41#,
       16#01#,16#C0#,16#80#,16#41#,16#00#,16#C1#,16#81#,16#40#,
       16#00#,16#C1#,16#81#,16#40#,16#01#,16#C0#,16#80#,16#41#,
       16#00#,16#C1#,16#81#,16#40#,16#01#,16#C0#,16#80#,16#41#,
       16#01#,16#C0#,16#80#,16#41#,16#00#,16#C1#,16#81#,16#40#,
       16#01#,16#C0#,16#80#,16#41#,16#00#,16#C1#,16#81#,16#40#,
       16#00#,16#C1#,16#81#,16#40#,16#01#,16#C0#,16#80#,16#41#,
       16#00#,16#C1#,16#81#,16#40#,16#01#,16#C0#,16#80#,16#41#,
       16#01#,16#C0#,16#80#,16#41#,16#00#,16#C1#,16#81#,16#40#,
       16#00#,16#C1#,16#81#,16#40#,16#01#,16#C0#,16#80#,16#41#,
       16#01#,16#C0#,16#80#,16#41#,16#00#,16#C1#,16#81#,16#40#,
       16#01#,16#C0#,16#80#,16#41#,16#00#,16#C1#,16#81#,16#40#,
       16#00#,16#C1#,16#81#,16#40#,16#01#,16#C0#,16#80#,16#41#
      );

   Crc_LB_T2 : constant array(0 .. 127) of Byte :=
     ( 16#01#,16#C0#,16#80#,16#41#,16#00#,16#C1#,16#81#,16#40#,
       16#00#,16#C1#,16#81#,16#40#,16#01#,16#C0#,16#80#,16#41#,
       16#00#,16#C1#,16#81#,16#40#,16#01#,16#C0#,16#80#,16#41#,
       16#01#,16#C0#,16#80#,16#41#,16#00#,16#C1#,16#81#,16#40#,
       16#00#,16#C1#,16#81#,16#40#,16#01#,16#C0#,16#80#,16#41#,
       16#01#,16#C0#,16#80#,16#41#,16#00#,16#C1#,16#81#,16#40#,
       16#01#,16#C0#,16#80#,16#41#,16#00#,16#C1#,16#81#,16#40#,
       16#00#,16#C1#,16#81#,16#40#,16#01#,16#C0#,16#80#,16#41#,
       16#00#,16#C1#,16#81#,16#40#,16#01#,16#C0#,16#80#,16#41#,
       16#01#,16#C0#,16#80#,16#41#,16#00#,16#C1#,16#81#,16#40#,
       16#01#,16#C0#,16#80#,16#41#,16#00#,16#C1#,16#81#,16#40#,
       16#00#,16#C1#,16#81#,16#40#,16#01#,16#C0#,16#80#,16#41#,
       16#01#,16#C0#,16#80#,16#41#,16#00#,16#C1#,16#81#,16#40#,
       16#00#,16#C1#,16#81#,16#40#,16#01#,16#C0#,16#80#,16#41#,
       16#00#,16#C1#,16#81#,16#40#,16#01#,16#C0#,16#80#,16#41#,
       16#01#,16#C0#,16#80#,16#41#,16#00#,16#C1#,16#81#,16#40#
      );

   function Calc_CRC(Buffer : Byte_Array; Length : MB_Transport.Msg_Length)
                     return CRC_Type is
      Index : Byte;
      Cnt : MB_Transport.Msg_Length := 1;
      CrcLsb : Byte := 16#FF#;
      CrcMsb : Byte := 16#FF#;
   begin
      while Cnt <= Length loop
         Index := CrcLsb xor Buffer(Cnt);
         if Index < 128 then
            CrcLsb := CrcMsb xor Crc_LB_T1(Integer (Index));
            CrcMsb := Crc_HB_T1(Integer (Index));
         else
            CrcLsb := CrcMsb xor Crc_LB_T2(Integer (Index) - 128);
            CrcMsb := Crc_HB_T2(Integer (Index) - 128);
         end if;
         Cnt := Cnt + 1;
      end loop;
      return CRC_Type (CrcMsb) * 256 + CRC_Type (CrcLsb);
   end Calc_CRC;

   function Check_CRC (Buffer : Byte_Array; Length : MB_Transport.Msg_Length)
                       return Boolean is
      Crc : CRC_Type;
      Crc_H : Byte;
      Crc_L : Byte;
   begin

      Crc := Calc_CRC (Buffer, Length-2);
      Crc_L := Byte (Crc and 16#FF#);
      Crc_H := Byte (Crc / 256);

      if Crc_L = Buffer (Length - 1) and Crc_H = Buffer (Length) then
         return True;
      end if;

      return False;

   end Check_CRC;

   -- =========================================================================
   -- Public procedures and functions
   -- =========================================================================

   procedure Calc_Times (Self : in out MB_Rtu_Type) is
      Baud : Positive := Self.Serial_Baud.all;
      Length : Positive := Self.Serial_Length.all;
      Microsecond : constant := 0.000001;
   begin

      Self.Time_Byte := To_Time_Span(1.0 * Length / Baud);

      if Baud > Limit_Baud then
         Self.Time_Inter_Byte := To_Time_Span(750 * Microsecond);
         Self.Time_Out_Byte := To_Time_Span(1750 * Microsecond);
      else
         Self.Time_Inter_Byte := Self.Time_Byte * 3 / 2;
         Self.Time_Out_Byte := Self.Time_Byte * 7 / 2;
      end if;

   end Calc_Times;


   overriding
   procedure Send (Self : in out MB_Rtu_Type ;
                   Buffer :  Byte_Array ;
                   Length : MB_Transport.Msg_Length) is

      Crc : CRC_Type := Calc_CRC (Buffer, Length);
      Rtu_Length : Msg_Length := Length;
   begin

      for I in 1 .. Length loop
         Self.Buffer(I) := Buffer(I);
      end loop;

      Rtu_Length := Rtu_Length + 1;
      Self.Buffer (Rtu_Length) := Byte (Crc and 16#FF#);

      Rtu_Length := Rtu_Length + 1;
      Self.Buffer (Rtu_Length) := Byte (Crc / 256);

      for I in 1 .. Rtu_Length loop
         Self.Serial_Send(Self.Buffer(I));
      end loop;

   end Send;

   overriding
   function Recv (Self :  in out MB_Rtu_Type ;
                  Timeout : Time_Span) return MB_Transport.Msg_Length is

      Begin_Time : Time := Clock;
      Start_Time : Time := Clock;
      Elapsed_Time : Time_Span;
      Total_Time : Time_Span;
      Index : Msg_Length := 0;
      Byte_Rec : Byte;
      Start_Rec : Boolean := False;
      Ret : Boolean := False;

   begin

      Reception_Loop:
      loop

         if Ret then
            Start_Time := Clock;
         end if;

         Ret := Self.Serial_Recv (Byte_Rec,
                                  Self.Time_Byte + Self.Time_Out_Byte);

         Elapsed_Time := Clock - Start_Time;
         Total_Time := Clock - Begin_Time;

         if Elapsed_Time >= Self.Time_Byte + Self.Time_Out_Byte then

            if Total_Time > Timeout then
               Index := 0;
               exit Reception_Loop;
            end if;

            if Ret then
               Start_Rec := True;
               Index := 1;
               Self.Buffer (Index) := Byte_Rec;
            end if;

            if Start_Rec and Index >= Min_Msg_Length then
               if Check_CRC (Self.Buffer, Index) then
                  Index := Index - 2;
                  exit Reception_Loop;
               else
                  Index := 0;
                  Start_Rec := False;
               end if;
            end if;
         else
            if Elapsed_Time < Self.Time_Byte + Self.Time_Inter_Byte then
               if Ret then
                  Index := Index + 1;
                  Self.Buffer (Index) := Byte_Rec;
               end if;
            else
               Start_Rec := False;
               Index := 0;
            end if;

         end if;

      end loop Reception_Loop;

      return Index;

   end Recv;

end MB_Rtu;
