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

   -- ID + FNC_COD + CRC
   Min_Msg_Length : constant Msg_Length := 1 + 1 + 2;
   Limit_Baud     : constant Positive := 19200;

   -- =========================================================================
   -- Public procedures and functions
   -- =========================================================================

   procedure Calc_Times (Self : in out MB_Rtu_Type) is
      Baud : Positive := Self.Serial_Baud.all;
      Length : Positive := Self.Serial_Length.all;
      Microsecond : constant := 0.000001;
      Time_Byte : Time_Span := To_Time_Span(1.0 * Length / Baud);
   begin

      if Baud > Limit_Baud then
         Self.Time_Inter_Byte := To_Time_Span(750 * Microsecond);
         Self.Time_Out_Byte := To_Time_Span(1750 * Microsecond);
      else
         Self.Time_Inter_Byte := Time_Byte * 3 / 2;
         Self.Time_Out_Byte := Time_Byte * 7 / 2;
      end if;

   end Calc_Times;


   overriding
   procedure Send (Self : in out MB_Rtu_Type ;
                   Buffer :  Byte_Array ;
                   Length : MB_Transport.Msg_Length) is

   begin

      null;

   end Send;

   overriding
   function Recv (Self :  in out MB_Rtu_Type ;
                  Timeout : Time_Span) return MB_Transport.Msg_Length is
   begin
      return 0;
   end Recv;

end MB_Rtu;
