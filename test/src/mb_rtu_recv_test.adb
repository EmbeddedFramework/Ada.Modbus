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
with MB_Rtu;
with MB_Transport;
with Interfaces; use Interfaces;

package body Mb_Rtu_Recv_Test is

   Baudrate : constant Positive := 9600;
   --  1 (Start) + 8 (Data) + 1 (Parity) + 1 (Stop)
   Total_Bits : constant Positive := 11;

   Time_Dur : constant Duration := Duration (Total_Bits) / Duration (Baudrate);

   Time_Byte : constant Time_Span := To_Time_Span (Time_Dur);

   --  Time_Inter_Byte : constant Time_Span := Time_Byte * 3 / 2;
   Time_Out_Byte   : constant Time_Span := Time_Byte * 7 / 2;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Modbus RTU Recv");
   end Name;

   type Time_Span_Array is array (Positive range <>) of Time_Span;

   Buffer_1 : aliased MB_Types.Byte_Array :=
     (1 => 16#11#, 2 => 16#22#, 3 => 16#33#, 4 => 16#44#,
      5 => 16#AA#, 6 => 16#BB#, 7 => 16#1A#, 8 => 16#E2#,
      9 => 16#AD#, 10 => 16#D1#);

   Times_1 : aliased Time_Span_Array :=
     (Time_Byte + Time_Out_Byte,
      Time_Byte, Time_Byte, Time_Byte, Time_Byte, Time_Byte, Time_Byte,
      Time_Byte, Time_Byte, Time_Byte);

   Buffer_2 : aliased MB_Types.Byte_Array :=
     (1 => 16#11#, 2 => 16#22#, 3 => 16#33#, 4 => 16#44#,
      5 => 16#AA#, 6 => 16#BB#, 7 => 16#1A#, 8 => 16#E2#,
      9 => 16#AD#, 10 => 16#D1#);

   Times_2 : aliased Time_Span_Array :=
     (Time_Byte + Time_Out_Byte,
      Time_Byte, Time_Byte, Time_Byte, Time_Byte, Time_Byte, Time_Byte,
      Time_Byte + Time_Out_Byte, Time_Byte, Time_Byte);

   type Byte_Array_Ptr is access all MB_Types.Byte_Array;
   Ptr_Buf : Byte_Array_Ptr;

   type Times_Array_Ptr is access all Time_Span_Array;
   Ptr_Tim : Times_Array_Ptr;

   Buffer_Exp : constant MB_Types.Byte_Array :=
     (1 => 16#11#, 2 => 16#22#, 3 => 16#33#, 4 => 16#44#,
      5 => 16#AA#, 6 => 16#BB#, 7 => 16#1A#, 8 => 16#E2#);

   Recv_Count : MB_Transport.Msg_Length := 1;

   procedure SSend (Data : in Byte) is
   begin
      null;
   end SSend;

   function SRecv (Data : out Byte; Timeout : in Time_Span) return Boolean is
   begin

      if Recv_Count <= Ptr_Buf'Length then
         delay until Clock + Ptr_Tim (Recv_Count);
         Data := Ptr_Buf (Recv_Count);
         Recv_Count := Recv_Count + 1;
         return True;
      end if;

      delay until Clock + Timeout;

      return False;
   end SRecv;

   function Get_Baud_CB return Positive is
   begin
      return Baudrate;
   end Get_Baud_CB;

   function Get_Length_CB return Positive is
   begin
      return Total_Bits;
   end Get_Length_CB;


   My_MB_Rtu : MB_Rtu.MB_Rtu_Type (SRecv'Access, SSend'Access,
                                   Get_Baud_CB'Access,
                                   Get_Length_CB'Access);

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
      Ret :  MB_Transport.Msg_Length := 0;
   begin

      MB_Rtu.Calc_Times (My_MB_Rtu);

      --  Test for Buffer 1
      Ptr_Buf := Buffer_1'Access;
      Ptr_Tim := Times_1'Access;
      Recv_Count := 1;

      Ret := MB_Rtu.Recv (My_MB_Rtu, Milliseconds (100));

      Assert (Ret = Buffer_Exp'Length,
              "Incorrect total bytes sent");

      for I in 1 .. Buffer_Exp'Length loop
         Assert (My_MB_Rtu.Buffer (I) = Buffer_Exp (I),
                 "Incorrect data sent at" & I'Image);
      end loop;

      --  Test for Buffer 2
      Ptr_Buf := Buffer_2'Access;
      Ptr_Tim := Times_2'Access;
      Recv_Count := 1;

      Ret := MB_Rtu.Recv (My_MB_Rtu, Milliseconds (100));

      Assert (Ret = 0,
              "Incorrect total bytes sent");

   end Run_Test;

end Mb_Rtu_Recv_Test;
