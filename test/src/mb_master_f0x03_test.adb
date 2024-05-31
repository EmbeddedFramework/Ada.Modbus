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
with MB_Master;
use MB_Master;
with Interfaces; use Interfaces;

package body Mb_Master_F0x03_Test is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Modbus Master F0x03");
   end Name;

   Buffer_Recv : constant MB_Types.Byte_Array :=
     (1 => 16#01#, 2 => 16#03#, 3 => 16#04#,
      4 => 16#12#, 5 => 16#34#,
      6 => 16#56#, 7 => 16#78#);

   Buffer_HR : MB_Types.Holding_Register_Array (1 .. 2);

   overriding
   procedure Send (Self : in out My_MB_Transport_Type;
                   Buffer : MB_Types.Byte_Array;
                   Length : MB_Transport.Msg_Length) is
   begin
      null;
   end Send;

   overriding
   function Recv (Self : in out My_MB_Transport_Type;
                  Timeout : Time_Span) return MB_Transport.Msg_Length is

   begin
      for I in 1 .. Buffer_Recv'Length loop
         Self.Buffer (I) := Buffer_Recv (I);
      end loop;

      return Buffer_Recv'Length;
   end Recv;

   My_MB_Transport : aliased My_MB_Transport_Type;

   My_MB_Master : MB_Master.MB_Master_Type :=
     (Transport => My_MB_Transport'Access,
     Retries => 5,
     Timeout => Milliseconds (100));

   EC : MB_Master.Error_Code_Type;

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin

      EC := MB_Master.Read_Hold_Reg (My_MB_Master, Buffer_HR, 0, 2, 1);

      Assert (EC = MB_Master.E_OK, "Incorrect EC" & EC'Image);

      Assert (Buffer_HR (1) = 16#1234#, "Incorrect HR" & Buffer_HR (1)'Image);
      Assert (Buffer_HR (2) = 16#5678#, "Incorrect HR" & Buffer_HR (1)'Image);

   end Run_Test;

end Mb_Master_F0x03_Test;
