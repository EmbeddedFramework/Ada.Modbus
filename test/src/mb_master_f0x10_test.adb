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

--  Check for correct communication

with AUnit.Assertions; use AUnit.Assertions;
with MB_Master;
use MB_Master;
with Interfaces; use Interfaces;

package body Mb_Master_F0x10_Test is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Modbus Master F0x10");
   end Name;

   --  Buffer for the Recv method
   Buffer_Recv : constant MB_Types.Byte_Array :=
     (1 => 16#01#, 2 => 16#10#,
      3 => 16#00#, 4 => 16#00#,
      5 => 16#00#, 6 => 16#02#);

   --  Buffer and length for storing the sent message
   Buffer_Send : MB_Types.Byte_Array (1 .. MB_Transport.ID_PDU_Length);
   Buffer_Send_Length : MB_Transport.Msg_Length := 0;

   --  Expected values for the Send method
   Buffer_Send_Exp : constant MB_Types.Byte_Array :=
     (1  => 16#01#, 2  => 16#10#,
      3  => 16#00#, 4  => 16#00#,
      5  => 16#00#, 6  => 16#02#,
      7  => 16#04#,
      8  => 16#12#, 9  => 16#34#,
      10 => 16#56#, 11 => 16#78#);

   --  Holding registers
   Buffer_HR : constant MB_Types.Holding_Register_Array :=
     (1  => 16#1234#,
      2  => 16#5678#);

   ---------------------------------------------------------------------------
   --  Send method (overriding from Modbus Transport)
   overriding
   procedure Send (Self : in out My_MB_Transport_Type;
                   Buffer : MB_Types.Byte_Array;
                   Length : MB_Transport.Msg_Length) is
      pragma Unused (Self);
   begin
      for I in 1 .. Length loop
         Buffer_Send (I) := Buffer (I);
      end loop;
      Buffer_Send_Length := Length;
   end Send;

   ---------------------------------------------------------------------------
   --  Recv method (overriding from Modbus Transport)
   overriding
   function Recv (Self : in out My_MB_Transport_Type;
                  Timeout : Time_Span) return MB_Transport.Msg_Length is
   begin
      for I in 1 .. Buffer_Recv'Length loop
         Self.Buffer (I) := Buffer_Recv (I);
      end loop;

      return Buffer_Recv'Length;
   end Recv;

   ---------------------------------------------------------------------------
   --  Objects
   My_MB_Transport : aliased My_MB_Transport_Type;

   My_MB_Master : MB_Master.MB_Master_Type :=
     (Transport => My_MB_Transport'Access,
     Retries => 5,
     Timeout => Milliseconds (100));

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
      EC : MB_Master.Error_Code_Type;
   begin

      EC := MB_Master.Write_Mult_Reg (My_MB_Master, Buffer_HR, 0, 2, 1);

      Assert (EC = MB_Master.E_OK, "Incorrect EC" & EC'Image);

      Assert (Buffer_Send_Length = Buffer_Send_Exp'Length,
              "Incorrect Buffer_Send_Length" & Buffer_Send_Length'Image);

      for I in 1 .. Buffer_Send_Length loop
         Assert (Buffer_Send (I) = Buffer_Send_Exp (I),
                 "Incorrect Buffer_Send at" & I'Image);
      end loop;

   end Run_Test;

end Mb_Master_F0x10_Test;
