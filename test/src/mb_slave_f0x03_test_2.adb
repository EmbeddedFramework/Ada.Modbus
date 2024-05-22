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
with MB_Slave;
with MB_Transport;
with MB_Protocol;
with Interfaces; use Interfaces;

package body Mb_Slave_F0x03_Test_2 is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Modbus Slave F0x03, wrong Qty");
   end Name;

   Buffer_Bin : MB_Types.Byte_Array (1 .. MB_Transport.ID_PDU_Length) :=
     (1 => 16#01#, 2 => 16#03#, 3 => 16#00#, 4 => 16#00#,
      5 => 16#00#, 6 => 16#00#, others => 0);

   Buffer_Bin2 : MB_Types.Byte_Array (1 .. MB_Transport.ID_PDU_Length) :=
     (1 => 16#01#, 2 => 16#03#, 3 => 16#00#, 4 => 16#00#,
      5 => 16#00#, 6 => 16#7E#, others => 0);

   Buffer_HR : constant MB_Types.Holding_Register_Array :=
     (1 => 16#0000#,
      2 => 16#1234#,
      3 => 16#5678#,
      4 => 16#9ABC#);

   procedure F0x03 (Start      : MB_Types.Address;
                    Quantity   : MB_Types.Quantity;
                    Exception_Code : out MB_Types.Byte;
                    Buffer     : out MB_Types.Holding_Register_Array) is
      Index : Integer := 1;
   begin

      if Start >= Buffer_HR'Length then
         Exception_Code := MB_Protocol.E_WRONG_STR_ADDR;
         return;
      end if;

      if Quantity > Buffer_HR'Length + Start then
         Exception_Code := MB_Protocol.E_WRONG_STR_ADDR;
         return;
      end if;

      for I in Start + 1 .. Quantity loop
         Buffer (Index) := Buffer_HR (Integer (I));
         Index := Index + 1;
      end loop;

      Exception_Code := MB_Protocol.E_OK;
   end F0x03;


   Cmd : aliased MB_Slave.Cmd_Type :=
     (Cmd_0x03_Read_Holding_Reg => F0x03'Access,
      Cmd_0x10_Write_Holding_Reg => null);

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
      Length : MB_Transport.Msg_Length;
   begin

      Length := MB_Slave.Process (Buffer_Bin, 2, Cmd'Access);

      --  Qty := 0
      Assert (Length = 2, "Incorrect total bytes" & Length'Image);

      Assert (Buffer_Bin (2) = 16#83#,
              "Incorrect Flag error" & Buffer_Bin (2)'Image);

      Assert (Buffer_Bin (3) = MB_Protocol.E_WRONG_REG_QTY,
              "Incorrect Flag error" & Buffer_Bin (3)'Image);


      --  Qty: 0x7E
      Length := MB_Slave.Process (Buffer_Bin2, 2, Cmd'Access);

      Assert (Length = 2, "Incorrect total bytes" & Length'Image);

      Assert (Buffer_Bin (2) = 16#83#,
              "Incorrect Flag error" & Buffer_Bin (2)'Image);

      Assert (Buffer_Bin (3) = MB_Protocol.E_WRONG_REG_QTY,
              "Incorrect Flag error" & Buffer_Bin (3)'Image);

   end Run_Test;

end Mb_Slave_F0x03_Test_2;
