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

with MB_Types;
with MB_Protocol;
with MB_Transport;
with MB_Ascii;
with MB_Slave;
with Serial;

with Ada.Real_Time; use Ada.Real_Time;
with Interfaces; use Interfaces;

package body Modbus_Slave is

   My_MB_Ascii : MB_Ascii.MB_Ascii_Type (Serial.Recv'Access, Serial.Send'Access);
   
   Buffer_HR : MB_Types.Holding_Register_Array :=
     (1 => 16#0000#,
      2 => 16#1111#,
      3 => 16#2222#,
      4 => 16#AAAA#);
   
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
         Buffer_HR (Integer (I)) := Buffer_HR (Integer (I)) + 1;
         Buffer (Index) := Buffer_HR (Integer (I));
         Index := Index + 1;
      end loop;

      Exception_Code := MB_Protocol.E_OK;
   end F0x03;
   
   Cmd : aliased MB_Slave.Cmd_Type :=
     (Cmd_0x03_Read_Holding_Reg => F0x03'Access,
      Cmd_0x10_Write_Holding_Reg => null);
   
   task body Modbus is
      ID_Modbus : constant MB_Types.Byte := 2;
      Length : MB_Transport.Msg_Length;
   begin
      
      loop
         Length := MB_Ascii.Recv (My_MB_Ascii, Milliseconds (3000));
      
         if Length > 0 then
         
            if My_MB_Ascii.Buffer (MB_Transport.ID_Pos) = ID_Modbus then
            
               Length := MB_Slave.Process (My_MB_Ascii.Buffer, 
                                           MB_Transport.PDU_Pos, 
                                           Cmd'Access);
            
               MB_Ascii.Send (My_MB_Ascii, My_MB_Ascii.Buffer, Length+1);
            
            end if;
         end if;
      end loop;
   end Modbus;

end Modbus_Slave;
