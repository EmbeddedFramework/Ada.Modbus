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
with MB_Transport;
with MB_Serial_CB; use MB_Serial_CB;
with Ada.Real_Time; use Ada.Real_Time;

package MB_Rtu is

   Msg_Max_Length : constant Integer := MB_Transport.ID_PDU_Length + 2;

   ---------------------------------------------------------------------------
   -- Description: Modbus RTU Transport object
   -- Parameters:
   --   - Recv   : Call back for receive data
   --   - Send   : Call back for send data
   --   - Baud   : Call back for get Serial Port Baudrate
   --   - Length : Call back for get total bits length on the the Serial Port
   ---------------------------------------------------------------------------
   type MB_Rtu_Type(Serial_Recv   : not null MB_Serial_CB.Recv_CB;
                    Serial_Send   : not null MB_Serial_CB.Send_CB;
                    Serial_Baud   : not null MB_Serial_CB.Get_Baud_CB;
                    Serial_Length : not null MB_Serial_CB.Get_Length_CB) is
     new MB_Transport.MB_Transport_Type (Msg_Max_Length) with record

      -- private fields
      Time_Byte : Time_Span;
      Time_Inter_Byte : Time_Span;
      Time_Out_Byte : Time_Span;

   end record;

   procedure Calc_Times (Self : in out MB_Rtu_Type);

   ---------------------------------------------------------------------------
   -- Description: Sends the specified buffer (binary) over the Modbus RTU
   -- transport.
   -- Parameters:
   --   - Self   : The Modbus RTU transport object.
   --   - Buffer : The buffer containing the data to send. It can be the
   --              either the same buffer in the Self.Buffer, or other buffer.
   --              If it is the Self.Buffer, note that the data will be
   --              destroyed.
   --   - Length : The length of the data to send.
   ---------------------------------------------------------------------------
   overriding
   procedure Send (Self : in out MB_Rtu_Type ;
                   Buffer : MB_Types.Byte_Array ;
                   Length : MB_Transport.Msg_Length);

   ---------------------------------------------------------------------------
   -- Description: Receives a Modbus RTU message
   -- Parameters:
   --   - Self   : The Modbus RTU transport object.
   --   - Timeout: The total time to wait for a correct Modbus RTU message.
   -- Return: The length of reception (in binary) (ID+PDU). The messaje is
   --         stored in Self.Buffer.
   ---------------------------------------------------------------------------
   overriding
   function Recv (Self : in out MB_Rtu_Type ;
                  Timeout : Time_Span) return MB_Transport.Msg_Length;

end MB_Rtu;
