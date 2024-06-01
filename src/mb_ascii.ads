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

-- This file define the Modbus Ascii functions and record.
-- You will need create a MB_Ascii_Type object and provide the call backs
-- for serial data access.
-- When you create the object, it creates a byte buffer store the incomming
-- byte and un pack them ti binary format:
-- The Ascii byte buffer is larger than ID_PDU_Length because the Ascii packed
-- use 2 chars per byte + start byte + LRS (1 byte, 2 chars) + CR + LF

with MB_Types;
with MB_Transport;
with MB_Serial_CB; use MB_Serial_CB;
with Ada.Real_Time; use Ada.Real_Time;

package MB_Ascii is

   Msg_Max_Length : constant Integer := 1 + (MB_Transport.ID_PDU_Length+1) * 2 + 2;

   ---------------------------------------------------------------------------
   -- Description: Modbus ASCII Transoport object
   -- Parameters:
   --   - Recv   : Call back for receive data
   --   - Send   : Call back for send data
   ---------------------------------------------------------------------------
   type MB_Ascii_Type(Serial_Recv : MB_Serial_CB.Recv_CB;
                      Serial_Send : MB_Serial_CB.Send_CB) is
     new MB_Transport.MB_Transport_Type (Msg_Max_Length) with record
      null;
   end record;

   ---------------------------------------------------------------------------
   -- Description: Sends the specified buffer (binary) over the Modbus ASCII
   -- transport.
   -- Parameters:
   --   - Self   : The Modbus ASCII transport object.
   --   - Buffer : The buffer containing the data to send. It can be the
   --              either the same buffer in the Self.Buffer, or other buffer.
   --              If it is the Self.Buffer, note that the data will be
   --              destroyed.
   --   - Length : The length of the data to send.
   ---------------------------------------------------------------------------
   overriding
   procedure Send (Self : in out MB_Ascii_Type ;
                   Buffer : MB_Types.Byte_Array ;
                   Length : MB_Transport.Msg_Length);

   ---------------------------------------------------------------------------
   -- Description: Receives a Modbus ASCII message
   -- Parameters:
   --   - Self   : The Modbus ASCII transport object.
   --   - Timeout: The total time to wait for a correct Modbus ASCII message.
   -- Return: The length of reception (in binary) (ID+PDU). The messaje is
   --         stored in Self.Buffer.
   ---------------------------------------------------------------------------
   overriding
   function Recv (Self : in out MB_Ascii_Type ;
                  Timeout : Time_Span) return MB_Transport.Msg_Length;

end MB_Ascii;
