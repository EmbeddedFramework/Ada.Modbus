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

-- This file defines the interface for all low-layer formats such as
-- Modbus ASCII, RTU, and TCP.
-- All the interface modules extend MB_Transport_Type by adding necessary
-- fields and implementing Send and Recv procedures.
-- The buffers always start with ID (1 byte) + PDU (up to 253 bytes)

with MB_Types;
with Ada.Real_Time; use Ada.Real_Time;

package MB_Transport is

   PDU_Length : constant Integer := 253;
   ID_PDU_Length : constant Integer := 1 + PDU_Length;

   -- Use the following type to manage index securely in the modbus buffers
   subtype Msg_Length is Integer range 0 .. ID_PDU_Length;

   type MB_Transport_Type is abstract tagged null record;

   procedure Send (Self : in out MB_Transport_Type ;
                   Buffer : MB_Types.Byte_Array ;
                   Length : MB_Transport.Msg_Length) is abstract;

   function Recv (Self : in out MB_Transport_Type ; Timeout : Time_Span)
                  return Msg_Length is abstract;

end MB_Transport;
