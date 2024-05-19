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

with MB_Protocol;
with Interfaces; use Interfaces;
with MB_Types; use MB_Types;


package body MB_Slave is


   function process (Buffer : in out Byte_Array;
                     Start_PDU : MB_Transport.Msg_Length;
                     Length : MB_Transport.Msg_Length;
                     Cmd : Cmd_Type_Ptr)
                     return MB_Transport.Msg_Length is
      Exception_Code : Byte := MB_Protocol.E_OK;
      Ret : MB_Transport.Msg_Length;
   begin

      case Buffer (Start_PDU) is
         when MB_Protocol.FCN_READ_COILS =>
            null;

         when MB_Protocol.FCN_READ_HOLDING_REGISTERS =>
            if Cmd.Cmd_0x03_Read_Holding_Reg = null then
               Exception_Code := MB_Protocol.E_FNC_NOT_SUPPORTED;
            else
               declare
                  Addr : Address := Read_16_Bits (Buffer, 3);
                  Qty  : Address := Read_16_Bits (Buffer, 5);
                  Buffer_HR :
                  Holding_Register_Array (1 .. Standard.Integer (Qty));
               begin

                  if Qty > 16#7D# or Qty < 1 then
                     Exception_Code := MB_Protocol.E_WRONG_REG_QTY;
                  else
                     Ret := Cmd.Cmd_0x03_Read_Holding_Reg (Addr, Qty,
                                                           Exception_Code,
                                                           Buffer_HR);

                  end if;
               end;

            end if;


         when others =>
            null;
      end case;

      if Exception_Code /= MB_Protocol.E_OK then
         Buffer (Start_PDU) := Buffer (Start_PDU) or MB_Protocol.ERROR_FLAG;
         Buffer (Start_PDU + 1) := Exception_Code;
         Ret := 3;
      end if;


      return Ret;

   end process;


end MB_Slave;
