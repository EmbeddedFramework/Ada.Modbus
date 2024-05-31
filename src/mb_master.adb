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

with MB_Transport; use MB_Transport;
with Ada.Real_Time; use Ada.Real_Time;
with Interfaces; use Interfaces;
with MB_Types;
with MB_Protocol; use MB_Protocol;
package body MB_Master is

   function Error_Code_Value (E_C : Error_Code_Type) return MB_Types.Byte is
   begin
      case E_C is
         when E_OK                 => return MB_Protocol.E_OK;
         when E_FNC_NOT_SUPPORTED  => return MB_Protocol.E_FNC_NOT_SUPPORTED;
         when E_WRONG_STR_ADDR     => return MB_Protocol.E_WRONG_STR_ADDR;
         when E_WRONG_REG_QTY      => return MB_Protocol.E_WRONG_REG_QTY;
         when E_FNC_ERROR          => return MB_Protocol.E_FNC_ERROR;
         when E_SLAVE_NO_RESPONSE  => return MB_Protocol.E_SLAVE_NO_RESPONSE;
         when E_INCORRECT_RESPONSE => return MB_Protocol.E_INCORRECT_RESPONSE;
      end case;
   end Error_Code_Value;

   function Error_Code_From_Value (Value : MB_Types.Byte)
                                   return Error_Code_Type is
   begin
      case Value is
         when MB_Protocol.E_OK                 => return E_OK;
         when MB_Protocol.E_FNC_NOT_SUPPORTED  => return E_FNC_NOT_SUPPORTED;
         when MB_Protocol.E_WRONG_STR_ADDR     => return E_WRONG_STR_ADDR;
         when MB_Protocol.E_WRONG_REG_QTY      => return E_WRONG_REG_QTY;
         when MB_Protocol.E_FNC_ERROR          => return E_FNC_ERROR;
         when MB_Protocol.E_SLAVE_NO_RESPONSE  => return E_SLAVE_NO_RESPONSE;
         when MB_Protocol.E_INCORRECT_RESPONSE => return E_INCORRECT_RESPONSE;
         when others                           => return E_INCORRECT_RESPONSE;
      end case;
   end Error_Code_From_Value;



   function Read_Hold_Reg(Self     : in out MB_Master_Type;
                          Buffer   : out MB_Types.Holding_Register_Array;
                          Address  : in MB_Types.Address;
                          Quantity : in MB_Types.Quantity;
                          Id       : in MB_Types.Byte)
                          return Error_Code_Type is

      -- Create an alias for the buffer byte. We are going to use the
      -- Byte_Array of the given transport
      Buffer_Byte : MB_Types.Byte_Array renames Self.Transport.all.Buffer;
      Retry_Cnt : Natural := Self.Retries;
      Length : MB_Transport.Msg_Length;
      E_C    : Error_Code_Type;
   begin

      if Quantity < 1 or Quantity > F0x03_Max_Qty then
         return E_WRONG_REG_QTY;
      end if;

      loop
         exit when Retry_Cnt <= 0;

         -- Set ID
         Length := ID_Pos;
         Buffer_Byte (Length) := Id;

         -- Set Function Code
         Length := PDU_Pos;
         Buffer_Byte (Length) := FCN_READ_HOLDING_REGISTERS;

         -- Set Register address
         MB_Types.Write_Word (Address, Buffer_Byte, Length+1);
         Length := Length + 2;

         -- Set quantity of registers
         MB_Types.Write_Word (Quantity, Buffer_Byte, Length+1);
         Length := Length + 2;

         -- Send the message to the slave
         Send (Self.Transport.all, Buffer_Byte, Length);

         -- Receive the message from the slave
         Length := Recv (Self.Transport.all, Self.Timeout);

         Retry_Cnt := Retry_Cnt - 1;

         -- If the slave didn't send a response, set Error Code
         if Length = 0 then
            E_C := E_SLAVE_NO_RESPONSE;

         -- If the ID doesn't match, set Error Code
         elsif Buffer_Byte (ID_Pos) /= Id then
            E_C := E_INCORRECT_RESPONSE;

         -- If the Function doesn't match, set Error Code
         elsif Buffer_Byte (PDU_Pos) /= FCN_READ_HOLDING_REGISTERS then
            E_C := E_INCORRECT_RESPONSE;

         -- If the slave sent an error code, assign it to E_C
         elsif Buffer_Byte (PDU_Pos) =
              (FCN_READ_HOLDING_REGISTERS or MB_Protocol.ERROR_FLAG) then
            E_C := Error_Code_From_Value (Buffer_Byte (PDU_Pos + 1 ));
            Retry_Cnt := 0;

         -- If the total byte doesn't match, set Error Code
         elsif Integer (Buffer_Byte (PDU_Pos + 1)) /=
               Integer ((Quantity * 2)) then
            E_C := E_INCORRECT_RESPONSE;

         -- If the message length doesn't match, set Error Code
         elsif Length /= MB_Transport.Msg_Length ((1 + 1 + 1 + Quantity*2)) then
            E_C := E_INCORRECT_RESPONSE;

         -- If everything fine, copy the received data to the holding registers
         else
            MB_Types.Read_Multiples_Words (Buffer, 1, Quantity, Buffer_Byte,
                                           PDU_Pos + 2);

            E_C := E_OK;
            Retry_Cnt := 0;
         end if;

      end loop;

      return E_C;

   end Read_Hold_Reg;


   function Write_Mult_Reg(Self     : in out MB_Master_Type;
                           Buffer   : in MB_Types.Holding_Register_Array;
                           Address  : in MB_Types.Address;
                           Quantity : in MB_Types.Quantity;
                           Id : in MB_Types.Byte) return Error_Code_Type is

      -- Create an alias for the buffer byte. We are going to use the
      -- Byte_Array of the given transport
      Buffer_Byte : MB_Types.Byte_Array renames Self.Transport.all.Buffer;
      Retry_Cnt : Natural := Self.Retries;
      Length : MB_Transport.Msg_Length;
      E_C    : Error_Code_Type;
   begin

      if Quantity < 1 or Quantity > F0x10_Max_Qty then
         return E_WRONG_REG_QTY;
      end if;

      loop
         exit when Retry_Cnt <= 0;

         -- Set ID
         Length := ID_Pos;
         Buffer_Byte (Length) := Id;

         -- Set Function Code
         Length := PDU_Pos;
         Buffer_Byte (Length) := FCN_WRITE_MULTIPLE_REGISTERS;

         -- Set Register address
         MB_Types.Write_Word (Address, Buffer_Byte, Length+1);
         Length := Length + 2;

         -- Set quantity of registers
         MB_Types.Write_Word (Quantity, Buffer_Byte, Length+1);
         Length := Length + 2;

         -- Byte count
         Length := Length + 1;
         Buffer_Byte (Length) := MB_Types.Byte (Quantity * 2);

         -- Registers Value
         MB_Types.Write_Multiples_Words (Buffer, 1, Quantity, Buffer_Byte,
                                        Length + 1);

         Length := Length + MB_Transport.Msg_Length (Quantity * 2);

         -- Send the message to the slave
         Send (Self.Transport.all, Buffer_Byte, Length);

         -- Receive the message from the slave
         Length := Recv (Self.Transport.all, Self.Timeout);

         Retry_Cnt := Retry_Cnt - 1;





      end loop;

      return E_C;

   end Write_Mult_Reg;


end MB_Master;
