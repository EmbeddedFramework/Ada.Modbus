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
with Ada.Real_Time; use Ada.Real_Time;

package MB_Master is

   -- The next error code enumeration is based on error codes from the Modbus
   -- protocol, and extends it with master errors
   type Error_Code_Type is (E_OK,
                            E_FNC_NOT_SUPPORTED,
                            E_WRONG_STR_ADDR,
                            E_WRONG_REG_QTY,
                            E_FNC_ERROR,
                            E_SLAVE_NO_RESPONSE,
                            E_INCORRECT_RESPONSE);

   type MB_Master_Type is tagged record
      Transport    : MB_Transport.MB_Transport_Access;
      Retries    : Positive;
      Timeout : Time_Span;
   end record;

   ---------------------------------------------------------------------------
   -- Description: Get the value of an error code
   -- Parameters:
   --   - E_C : Error code
   --   Return : Error code value
   ---------------------------------------------------------------------------
   function Error_Code_Value (E_C : Error_Code_Type) return MB_Types.Byte;

   ---------------------------------------------------------------------------
   -- Description: Get Error code from the given value
   -- Parameters:
   --   - Value : Error value
   --   Return : Error code
   ---------------------------------------------------------------------------
   function Error_Code_From_Value (Value : MB_Types.Byte)
                                   return Error_Code_Type;

   ---------------------------------------------------------------------------
   -- Description: Execute the function "Read Holding Register" on a Slave
   -- Parameters:
   --   - Self     : Modbus Master Object
   --   - Buffer   : Buffer to store the values read from the holding registers
   --   - Address  : Address to stard reading
   --   - Quantity : Number of holding register to read
   --   - Id       : Slave Id
   --   Return : Error code
   ---------------------------------------------------------------------------
   function Read_Hold_Reg(Self     : in out MB_Master_Type;
                          Buffer   : out MB_Types.Holding_Register_Array;
                          Address  : in MB_Types.Address;
                          Quantity : in MB_Types.Quantity;
                          Id       : in MB_Types.Byte) return Error_Code_Type;

end MB_Master;
