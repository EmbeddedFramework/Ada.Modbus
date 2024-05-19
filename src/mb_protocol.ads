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

package MB_Protocol is
   
   -- Modbus Functions definitions
   FCN_READ_COILS                  : constant := 16#01#;
   FCN_READ_DISCRETE_INPUTS        : constant := 16#02#;
   FCN_READ_HOLDING_REGISTERS      : constant := 16#03#;
   FCN_READ_INPUT_REGISTERS        : constant := 16#04#;
   FCN_WRITE_SINGLE_COIL           : constant := 16#05#;
   FCN_WRITE_SINGLE_REGISTER       : constant := 16#06#;
   FCN_WRITE_MULTIPLE_COILS        : constant := 16#0F#;
   FCN_WRITE_MULTIPLE_REGISTERS    : constant := 16#10#;

   -- Modbus Error Codes
   E_OK                            : constant := 16#00#;
   E_FNC_NOT_SUPPORTED             : constant := 16#01#;
   E_WRONG_STR_ADDR                : constant := 16#02#;
   E_WRONG_REG_QTY                 : constant := 16#03#;
   E_FNC_ERROR                     : constant := 16#04#;
   
   ERROR_FLAG                      : constant := 16#80#;
   
end MB_Protocol;
