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

with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;
with Mb_Ascii_Send_Test;
with Mb_Ascii_Recv_Test;
with Mb_Rtu_Send_Test;
with Mb_Rtu_Recv_Test;
with Mb_Slave_F0x03_Test;
with Mb_Slave_F0x03_Test_2;
with Mb_Slave_F0x10_Test;
with Mb_Master_F0x03_Test;
with Mb_Master_F0x10_Test;

package body Modbus_Suite is

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Test_Case_Access'(new Mb_Ascii_Send_Test.Test));
      Ret.Add_Test (Test_Case_Access'(new Mb_Ascii_Recv_Test.Test));
      Ret.Add_Test (Test_Case_Access'(new Mb_Rtu_Send_Test.Test));
      Ret.Add_Test (Test_Case_Access'(new Mb_Rtu_Recv_Test.Test));
      Ret.Add_Test (Test_Case_Access'(new Mb_Slave_F0x03_Test.Test));
      Ret.Add_Test (Test_Case_Access'(new Mb_Slave_F0x03_Test_2.Test));
      Ret.Add_Test (Test_Case_Access'(new Mb_Slave_F0x10_Test.Test));
      Ret.Add_Test (Test_Case_Access'(new Mb_Master_F0x03_Test.Test));
      Ret.Add_Test (Test_Case_Access'(new Mb_Master_F0x10_Test.Test));
      return Ret;
   end Suite;

end Modbus_Suite;
