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

-- This file implements an interface between Serial Call Backs required in
-- Modbus and the Serial Port handled by POSIX.

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Streams; use Ada.Streams;
with GNAT.Serial_Communications; use GNAT.Serial_Communications;

package body Serial is
   Name : Port_Name := "/dev/pts/3";
   Port : Serial_Port;
   
   ---------------------------------------------------------------------------
   -- Recv Function
   ---------------------------------------------------------------------------
   function Recv (Data : out Byte;
                  Timeout : in Ada.Real_Time.Time_Span) return Boolean is
      Start_Time : Ada.Real_Time.Time := Clock;
      Buffer : Stream_Element_Array (1 .. 1);
      Last   : Stream_Element_Offset;
   begin
      
      loop
         
         Read (Port, Buffer, Last);

         -- if data has been received, store it in Data and return
         if Last >= Buffer'First then
            Data := Byte(Buffer(Buffer'First));
            return True;
         end if;
      
         -- if no data has been received, check for timeout
         if Clock - Start_Time >= Timeout then
            return False;
         end if;
         
      end loop;
      
   end Recv;

   ---------------------------------------------------------------------------
   -- Recv Procedure
   ---------------------------------------------------------------------------
   procedure Send (Data : in Byte) is
      Buffer : Stream_Element_Array (1 .. 1);
   begin
      Buffer (1) := Stream_Element (Data);
      Write (Port, Buffer);
   end Send;
   
begin 
  
   -- open serial port
   Open (Port, Name);
   
   -- Set receive timeout to 0.05 seconds. Setting a value lower than this 
   -- may cause POSIX to use a spinlock and consume CPU unnecessarily. 
   -- In this case, we don't need a timeout less than 0.05 seconds.
   Set (Port => Port, Timeout => 0.050);
   
end Serial;
