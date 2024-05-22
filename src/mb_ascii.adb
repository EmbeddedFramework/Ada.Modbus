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

with MB_Transport;
with Ada.Real_Time; use Ada.Real_Time;
with Interfaces; use Interfaces;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with MB_Types; use MB_Types;

package body MB_Ascii is

   Start_Byte : constant  Byte := 58;
   End1_Byte : constant  Byte := 13;
   End2_Byte : constant  Byte := 10;

   subtype Msg_Length is Integer range 0 .. MB_Ascii.Msg_Max_Length;
   subtype Nibble is Interfaces.Unsigned_8 range 0 .. 15;

   -- ':' + ID + FNC_COD + LRC + CR + LF
   Min_Msg_Length : constant Msg_Length := 1 + 2 + 2 + 2 + 1 + 1;

   ---------------------------------------------------------------------------
   -- Description: Returns the LRC (Longitudinal Redundancy Check) of the
   --              given buffer
   -- Parameters:
   --   - Buffer :
   --   - Length :
   -- Return: LRC Calculated
   ---------------------------------------------------------------------------
   function Calc_LRC (Buffer :  Byte_Array ;
                      Length : MB_Transport.Msg_Length) return  Byte is
      Result :  Byte := 0;
   begin
      for I in 1 .. Length loop
         Result := Result + Buffer(I);
      end loop;
      return -Result;
   end Calc_LRC;

   ---------------------------------------------------------------------------
   -- Description: Check if the LRC of the given buffer is correct. Assumes
   --              that the LRC is the last byte of the array.
   -- Parameters:
   --   - Buffer :
   --   - Length :
   -- Return: True if the LRC is correct
   --         False if the LRC in incorrect
   ---------------------------------------------------------------------------
   function Check_LRC (Buffer :  Byte_Array ;
                      Length : MB_Transport.Msg_Length) return Boolean is
      Lrc :  Byte;
   begin
      Lrc := Calc_LRC (Buffer, Length - 1);
      return Lrc = Buffer(Length);
   end Check_LRC;

   ---------------------------------------------------------------------------
   -- Description: Convert Nibble to a Byte
   -- Parameters:
   --   - N :
   -- Return: Ascii byte from the given Nibble
   ---------------------------------------------------------------------------
   function Nibble_To_Char(N : Nibble) return Byte is
   begin
      if N < 10 then
         return  Byte(Character'Pos('0') + Nibble'Pos(N));
      else
         return  Byte(Character'Pos('A') + Nibble'Pos(N) - 10);
      end if;
   end Nibble_To_Char;

   ---------------------------------------------------------------------------
   -- Description: This function calls Serial_Recv call back to receive a
   --              byte and checks if it is the expected one.
   -- Parameters:
   --   - Self : Modbus Ascii Object
   --   - B : Expected value
   --   - Exit_On_Diff: Allows retrying if the received byte does not
   --                   match the expected one.
   --   - Dead_Line
   -- Return: True if the reception was correct
   ---------------------------------------------------------------------------
   function Wait_For_Byte (Self : in out MB_Ascii_Type;
                           B :  Byte;
                           Exit_On_Diff : Boolean;
                           Dead_Line : Time) return Boolean is
      Byte_Rec :  Byte;
      Ret : Boolean;
      Timeout : Time_Span;
   begin
      loop
         Timeout := Dead_Line - Clock;

         if Timeout <= Time_Span_Zero then
            return False;
         end if;

         Ret := Self.Serial_Recv (Byte_Rec, Timeout);
         if Ret = True then
            if Byte_Rec = B then
               return True;
            else
               if Exit_On_Diff then
                  return False;
               end if;
            end if;
         else
            return False;
         end if;
      end loop;
   end Wait_For_Byte;

   ---------------------------------------------------------------------------
   -- Description: Check if the given byte is a valid Nibble
   -- Parameters:
   --   - B : The byte to check.
   -- Return: True if it is between 1..9 A..F
   ---------------------------------------------------------------------------
   function Is_Valid_Byte(B :  Byte) return Boolean is
   begin
      if (B >=  Byte(Character'Pos('0')) and B <=  Byte(Character'Pos('9'))) or
         (B >=  Byte(Character'Pos('A')) and B <=  Byte(Character'Pos('F'))) then
         return True;
      else
         return False;
      end if;
   end Is_Valid_Byte;

   ---------------------------------------------------------------------------
   -- Description: Convert a byte to a Nibble
   -- Parameters:
   --   - B : The byte to convert.
   -- Return: Nibble value
   ---------------------------------------------------------------------------
   function Nibble_Value(B :  Byte) return Nibble is
   begin
      if B >= Byte(Character'Pos('0')) and B <= Byte(Character'Pos('9')) then
         return B - Byte(Character'Pos('0'));
      elsif B >= Byte(Character'Pos('A')) and B <= Byte(Character'Pos('F')) then
         return B - Byte(Character'Pos('A')) + 10;
      else
         raise Constraint_Error with "Invalid byte value";
      end if;
   end Nibble_Value;

   ---------------------------------------------------------------------------
   -- Description: Combine two Bytes (in Nibble range) to obtain a single Byte
   -- Parameters:
   --   - High : High ASCII nibble.
   --   - Low : Low ASCII nibble.
   -- Return: combined byte value
   ---------------------------------------------------------------------------
   function Combine_Bytes(High, Low :  Byte) return  Byte is
      High_Nibble :  Byte;
      Low_Nibble  :  Byte;
   begin
      if not Is_Valid_Byte(High) or not Is_Valid_Byte(Low) then
         raise Constraint_Error with "Invalid byte value";
      end if;

      High_Nibble := Nibble_Value(High);
      Low_Nibble  := Nibble_Value(Low);

      return (High_Nibble * 16) + Low_Nibble;
   end Combine_Bytes;

   -- =========================================================================
   -- Public procedures and functions
   -- =========================================================================

   overriding
   procedure Send (Self : in out MB_Ascii_Type ;
                   Buffer :  Byte_Array ;
                   Length : MB_Transport.Msg_Length) is
      Ascii_Length : Msg_Length := Length;
      High : Nibble := 0;
      Low : Nibble := 0;

   begin

      for I in 1 .. Length loop
         Self.Buffer(I) := Buffer(I);
      end loop;

      Self.Buffer (Length + 1) := Calc_LRC(Self.Buffer, Ascii_Length);
      Ascii_Length := Ascii_Length + 1;

      for I in reverse 1 .. Ascii_Length loop
         High := Nibble(Self.Buffer(I) / 16);
         Low := Nibble(Self.Buffer(I) mod 16);

         Self.Buffer (I * 2 + 0) := Nibble_To_Char(High);
         Self.Buffer (I * 2 + 1) := Nibble_To_Char(Low);
      end loop;

      Self.Buffer (1) := Start_Byte;
      Ascii_Length := Ascii_Length * 2 + 1;

      Ascii_Length := Ascii_Length + 1;
      Self.Buffer (Ascii_Length) := End1_Byte;

      Ascii_Length := Ascii_Length + 1;
      Self.Buffer (Ascii_Length) := End2_Byte;

      for I in 1 .. Ascii_Length loop
         Self.Serial_Send(Self.Buffer(I));
      end loop;

   end Send;

   overriding
   function Recv (Self :  in out MB_Ascii_Type ;
                  Timeout : Time_Span) return MB_Transport.Msg_Length is
      Start_Time : Time := Clock;
      Time_Remaining : Time_Span := Timeout;
      Dead_Line : Time := Timeout + Clock;
      Ret : Boolean;
      Index : Msg_Length := 0;
      Byte_Rec : Byte;
   begin

      -- Wait for ':'
      if Wait_For_Byte (Self, Character'Pos(':'), False, Dead_Line) = False
      then
         return 0;
      end if;

      -- Reception
      loop

         Time_Remaining := Dead_Line - Clock;

         if Time_Remaining <= Time_Span_Zero then
            return 0;
         end if;

         Ret := Self.Serial_Recv (Byte_Rec, Time_Remaining);

         if Ret = False then
            return 0;
         end if;

         case Byte_Rec is
            when Character'Pos(':') =>
               Index := 0;

            when End2_Byte =>
               Index := 0;

            when End1_Byte =>
               if Index >= Min_Msg_Length then
                  exit;
               else
                  Index := 0;
               end if;

            when others =>
               if Is_Valid_Byte(Byte_Rec) then
                  Index := Index + 1;
                  Self.Buffer (Index) := Byte_Rec;
               else
                  Index := 0;
               end if;

         end case;

      end loop;

      -- Wait for 'LF'
      if Wait_For_Byte (Self, End2_Byte, True, Dead_Line) = False then
         return 0;
      end if;

      if Index mod 2 = 1 then
         return 0;
      end if;

      -- ASCII to BIN

      Index := Index / 2;

      for I in 1 .. Index loop
         Self.Buffer (I) := Combine_Bytes(Self.Buffer (I * 2 - 1),
                                          Self.Buffer (I * 2));
      end loop;

      if Check_LRC(Self.Buffer, Index) then
         return Index - 1;
      else
         return 0;
      end if;

   end Recv;

end MB_Ascii;
