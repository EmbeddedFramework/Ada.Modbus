with Serial;
with GNAT.Serial_Communications;
with MB_Ascii;
with MB_Transport;
with Ada.Real_Time; use Ada.Real_Time;
with MB_Slave;
with Interfaces; use Interfaces;
with MB_Protocol;
with MB_Types;

package body Modbus_Slave is

   My_MB_Ascii : MB_Ascii.MB_Ascii_Type (Serial.Recv'Access, Serial.Send'Access);
   
   Buffer_HR : MB_Types.Holding_Register_Array :=
     (1 => 16#0000#,
      2 => 16#1111#,
      3 => 16#2222#,
      4 => 16#AAAA#);
   
   procedure F0x03 (Start      : MB_Types.Address;
                    Quantity   : MB_Types.Quantity;
                    Exception_Code : out MB_Types.Byte;
                    Buffer     : out MB_Types.Holding_Register_Array) is
      Index : Integer := 1;
   begin

      if Start >= Buffer_HR'Length then
         Exception_Code := MB_Protocol.E_WRONG_STR_ADDR;
         return;
      end if;

      if Quantity > Buffer_HR'Length + Start then
         Exception_Code := MB_Protocol.E_WRONG_STR_ADDR;
         return;
      end if;

      for I in Start + 1 .. Quantity loop
         Buffer_HR (Integer (I)) := Buffer_HR (Integer (I)) + 1;
         Buffer (Index) := Buffer_HR (Integer (I));
         Index := Index + 1;
      end loop;

      Exception_Code := MB_Protocol.E_OK;
   end F0x03;
   
   Cmd : aliased MB_Slave.Cmd_Type :=
     (Cmd_0x03_Read_Holding_Reg => F0x03'Access,
      Cmd_0x10_Write_Holding_Reg => null);
   
   task body Modbus is
      ID_Modbus : constant MB_Types.Byte := 2;
      Length : MB_Transport.Msg_Length;
   begin
      
      loop
         Length := MB_Ascii.Recv (My_MB_Ascii, Milliseconds (3000));
      
         if Length > 0 then
         
            if My_MB_Ascii.Buffer (MB_Transport.ID_Pos) = ID_Modbus then
            
               Length := MB_Slave.Process (My_MB_Ascii.Buffer, 
                                           MB_Transport.PDU_Pos, 
                                           Cmd'Access);
            
               MB_Ascii.Send (My_MB_Ascii, My_MB_Ascii.Buffer, Length+1);
            
            end if;
         end if;
      end loop;
   end Modbus;

end Modbus_Slave;
