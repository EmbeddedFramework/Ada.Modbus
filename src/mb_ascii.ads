
with MB_Types;
with MB_Transport;
with MB_Serial_CB; use MB_Serial_CB;
with Ada.Real_Time; use Ada.Real_Time;

package MB_Ascii is

   Msg_Max_Length : constant Integer := 1 + (MB_Transport.Msg_Max_Length+1) * 2 + 2;

   type MB_Ascii_Type(Recv : MB_Serial_CB.Recv_CB;
                      Send : MB_Serial_CB.Send_CB) is
     new MB_Transport.MB_Transport_Type with record
      Serial_Recv : MB_Serial_CB.Recv_CB := Recv;
      Serial_Send : MB_Serial_CB.Send_CB := Send;
      Buffer : MB_Types.Byte_Array(1 .. Msg_Max_Length) := (others => 0);
   end record;

   overriding
   procedure Send (Self : in out MB_Ascii_Type ;
                   Buffer : MB_Types.Byte_Array ;
                   Length : MB_Transport.Msg_Length);

   overriding
   function Recv (Self : in out MB_Ascii_Type ; Timeout : Time_Span) return MB_Transport.Msg_Length;

end MB_Ascii;
