with MB_Types;
with Ada.Real_Time; use Ada.Real_Time;

package MB_Transport is

   Msg_Max_Length : constant Integer := 253;
   subtype Msg_Length is Integer range 0 .. Msg_Max_Length;

   type MB_Transport_Type is abstract tagged null record;

   procedure Send (Self : in out MB_Transport_Type ;
                   Buffer : MB_Types.Byte_Array ;
                   Length : MB_Transport.Msg_Length) is abstract;

   function Recv (Self : in out MB_Transport_Type ; Timeout : Time_Span) return Msg_Length is abstract;

end MB_Transport;
