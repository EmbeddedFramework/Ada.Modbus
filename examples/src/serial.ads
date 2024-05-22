with Ada.Real_Time;
with GNAT.Serial_Communications; use GNAT.Serial_Communications;
with Interfaces; use Interfaces;
with MB_Types; use MB_Types;

package Serial is

   function Recv (Data : out Byte;
                  Timeout : in Ada.Real_Time.Time_Span) return Boolean;

   
   procedure Send (Data : in Byte);
   

end Serial;
