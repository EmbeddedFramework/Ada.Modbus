
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Streams;
with GNAT.Serial_Communications; use GNAT.Serial_Communications;
with Interfaces; use Interfaces;
with Ada.Streams; use Ada.Streams;

package body Serial is
   Port : Serial_Port;
   
   function Recv (Data : out Byte;
                  Timeout : in Ada.Real_Time.Time_Span) return Boolean is
      Start_Time : Ada.Real_Time.Time := Clock;
      Buffer : Stream_Element_Array (1 .. 1);
      Last   : Stream_Element_Offset;
   begin
      
      loop
         
         Read (Port, Buffer, Last);

         if Last >= Buffer'First then
            Data := Byte(Buffer(Buffer'First));
            return True;
         end if;
      
         if Clock - Start_Time >= Timeout then
            return False;
         end if;
         
      end loop;
      
   end Recv;

   procedure Send (Data : in Byte) is
      Buffer : Stream_Element_Array (1 .. 1);
   begin
      Buffer (1) := Stream_Element (Data);
      Write (Port, Buffer);
   end Send;
   
begin 
  
   Open (Port, "/dev/pts/2");
   Set (Port => Port, Timeout => 0.05);
   
end Serial;
