with MB_Types; use MB_Types;

package MB_Serial_CB is

   type Recv_CB is access function (Data : out Byte ; Timeout : in Duration) return Boolean;

   type Send_CB is access procedure (Data : in Byte);

end MB_Serial_CB;
