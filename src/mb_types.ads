with Interfaces;

package MB_Types is
   
   subtype Byte is Interfaces.Unsigned_8;
   
   type Byte_Array is array (Positive range <>) of Byte;

end MB_Types;
