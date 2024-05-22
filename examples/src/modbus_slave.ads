
with System;

package Modbus_Slave is

   -- Declare the task type
   task Modbus is
      pragma Storage_Size (4 * 1024);
      pragma Priority (System.Default_Priority);
   end Modbus;

end Modbus_Slave;
