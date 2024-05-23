# Ada_Modbus [![CI](https://github.com/EmbeddedFramework/Ada_Modbus/workflows/CI/badge.svg)](https://github.com/EmbeddedFramework/Ada_Modbus/actions)
This is an implementation of Modbus in Ada.
# How to use:
First, you need to implement the serial callbacks according to the provided description:
```ada
   ---------------------------------------------------------------------------
   -- Description: Receive data from the serial port
   -- Parameters:
   --   - Data   : Data received
   --   - Timeout: Maximum time to wait for a data.
   -- Return: True if data has been received.
   ---------------------------------------------------------------------------
   type Recv_CB is access function (Data : out Byte ;
                                    Timeout : in Ada.Real_Time.Time_Span)
                                    return Boolean;
```
```ada
   ---------------------------------------------------------------------------
   -- Description: Send data to the serial port
   -- Parameters:
   --   - Data   : Data to be sent
   ---------------------------------------------------------------------------
   type Send_CB is access procedure (Data : in Byte);
```
If you want to use Modbus Slave, you need to implement the desired function like F0X03 (Read Holding Registers) and F0X10 (Write Multiple Registers).

Finally, use the desired transport layer (MB_Ascii or MB_Rtu) to send and receive messages, and the application layer (MB_Slave or MB_Master) to process them.

For more information, take a look at the examples folder.

Enjoy!
