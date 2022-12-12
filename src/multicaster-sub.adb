with Ada.Streams;
with GNAT.Sockets; use GNAT.Sockets;

with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with GNAT.Exception_Traces;
with Multicaster.Configuration;
with GNAT.Calendar.Time_IO;
procedure Multicaster.sub is
   use Multicaster.Configuration;
   use Ada.Streams;
   use Strings_128;
   use Ada.Calendar;
procedure Pong is
      Address          : Sock_Addr_Type;
      Socket           : Socket_Type;
      Reception_Buffer : Message_Array (1 .. (if Configuration.Program_Count.Get = 0
                                        then
                                           Configuration.Default_Program_Count
                                        else
                                           Configuration.Program_Count.Get ) * Configuration.Count.Get);
      Cursor           : Natural := Reception_Buffer'First;
   begin
      --  Part of the multicast example

      --  Create a datagram socket to send connectionless, unreliable
      --  messages of a fixed maximum length.

      Create_Socket (Socket, Family_Inet, Socket_Datagram);

      --  Allow reuse of local addresses

      Set_Socket_Option
        (Socket,
         Socket_Level,
         (Reuse_Address, True));

      --  Controls the live time of the datagram to avoid it being
      --  looped forever due to routing errors. Routers decrement
      --  the TTL of every datagram as it traverses from one network
      --  to another and when its value reaches 0 the packet is
      --  dropped. Default is 1.

      Set_Socket_Option
        (Socket,
         IP_Protocol_For_IP_Level,
         (Multicast_TTL, 1));

      --  Want the data you send to be looped back to your host

      Set_Socket_Option
        (Socket,
         IP_Protocol_For_IP_Level,
         (Multicast_Loop, True));

      --  If this socket is intended to receive messages, bind it
      --  to a given socket address.

      Address.Addr := Any_Inet_Addr;
      Address.Port := Configuration.Port.Get;

      Bind_Socket (Socket, Address);

      --  Join a multicast group

      --  Portability note: On Windows, this option may be set only
      --  on a bound socket.

      Set_Socket_Option
        (Socket,
         IP_Protocol_For_IP_Level,
         (Add_Membership, Inet_Addr (Group.Get.all), Any_Inet_Addr));

      --  If this socket is intended to send messages, provide the
      --  receiver socket address.


      loop
         declare
            Buffer  : aliased Ada.Streams.Stream_Element_Array (1 .. 16#1_0000#); -- Max UDP Packet size
            Message : Message_Type with Import => True , Address => Buffer'Address;
            Last    : Ada.Streams.Stream_Element_Offset;
         begin
            GNAT.Sockets.Receive_Socket (Socket => Socket, Item => Buffer , Last => Last);
            exit when Message.Counter = 0 or Cursor >= Reception_Buffer'Last;
            Reception_Buffer (Cursor) := Message;
            Cursor := Cursor + 1;

            if not Configuration.Quiet.Get then
               Ada.Text_IO.Put_Line ( Message.Counter'Image &"," & To_String (Message.Source) & GNAT.Calendar.Time_IO.Image (Message.Time, ", %T.%i, ") & Duration'(Ada.Calendar.Clock - Message.Time)'Image);
            end if;
         end;
      end loop;
      Close_Socket (Socket);


   end Pong;



begin

   if Configuration.Parser.Parse then
      if Configuration.Trace.Get then
         GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
         GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback_No_Hex'Access);
      end if;
         Pong;
   end if;
   exception when E : others =>
         Ada.Text_IO.Put_Line
           (Exception_Name (E) & ": " & Exception_Message (E));
end Multicaster.sub;
