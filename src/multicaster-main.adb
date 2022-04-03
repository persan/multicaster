with Ada.Streams;
with GNAT.Sockets; use GNAT.Sockets;

with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with GNAT.Exception_Traces;
with Multicaster.Configuration;
procedure Multicaster.Main is
   use Multicaster.Configuration;
   use Ada.Streams;
   use Strings_256;

   Continue : Boolean := True with Warnings => Off;

   task type Pong_Type is
      entry Start;
      entry Stop;
   end Pong_Type;

   task body Pong_Type is
      Address  : Sock_Addr_Type;
      Socket   : Socket_Type;

   begin
      accept Start;
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
      Address.Port := 55505;

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

      Address.Addr := Inet_Addr (Group.Get.all);
      Address.Port := 55506;


      --  Receive and print message from client Ping

      while Continue loop
         declare
            Buffer  : aliased Ada.Streams.Stream_Element_Array (1 .. 16#1_0000#); -- Max UDP Packet size
            Message : Message_Type with Import => True , Address => Buffer'Address;
            Last    : Ada.Streams.Stream_Element_Offset;
         begin
            GNAT.Sockets.Receive_Socket (Socket => Socket, Item => Buffer , Last => Last);
            Ada.Text_IO.Put_Line (To_String (Message.Source) & ":" & Message.Counter'Img & ",size=>" & Last'Img);
         end;
      end loop;
      Close_Socket (Socket);

      accept Stop;

   exception when E : others =>
         Ada.Text_IO.Put_Line
           (Exception_Name (E) & ": " & Exception_Message (E));
   end Pong_Type;

   task type Ping_Type is
      entry Start;
      entry Stop;
   end Ping_Type;

   task body Ping_Type is
      Address  : Sock_Addr_Type;
      Socket   : Socket_Type;
      Message  : aliased Message_Type :=  Message_Type'(Counter      => 0,
                                                        Time         => Ada.Calendar.Clock,
                                                        Source       => To_Bounded_String (Configuration.Name.Get.all));
      Buffer   : aliased Ada.Streams.Stream_Element_Array (1 .. Ballast_Size.Get);
      for Buffer'Address use Message'Address;

      Last     : Ada.Streams.Stream_Element_Offset;

   begin
      accept Start;


      --  Part of multicast example. Code similar to Pong's one

      Create_Socket (Socket, Family_Inet, Socket_Datagram);

      Set_Socket_Option
        (Socket,
         Socket_Level,
         (Reuse_Address, True));

      Set_Socket_Option
        (Socket,
         IP_Protocol_For_IP_Level,
         (Multicast_TTL, 1));

      Set_Socket_Option
        (Socket,
         IP_Protocol_For_IP_Level,
         (Multicast_Loop, True));

      Address.Addr := Any_Inet_Addr;
      Address.Port := 55506;

      Bind_Socket (Socket, Address);

      Set_Socket_Option
        (Socket,
         IP_Protocol_For_IP_Level,
         (Add_Membership, Inet_Addr (Group.Get.all), Any_Inet_Addr));

      Address.Addr := Inet_Addr (Group.Get.all);
      Address.Port := 55505;



      --  Receive and print message from server Pong
      Ada.Text_IO.Put ("-");

      while Continue loop
         Message.Counter := Message.Counter + 1;
         Message.Time    := Ada.Calendar.Clock;
         Send_Socket (Socket, Buffer, LAst, Address);
      end loop;
      Close_Socket (Socket);

      accept Stop;

   exception when E : others =>
         Ada.Text_IO.Put_Line
           (Exception_Name (E) & ": " & Exception_Message (E));
   end Ping_Type;

begin

   if Multicaster.Configuration.Parser.Parse then
      GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
      GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback_No_Hex'Access);
      declare
         Ping : Ping_Type;
         Pong : Pong_Type;
      begin
         Ping.Start;
         Pong.Start;
         delay Configuration.Exec_Time.Get;
         Continue := False;
         Ping.Stop;
         Pong.Stop;

      end;
   end if;
end Multicaster.Main;
