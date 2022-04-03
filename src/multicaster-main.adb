with GNAT.Sockets; use GNAT.Sockets;

with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with GNAT.Exception_Traces;
with Multicaster.Configuration;

procedure Multicaster.Main is
   use Multicaster.Configuration;
   use Strings_256;
   Continue : Boolean := True;
   task Pong is
      entry Start;
      entry Stop;
   end Pong;

   task body Pong is
      Address  : Sock_Addr_Type;
      Socket   : Socket_Type;
      Channel  : Stream_Access;

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

      Channel := Stream (Socket, Address);

      --  Receive and print message from client Ping

      while Continue loop
         declare
            Message : constant Message_Type := Message_Type'Input (Channel);

         begin
            Address := Get_Address (Channel);
            Ada.Text_IO.Put_Line (To_String (Message.Source) & ":" & Message.Counter'Img & ",size=>" & Integer'(Message'Size / 8)'Img);

         end;
      end loop;
      Close_Socket (Socket);

      accept Stop;

   exception when E : others =>
         Ada.Text_IO.Put_Line
           (Exception_Name (E) & ": " & Exception_Message (E));
   end Pong;

   task Ping is
      entry Start;
      entry Stop;
   end Ping;

   task body Ping is
      Address  : Sock_Addr_Type;
      Socket   : Socket_Type;
      Channel  : Stream_Access;
      Message  : Message_Type :=  Message_Type'(Ballast_Size => Configuration.Ballast_Size.Get,
                                                Counter      => 0,
                                                Time         => Ada.Calendar.Clock,
                                                Source       => To_Bounded_String (Configuration.Name.Get.all),
                                                Ballast      => (others => '-'));
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

      Channel := Stream (Socket, Address);


      --  Receive and print message from server Pong

      while Continue loop
         Message.Counter := Message.Counter + 1;
         Message.Time    := Ada.Calendar.Clock;
         Message_Type'Output (Channel, Message);
      end loop;
      Close_Socket (Socket);

      accept Stop;

   exception when E : others =>
         Ada.Text_IO.Put_Line
           (Exception_Name (E) & ": " & Exception_Message (E));
   end Ping;

begin
   if Multicaster.Configuration.Parser.Parse then
      GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
      GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback_No_Hex'Access);
      Ping.Start;
      Pong.Start;
      delay Configuration.Exec_Time.Get;
      Continue := False;
      Ping.Stop;
      Pong.Stop;
   end if;
end Multicaster.Main;
