with Ada.Streams;
with GNAT.Sockets; use GNAT.Sockets;

with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with GNAT.Exception_Traces;
with Multicaster.Configuration;
pragma Warnings (On);

procedure Multicaster.Pub is
   use Multicaster.Configuration;
   use Ada.Streams;
   use Strings_128;


   procedure Ping is
      Address  : Sock_Addr_Type;
      Socket   : Socket_Type;
      Message  : aliased Message_Type :=  Message_Type'(Counter      => 0,
                                                        Time         => Ada.Calendar.Clock,
                                                        Source       => To_Bounded_String (Configuration.Name.Get.all));
      Buffer   : aliased Ada.Streams.Stream_Element_Array (1 .. Message'Size / Ada.Streams.Stream_Element'Size);
      for Buffer'Address use Message'Address;

      Last     : Ada.Streams.Stream_Element_Offset;
   begin


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
      Address.Port := Any_Port;

      Bind_Socket (Socket, Address);

      Set_Socket_Option
        (Socket,
         IP_Protocol_For_IP_Level,
         (Add_Membership, Inet_Addr (Group.Get.all), Any_Inet_Addr));

      Address.Addr := Inet_Addr (Group.Get.all);
      Address.Port := Configuration.Port.Get;



      for I in 1 .. Configuration.Count.Get loop
         Message.Counter := I;
         Message.Time    := Ada.Calendar.Clock;
         Send_Socket (Socket, Buffer, LAst, Address);
         delay Configuration.Delay_Time.Get;
      end loop;


      Close_Socket (Socket);


   end Ping;

begin

   if Configuration.Parser.Parse then
      if Configuration.Trace.Get then
         GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
         GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback_No_Hex'Access);
      end if;
         Ping;
   end if;
   exception when E : others =>
         Ada.Text_IO.Put_Line
           (Exception_Name (E) & ": " & Exception_Message (E));
end Multicaster.Pub;
