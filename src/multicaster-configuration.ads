with GNATCOLL.Opt_Parse;    use GNATCOLL.Opt_Parse;
with GNAT.Strings;
with GNAT.Sockets;
with Ada.Streams;
package Multicaster.Configuration is
   Parser : Argument_Parser := Create_Argument_Parser
     (Help => "Help string for the parser");

   function Convert (Item : String ) return  GNAT.Strings.String_Access is
     (new String'(Item));

   function Convert (Item : String ) return  Duration is
     (Duration'Value (Item));

   function Convert (Item : String ) return  GNAT.Sockets.Port_Type is
     (GNAT.Sockets.Port_Type'Value (Item));

   function Convert (Item : String ) return  Ada.Streams.Stream_Element_Offset is
     (Ada.Streams.Stream_Element_Offset'Value (Item));
   --  -------------------------------------------------------------------------

   package Quiet is new Parse_Flag
     (Parser => Parser,
      Short  => "-q",
      Long   => "--quiet",
      Help   => "Whether the tool should be quiet or not");

   --  -------------------------------------------------------------------------

   Default_Group : constant GNAT.Strings.String_Access := Convert ("239.255.128.128");

   package Group is new Parse_Option
     (Parser      => Parser,
      Short       => "-g",
      Long        => "--group",
      Arg_Type    => GNAT.Strings.String_Access,
      Help        => "What Multicast group to use. Default is """ & Default_Group.all & """.",
      Default_Val => Default_Group);

   --  -------------------------------------------------------------------------

   Default_Name : constant GNAT.Strings.String_Access := Convert (GNAT.Sockets.Host_Name);

   package Name is new Parse_Option
     (Parser      => Parser,
      Short       => "-n",
      Long        => "--name",
      Arg_Type    => GNAT.Strings.String_Access,
      Help        => "What name to use. Default is """ & Default_Name.all & """.",
      Default_Val => Default_Name);

   --  -------------------------------------------------------------------------

   Default_Count : Natural := 1000;

   package Count is new Parse_Option
     (Parser      => Parser,
      Short       => "-c",
      Long        => "--count",
      Arg_Type    => Natural,
      Help        => "How many samples to send. Default is" & Default_Count'Img & ".",
      Default_Val => Default_Count);

   Default_Program_Count : Natural := 10;

   package Program_Count is new Parse_Option
     (Parser      => Parser,
      Long        => "--program-count",
      Arg_Type    => Natural,
      Help        => "How many programms to allocate reception buffers fo. Default is 10.",
      Default_Val => 0);

   --  -------------------------------------------------------------------------

   Default_Delay_Time : Duration := 0.001;

   package Delay_Time is new Parse_Option
     (Parser      => Parser,
      Short       => "-t",
      Long        => "--time",
      Arg_Type    => Duration,
      Help        => "How Howe long to wait between each sample. Default is" & Default_Delay_Time'Img & ".",
      Default_Val => Default_Delay_Time);

   --  -------------------------------------------------------------------------

   Default_Ballast_Size : Ada.Streams.Stream_Element_Offset := 40_000;

   package Ballast_Size is new Parse_Option
     (Parser      => Parser,
      Short       => "-s",
      Long        => "--size",
      Arg_Type    => Ada.Streams.Stream_Element_Offset,
      Help        => "Size of ballast in message. Default is" & Default_Ballast_Size'Img & ".",
      Default_Val => Default_Ballast_Size);

   Default_Port : GNAT.Sockets.Port_Type := 55506;

   package Port is new Parse_Option
     (Parser      => Parser,
      Short       => "-p",
      Long        => "--Port",
      Arg_Type    => GNAT.Sockets.Port_Type,
      Help        => "Port to use. Default is" & Default_Port'Img & ".",
      Default_Val => Default_Port);


   package Trace is new Parse_Flag
     (Parser      => Parser,
      Long        => "--trace",
      Help        => "Enable Exception_Traces.");

end Multicaster.Configuration;
