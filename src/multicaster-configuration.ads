
with GNATCOLL.Opt_Parse;    use GNATCOLL.Opt_Parse;
with GNAT.Strings;
with GNAT.Sockets;

package Multicaster.Configuration is
   Parser : Argument_Parser := Create_Argument_Parser
     (Help => "Help string for the parser");

   function Convert (Item : String ) return  GNAT.Strings.String_Access is
     (new String'(Item));

   function Convert (Item : String ) return  Duration is
     (Duration'Value (Item));

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
      Short       => "-g",
      Long        => "--group",
      Arg_Type    => GNAT.Strings.String_Access,
      Help        => "What name to use. Default is """ & Default_Name.all & """.",
      Default_Val => Default_Name);

   --  -------------------------------------------------------------------------

   Default_Exec_Time : Duration := 5.0;

   package Exec_Time is new Parse_Option
     (Parser      => Parser,
      Short       => "-t",
      Long        => "--time",
      Arg_Type    => Duration,
      Help        => "How long to execute. Default is" & Default_Exec_Time'img & ".",
      Default_Val => Default_Exec_Time);

end Multicaster.Configuration;
