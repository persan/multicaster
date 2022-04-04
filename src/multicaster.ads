with Ada.Strings.Bounded;
with Ada.Calendar;
package Multicaster is
   package Strings_64 is new Ada.Strings.Bounded.Generic_Bounded_Length (64);

   type Message_Type is record
      Counter : Natural := 1;
      Time    : Ada.Calendar.Time;
      Source  : Strings_64.Bounded_String;
   end record;
   for Message_Type'Alignment use 1;

end Multicaster;
