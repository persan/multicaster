with Ada.Strings.Bounded;
with Ada.Calendar;
package Multicaster is
   package Strings_256 is new Ada.Strings.Bounded.Generic_Bounded_Length (256);

   type Message_Type (Ballast_Size : Positive)is record
      Counter : Natural := 0;
      Time    : Ada.Calendar.Time;
      Source  : Strings_256.Bounded_String;
      Ballast : String (1 .. Ballast_Size);
   end record;
end Multicaster;
