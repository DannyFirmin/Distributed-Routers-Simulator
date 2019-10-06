--
--  Uwe R. Zimmer, Australia, September 2011
--

with Ada.Strings.Bounded;                  use Ada.Strings.Bounded;
with Generic_Routers_Configuration;

generic
   with package Routers_Configuration is new Generic_Routers_Configuration (<>);

package Generic_Message_Structures is

   use Routers_Configuration;

   package Message_Strings is new Generic_Bounded_Length (Max => 80);
   use Message_Strings;

   subtype The_Core_Message is Bounded_String;

   type Messages_Client is record
      Destination : Router_Range;
      The_Message : The_Core_Message;
   end record;

   type Messages_Mailbox is record
      Sender      : Router_Range;
      The_Message : The_Core_Message;
      Hop_Counter : Natural := 0;
   end record;

   --  Add one or multiple more messages formats here ..

end Generic_Message_Structures;
