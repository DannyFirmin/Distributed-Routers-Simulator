--
--  Framework: Uwe R. Zimmer, Australia, 2015
--

with Generic_Message_Structures;
with Generic_Router_Links;
with Id_Dispenser;
with Queue_Pack_Protected_Generic;

generic

   with package Message_Structures is new Generic_Message_Structures (<>);

package Generic_Router is

   use Message_Structures;
   use Routers_Configuration;

   package Router_Id_Generator is new Id_Dispenser (Element => Router_Range);
   use Router_Id_Generator;

   type Router_Task;
   type Router_Task_P is access all Router_Task;

   package Router_Link is new Generic_Router_Links (Router_Range, Router_Task_P, null);
   use Router_Link;

   type Queue_Element is record
      Msg : Router_Messages;
      Id : Router_Range;
      Link : Router_Task_P;
   end record;

   type Queue_Size is mod 20;

   package Queue_Pack_Msg is
     new Queue_Pack_Protected_Generic (Element => Queue_Element, Index => Queue_Size);
   use Queue_Pack_Msg;

   task type Router_Task (Task_Id  : Router_Range := Draw_Id) is

      entry Configure (Links : Ids_To_Links);

      entry Send_Message    (Message :     Messages_Client);
      entry Receive_Message (Message : out Messages_Mailbox);

      entry Alive_Test;
      entry Shutdown;

      -- Leave anything above this line as it will be used by the testing framework
      -- to communicate with your router.

      --  Add one or multiple further entries for inter-router communications here
      entry Router_Send (Message : Router_Messages);
      entry Router_Receive (Message : out Router_Messages);

   end Router_Task;

end Generic_Router;
