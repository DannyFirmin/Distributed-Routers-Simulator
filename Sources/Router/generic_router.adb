--
--  Framework: Uwe R. Zimmer, Australia, 2019
--

with Exceptions; use Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
package body Generic_Router is

   task body Router_Task is

      Connected_Routers : Ids_To_Links;
      My_Routing_Table : Routing_Table;

   begin
      accept Configure (Links : Ids_To_Links) do
         Connected_Routers := Links;
      end Configure;

      declare
         Port_List : constant Connected_Router_Ports := To_Router_Ports (Task_Id, Connected_Routers);

         Message_Receive : Messages_Mailbox;
         Message_Send : Messages_Client;
         Message_Router : Router_Messages;
      begin
         -- Build routing table to include my neighbors and myself.
         My_Routing_Table (Task_Id).Cost := 0;
         My_Routing_Table (Task_Id).Next_Hop := Task_Id;
         for nbr of Port_List loop
            My_Routing_Table (nbr.Id).Cost := 1;
            My_Routing_Table (nbr.Id).Next_Hop := nbr.Id;
            My_Routing_Table (nbr.Id).Online := True;
            --   Put_Line (Router_Range'Image (Task_Id) & " has" & Router_Range'Image(nbr.Id));
         end loop;

         declare
            task type Dynamic_Sender;
            type Dynamic_Sender_Ptr is access Dynamic_Sender;
            task body Dynamic_Sender is
            begin
               for nbr of Port_List loop
                  Message_Router := (Sender => Task_Id,
                                     Destination => nbr.Id,
                                     Offline_Broadcast => False,
                                     The_Routing_Table => My_Routing_Table);
                  nbr.Link.all.Router_Send (Message_Router);
                  Put_Line (Router_Range'Image (Task_Id) & " send a message to " & Router_Range'Image (nbr.Id));
               end loop;
            end Dynamic_Sender;
            Dynamic_Sender_Instance : constant Dynamic_Sender_Ptr := new Dynamic_Sender;
            pragma Unreferenced (Dynamic_Sender_Instance);
         begin
            receive :
            loop
               select
                  accept Router_Send (Message : Router_Messages) do
                     Put_Line (Router_Range'Image (Task_Id) & " Received a message from " & Router_Range'Image (Message.Sender));
                  end Router_Send;
                  exit receive;
               or
                  delay 0.0005;
                  Put_Line (Router_Range'Image (Task_Id) &  " Entered select else part");
               end select;
            end loop receive;
         end;
         accept Shutdown;
      end;

   exception
      when Exception_Id : others => Show_Exception (Exception_Id);
   end Router_Task;

end Generic_Router;
