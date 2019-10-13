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

         type Sent_Arr_Index_Type is mod 99;
         type Sent_Arr_Type is array (Sent_Arr_Index_Type) of Router_Messages;
         Sent_Arr_Index : Sent_Arr_Index_Type := 0;
         Sent_Arr : Sent_Arr_Type;

         Wait_Queue : Queue_Type;
         Current_Item  : Queue_Element;
         task type Dynamic_Sender;
         type Dynamic_Sender_Ptr is access Dynamic_Sender;
         task body Dynamic_Sender is
         begin
            for nbr of Port_List loop
               Message_Router := (Sender => Task_Id,
                                  Destination => nbr.Id,
                                  Offline_Broadcast => False,
                                  The_Routing_Table => My_Routing_Table);
               Sent_Arr (Sent_Arr_Index) := Message_Router;
               Sent_Arr_Index := Sent_Arr_Index + 1;
               select
                  nbr.Link.all.Router_Send (Message_Router);
                 -- Put_Line (Router_Range'Image (Task_Id) & " sent a message to " & Router_Range'Image (nbr.Id));
               or
                  delay 0.01;
                  Sent_Arr_Index := Sent_Arr_Index - 1; -- Didn't send, remove from sent array
                  Current_Item.Msg := Message_Router;
                  Current_Item.Link := nbr.Link;
                  Current_Item.Id := nbr.Id;
                  Enqueue (Item => Current_Item, Queue => Wait_Queue);
                  Put_Line (Router_Range'Image (Task_Id) & " failed to send msg to" & Router_Range'Image (nbr.Id) & ". Added msg to queue");
               end select;
            end loop;

            while not Is_Empty (Wait_Queue) loop
               Dequeue (Current_Item, Wait_Queue);
               Sent_Arr (Sent_Arr_Index) := Current_Item.Msg;
               Sent_Arr_Index := Sent_Arr_Index + 1;
               select
                  Current_Item.Link.all.Router_Send (Current_Item.Msg);
                  Put_Line (Router_Range'Image (Task_Id) & " sent a message to " & Router_Range'Image (Current_Item.Id) & " thanks to the queue");
               or
                  delay 0.01;
                  Sent_Arr_Index := Sent_Arr_Index - 1;
                  -- No response, send failed, store the msg back to the queue
                  Enqueue (Item => Current_Item, Queue => Wait_Queue);
                  Put_Line (Router_Range'Image (Task_Id) & " failed to send msg to" & Router_Range'Image (Current_Item.Id) & ". Added msg BACK to queue");
               end select;
            end loop;
         end Dynamic_Sender;
         Dynamic_Sender_Instance : constant Dynamic_Sender_Ptr := new Dynamic_Sender;
         pragma Unreferenced (Dynamic_Sender_Instance);
         Found : Boolean := False;
      begin
         -- Build routing table to include my neighbors and myself.
         My_Routing_Table (Task_Id).Cost := 0;
         My_Routing_Table (Task_Id).Next_Hop := Task_Id;
         for nbr of Port_List loop
            My_Routing_Table (nbr.Id).Cost := 1;
            My_Routing_Table (nbr.Id).Next_Hop := nbr.Id;
            My_Routing_Table (nbr.Id).Online := True;
            -- Put_Line (Router_Range'Image (Task_Id) & " has" & Router_Range'Image(nbr.Id));
         end loop;

         receive :
         loop
            select
               accept Router_Send (Message : Router_Messages) do
                  Put_Line (Router_Range'Image (Task_Id) & " Received a message from " & Router_Range'Image (Message.Sender));
                  Message_Router := Message;
               end Router_Send;
               delay 0.0;
               Found := False;
               for msg of Sent_Arr loop
                  if msg = Message_Router then
                     Found := True;
                  end if;
                  exit when Found;
               end loop;

               if not Found then
                  declare
                     Dynamic_Sender_Instance2 : constant Dynamic_Sender_Ptr := new Dynamic_Sender;
                     pragma Unreferenced (Dynamic_Sender_Instance2);
                  begin
                     null;
                  end;
               end if;
              -- exit receive;
            or
               delay 0.0005;
              -- Put_Line (Router_Range'Image (Task_Id) &  " Entered select else part");
            end select;
         end loop receive;
         Put_Line (Router_Range'Image (Task_Id) & " Shutdown!");
         accept Shutdown;
      end;

   exception
      when Exception_Id : others => Show_Exception (Exception_Id);
   end Router_Task;

end Generic_Router;
