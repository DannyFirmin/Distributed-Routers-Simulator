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
         RouterMsg_Out : Router_Messages;

         type Sent_Arr_Index_Type is mod 99;
         type Sent_Arr_Type is array (Sent_Arr_Index_Type) of Router_Messages;
         Sent_Arr_Index : Sent_Arr_Index_Type := 0;
         Sent_Arr : Sent_Arr_Type;

         Wait_Queue : Queue_Type;
         Current_Item  : Queue_Element;

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

         declare
            task Sender_Task is
               entry Start_Send;
            end Sender_Task;

            task body Sender_Task is
            begin
               loop
                  select
                     accept Start_Send;
                     for nbr of Port_List loop
                        RouterMsg_Out := (Sender => Task_Id,
                                          Destination => nbr.Id,
                                          Offline_Broadcast => False,
                                          The_Routing_Table => My_Routing_Table);
                        Sent_Arr (Sent_Arr_Index) := RouterMsg_Out;
                        Sent_Arr_Index := Sent_Arr_Index + 1;
                        select
                           nbr.Link.all.Router_Send (RouterMsg_Out);
                           -- Put_Line (Router_Range'Image (Task_Id) & " sent a message to " & Router_Range'Image (nbr.Id));
                        or
                           delay 0.01;
                           Sent_Arr_Index := Sent_Arr_Index - 1; -- Didn't send, remove from sent array
                           Current_Item.Msg := RouterMsg_Out;
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
                  or
                     delay 0.0005;
                  end select;
               end loop;
            end Sender_Task;

            task Promoter_Task;
            task body Promoter_Task is
            begin
               select
                  Sender_Task.Start_Send;
               or
                  delay 0.005;
               end select;
            end Promoter_Task;


         begin
            receive :
            loop
               declare
                  RouterMsg_In : Router_Messages;
                  Need_2_Send : Boolean := False;
                  -- Distance Vector Routing Algorithm (Bellman-Ford Algorithm)
                  -- Dx(y) = min {current estimiate, c(x,v) + Dv(y)}
                  Cxv : constant Natural := 1; -- C(x,v) Cost of x to neighbor v
                  Dvy : Natural; -- Distance from neighbor v to destination y
               begin
                  select
                     accept Router_Send (Message : Router_Messages) do
                        Put_Line (Router_Range'Image (Task_Id) & " Received a message from " & Router_Range'Image (Message.Sender));
                        if Task_Id = RouterMsg_In.Sender then
                           Need_2_Send := False;
                        else
                           Need_2_Send := True;
                           RouterMsg_In := Message;
                        end if;
                     end Router_Send;
                     if Need_2_Send then
                        for i in RouterMsg_In.The_Routing_Table'Range loop
                           if RouterMsg_In.The_Routing_Table (i).Cost /= Natural'Invalid_Value then
                              Dvy := RouterMsg_In.The_Routing_Table (i).Cost;
                              if My_Routing_Table (i).Cost = Natural'Invalid_Value or else My_Routing_Table (i).Cost > Cxv + Dvy then
                                 My_Routing_Table (i).Cost := Cxv + Dvy;
                                 My_Routing_Table (i).Next_Hop := RouterMsg_In.Sender;
                                 Put_Line ("new RT");
                                 Need_2_Send := True;
                              end if;
                           elsif RouterMsg_In.The_Routing_Table (i).Cost = Natural'Invalid_Value and then My_Routing_Table (i).Cost /= Natural'Invalid_Value then
                              Need_2_Send := True;
                              Put_Line ("nbr is fool");
                           else
                              Need_2_Send := False;
                           end if;
                        end loop;

                        for msg of Sent_Arr loop
                           if msg = RouterMsg_In then
                              Need_2_Send := False; -- Found same message sent before
                              Put_Line ("Sent before");
                           end if;
                           exit when not Need_2_Send;
                        end loop;

                        while Need_2_Send loop
                           select
                              Sender_Task.Start_Send;
                              Need_2_Send := False;
                           or
                              delay 0.005;
                           end select;
                        end loop;
                     end if;
                     -- exit receive;
                  or
                     delay 0.0005;
                     -- Put_Line (Router_Range'Image (Task_Id) &  " Entered select else part");
                  end select;
               end;

            end loop receive;
            Put_Line (Router_Range'Image (Task_Id) & " Shutdown!");
            accept Shutdown;
         end;
      end;

   exception
      when Exception_Id : others => Show_Exception (Exception_Id);
   end Router_Task;

end Generic_Router;
