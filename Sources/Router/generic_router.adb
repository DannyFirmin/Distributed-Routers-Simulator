--
--  Framework: Uwe R. Zimmer, Australia, 2019
--
--  Student: Danny Feng (u6611178), Australia, 2019
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

         type Sent_Arr_Index_Type is mod 99;
         type Sent_Arr_Type is array (Sent_Arr_Index_Type) of Router_Messages;

         protected Sent_RouterMsg is
            entry Put (Item : Router_Messages);
            entry Remove_Last;
            entry Find (Item : out Router_Messages; Result : out Boolean);
         private
            Index : Sent_Arr_Index_Type := 0;
            Sent_Arr : Sent_Arr_Type;
            Done : Boolean := True;
         end Sent_RouterMsg;

         protected body Sent_RouterMsg is
            entry Put (Item : Router_Messages) when Done is
            begin
               Done := False;
               Sent_Arr (Index) := Item;
               Index := Index + 1;
               Done := True;
            end Put;

            entry Remove_Last when Done is
            begin
               Done := False;
               Index := Index - 1;
               Done := True;
            end Remove_Last;

            entry Find (Item : out Router_Messages; Result : out Boolean) when Done is
            begin
               Done := False;
               for e of Sent_Arr loop
                  if e.Sender = Item.Sender and then e.Destination = Item.Destination and then e.Offline_Broadcast = Item.Offline_Broadcast and then e.The_Routing_Table = Item.The_Routing_Table then
                     Item := e;
                     Result := True;
                     exit;
                  else
                     --  Item := Router_Messages'Invalid_Value;
                     Result := False;
                  end if;
               end loop;
               Done := True;
            end Find;
         end Sent_RouterMsg;

         task type Dynamic_Sender;
         type Dynamic_Sender_Ptr is access Dynamic_Sender;
         task body Dynamic_Sender is
         begin
            declare
               RouterMsg_Out : Router_Messages;
               Wait_Queue : Queue_Type;
               Current_Item  : Queue_Element;
               Found : Boolean := False;
            begin
               for nbr of Port_List loop
                  RouterMsg_Out := (Sender => Task_Id,
                                    Destination => nbr.Id,
                                    Offline_Broadcast => False,
                                    The_Routing_Table => My_Routing_Table);
                  Sent_RouterMsg.Find (Item => RouterMsg_Out, Result => Found);
                  if not Found then
                     Sent_RouterMsg.Put (Item => RouterMsg_Out);
                     select
                        nbr.Link.all.Router_Send (RouterMsg_Out);
                        -- Put_Line (Router_Range'Image (Task_Id) & " sent a message to " & Router_Range'Image (nbr.Id));
                     or
                        delay 0.01;
                        Sent_RouterMsg.Remove_Last; -- Didn't send, remove from sent array
                        Current_Item.Msg := RouterMsg_Out;
                        Current_Item.Link := nbr.Link;
                        Current_Item.Id := nbr.Id;
                        Enqueue (Item => Current_Item, Queue => Wait_Queue);
                        Put_Line (Router_Range'Image (Task_Id) & " failed to send msg to" & Router_Range'Image (nbr.Id) & ". Added msg to queue");
                     end select;
                  end if;
               end loop;

               while not Is_Empty (Wait_Queue) loop
                  Dequeue (Current_Item, Wait_Queue);
                  Sent_RouterMsg.Find (Item => Current_Item.Msg, Result => Found);
                  if not Found then
                     Sent_RouterMsg.Put (Item => Current_Item.Msg);
                     select
                        Current_Item.Link.all.Router_Send (Current_Item.Msg);
                        Put_Line (Router_Range'Image (Task_Id) & " sent a message to " & Router_Range'Image (Current_Item.Id) & " thanks to the queue");
                     or
                        delay 0.01;
                        Sent_RouterMsg.Remove_Last;
                        -- No response, send failed, store the msg back to the queue
                        Enqueue (Item => Current_Item, Queue => Wait_Queue);
                        Put_Line (Router_Range'Image (Task_Id) & " failed to send msg to" & Router_Range'Image (Current_Item.Id) & ". Added msg BACK to queue");
                     end select;

                  end if;
               end loop;

            end;
         end Dynamic_Sender;
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
            Dynamic_Sender_Instance : constant Dynamic_Sender_Ptr := new Dynamic_Sender;
            pragma Unreferenced (Dynamic_Sender_Instance);
         begin
            null;
         end;

         receive :
         loop
            declare
               Need_2_Send : Boolean := False;
               -- Distance Vector Routing Algorithm (Bellman-Ford Algorithm)
               -- Dx(y) = min {current estimiate, c(x,v) + Dv(y)}
               Cxv : constant Natural := 1; -- C(x,v) Cost of x to neighbor v
               Dvy : Natural; -- Distance from neighbor v to destination y
            begin
               select
                  accept Router_Send (Message : Router_Messages) do
                     Put_Line (Router_Range'Image (Task_Id) & " Received a message from " & Router_Range'Image (Message.Sender));
                     if Task_Id /= Message.Sender then

                        for i in Message.The_Routing_Table'Range loop
                           if Message.The_Routing_Table (i).Cost /= Natural'Invalid_Value then
                              Dvy := Message.The_Routing_Table (i).Cost;
                              if My_Routing_Table (i).Cost = Natural'Invalid_Value or else My_Routing_Table (i).Cost > Cxv + Dvy then
                                 My_Routing_Table (i).Cost := Cxv + Dvy;
                                 My_Routing_Table (i).Next_Hop := Message.Sender;
                                 Put_Line ("new RT");
                                 Need_2_Send := True;
                              end if;
                           elsif Message.The_Routing_Table (i).Cost = Natural'Invalid_Value and then My_Routing_Table (i).Cost /= Natural'Invalid_Value then
                              Need_2_Send := True;
                              Put_Line ("nbr is fool");
                           else
                              Need_2_Send := False;
                           end if;
                        end loop;

                        if Need_2_Send then
                           declare
                              Dynamic_Sender_Instance2 : constant Dynamic_Sender_Ptr := new Dynamic_Sender;
                              pragma Unreferenced (Dynamic_Sender_Instance2);
                           begin
                              null;
                           end;
                        else
                           Put_Line ("No need to send");
                        end if;

                     end if;
                  end Router_Send;

                  -- exit receive;
               or
                  accept Shutdown;
                  Put_Line (Router_Range'Image (Task_Id) & " Shutdown!");
                  -- Put_Line (Router_Range'Image (Task_Id) &  " Entered select else part");
               end select;
            end;

         end loop receive;
      end;

   exception
      when Exception_Id : others => Show_Exception (Exception_Id);
   end Router_Task;

end Generic_Router;
