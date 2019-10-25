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
      Send_Queue : Protected_Queue;

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

         task type Send_Worker is
            entry Stop_Send;
         end Send_Worker;

         type Send_Worker_Ptr is access Send_Worker;
         task body Send_Worker is
            Current_Item  : Router_Messages;
         begin
            Sender_Loop :
            loop
               select
                  accept Stop_Send;
                  exit Sender_Loop;
                  -- terminate;
               or
                  delay 0.01;
                  if not Send_Queue.Is_Empty then
                     Send_Queue.Dequeue (Current_Item);
                     for nbr of Port_List loop
                        if nbr.Id = My_Routing_Table (Current_Item.Destination).Next_Hop and then My_Routing_Table (Current_Item.Destination).Online then
                           nbr.Link.all.Router_Send (Current_Item);
                           -- exit;
                        end if;
                     end loop;
                  end if;
               end select;
            end loop Sender_Loop;
         exception
            when Tasking_Error =>
               begin
                  Put_Line (Router_Range'Image (Task_Id) & " found " & Router_Range'Image (My_Routing_Table (Current_Item.Destination).Next_Hop) & " dead");
               end;
         end Send_Worker;

         Send_Worker_Instance : constant Send_Worker_Ptr := new Send_Worker;
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
            RouterMsg_Out : Router_Messages;
            Found : Boolean := False;
         begin
            for nbr of Port_List loop
               RouterMsg_Out := (Sender => Task_Id,
                                 Destination => nbr.Id,
                                 Offline_Broadcast => False,
                                 The_Routing_Table => My_Routing_Table,
                                 Core_Msg => Message_Strings.To_Bounded_String (""),
                                 Hop_Counter => 0);
               Sent_RouterMsg.Find (Item => RouterMsg_Out, Result => Found);
               if not Found then
                  Sent_RouterMsg.Put (Item => RouterMsg_Out);
                  Send_Queue.Enqueue (RouterMsg_Out);
               end if;
            end loop;
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
                     --  Put_Line (Router_Range'Image (Task_Id) & " Received a message from " & Router_Range'Image (Message.Sender));
                     if Task_Id /= Message.Sender then
                        for i in Message.The_Routing_Table'Range loop
                           if Message.The_Routing_Table (i).Cost /= Natural'Invalid_Value then
                              Dvy := Message.The_Routing_Table (i).Cost;
                              if My_Routing_Table (i).Cost = Natural'Invalid_Value or else My_Routing_Table (i).Cost > Cxv + Dvy then
                                 My_Routing_Table (i).Cost := Cxv + Dvy;
                                 My_Routing_Table (i).Next_Hop := Message.Sender;
                                 --   Put_Line ("new RT");
                                 Need_2_Send := True;
                              end if;
                           elsif Message.The_Routing_Table (i).Cost = Natural'Invalid_Value and then My_Routing_Table (i).Cost /= Natural'Invalid_Value then
                              Need_2_Send := True;
                              --    Put_Line ("nbr is fool");
                           else
                              Need_2_Send := False;
                           end if;
                        end loop;

                        if Need_2_Send then
                           declare
                              RouterMsg_Out : Router_Messages;
                              Found : Boolean := False;
                           begin
                              for nbr of Port_List loop
                                 RouterMsg_Out := (Sender => Task_Id,
                                                   Destination => nbr.Id,
                                                   Offline_Broadcast => False,
                                                   The_Routing_Table => My_Routing_Table,
                                                   Core_Msg => Message_Strings.To_Bounded_String (""),
                                                   Hop_Counter => 0);
                                 Sent_RouterMsg.Find (Item => RouterMsg_Out, Result => Found);
                                 if not Found then
                                    Sent_RouterMsg.Put (Item => RouterMsg_Out);
                                    Send_Queue.Enqueue (RouterMsg_Out);
                                 end if;
                              end loop;
                           end;
                        else
                           null;
                           --   Put_Line ("No need to send");
                        end if;

                        if Message_Strings.Length (Message.Core_Msg) /= 0 and then Message.Destination = Task_Id then
                           -- Put_Line ("This msg is for me! YEAH!");
                           Message_Receive := (Sender => Message.Sender,
                                               The_Message => Message.Core_Msg,
                                               Hop_Counter => Message.Hop_Counter);
                        elsif Message_Strings.Length (Message.Core_Msg) /= 0 and then Message.Destination /= Task_Id then
                           -- Put_Line ("This msg is for " & Router_Range'Image (Message.Destination));
                           declare
                              RouterMsg_Out : Router_Messages;
                           begin
                              RouterMsg_Out := (Sender => Message.Sender,
                                                Destination => Message.Destination,
                                                Offline_Broadcast => False,
                                                The_Routing_Table => My_Routing_Table,
                                                Core_Msg => Message.Core_Msg,
                                                Hop_Counter => Message.Hop_Counter + 1);
                              Send_Queue.Enqueue (RouterMsg_Out);
                           end;
                        end if;

                     end if;
                  end Router_Send;
               or
                  accept Send_Message (Message : in Messages_Client) do
                     if My_Routing_Table (Message.Destination).Next_Hop'Valid then
                        declare
                           RouterMsg_Out : Router_Messages;
                        begin
                           RouterMsg_Out := (Sender => Task_Id,
                                             Destination => Message.Destination,
                                             Offline_Broadcast => False,
                                             The_Routing_Table => My_Routing_Table,
                                             Core_Msg => Message.The_Message,
                                             Hop_Counter => 1);
                           Send_Queue.Enqueue (RouterMsg_Out);
                        end;
                     end if;

                  end Send_Message;

                  -- exit receive;
               or
                  accept Receive_Message (Message : out Messages_Mailbox) do
                     Message := Message_Receive;
                  end Receive_Message;
               or
                  accept Alive_Test  do
                     Put_Line ("I am alive");
                  end Alive_Test;
               or
                  accept Shutdown;
                  Send_Worker_Instance.all.Stop_Send;
                  Put_Line (Router_Range'Image (Task_Id) & " Shutdown!");
                  --                    if Task_Id = 5 then
                  --                       for i in My_Routing_Table'Range loop
                  --                          Put_Line ("Dest: " & Router_Range'Image(i));
                  --                          Put_Line ("Cost: " & Natural'Image(My_Routing_Table(i).Cost));
                  --                          Put_Line ("Next_Hop: " & Router_Range'Image(My_Routing_Table(i).Next_Hop));
                  --                      end loop;
                  --                    end if;
                  exit receive;
               end select;
            end;

         end loop receive;
      end;

   exception
      when Exception_Id : others => Show_Exception (Exception_Id);
   end Router_Task;
end Generic_Router;
