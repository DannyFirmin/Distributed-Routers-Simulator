--
--  Framework: Uwe R. Zimmer, Australia, 2019
--
--  Student: Danny Feng (u6611178), Australia, 2019
--

with Exceptions; use Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
package body Generic_Router is

   task body Router_Task is
      Offline_Cost : constant Natural := 9999;
      Connected_Routers : Ids_To_Links;
      My_Routing_Table : Routing_Table;

   begin
      accept Configure (Links : Ids_To_Links) do
         Connected_Routers := Links;
      end Configure;

      declare
         Port_List : constant Connected_Router_Ports := To_Router_Ports (Task_Id, Connected_Routers);

         Message_Receive : Messages_Mailbox;

         type Sent_Arr_Index_Type is mod 99;
         type Sent_Arr_Type is array (Sent_Arr_Index_Type) of Router_Messages;

         -- Protected array shared between sender tasks. To remember what I sent
         -- So that sender can filter junk msg to make system fast and efficient
         protected Sent_RouterMsg is
            entry Put (Item : Router_Messages);
            entry Remove_Last;
            entry Find (Item : Router_Messages; Result : out Boolean);
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

            entry Find (Item : Router_Messages; Result : out Boolean) when Done is
            begin
               Done := False;
               for e of Sent_Arr loop
                  if e.Sender = Item.Sender and then e.Destination = Item.Destination and then e.Offline_Broadcast = Item.Offline_Broadcast and then e.The_Routing_Table = Item.The_Routing_Table then
                     Result := True;
                     exit;
                  else
                     Result := False;
                  end if;
               end loop;
               Done := True;
            end Find;
         end Sent_RouterMsg;

         task type Dynamic_Send2Nbr;
         type Dynamic_Send2Nbr_Ptr is access Dynamic_Send2Nbr;
         task body Dynamic_Send2Nbr is
            RouterMsg_Out : Router_Messages;
            -- Each sender has a local queue to requeue the msg if no response for now
            Wait_Queue : Queue_Type;
            Current_Item  : Queue_Element;
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
                  select
                     nbr.Link.all.Router_Send (RouterMsg_Out);
                  or
                     delay 0.01;
                     Sent_RouterMsg.Remove_Last; -- Didn't send, remove from sent array
                     Current_Item.Msg := RouterMsg_Out;
                     Current_Item.Link := nbr.Link;
                     Current_Item.Id := nbr.Id;
                     Enqueue (Item => Current_Item, Queue => Wait_Queue);
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
                  or
                     delay 0.01;
                     Sent_RouterMsg.Remove_Last;
                     -- No response, send failed, store the msg back to the queue
                     Enqueue (Item => Current_Item, Queue => Wait_Queue);
                  end select;
               end if;
            end loop;
         end Dynamic_Send2Nbr;

         task type Dynamic_Send2Next is
            entry Pass_Task_Parameter (Msg : Router_Messages);
         end Dynamic_Send2Next;

         type Dynamic_Send2Next_Ptr is access Dynamic_Send2Next;

         task body Dynamic_Send2Next is
            My_Msg : Router_Messages;
         begin
            accept Pass_Task_Parameter (Msg : in Router_Messages) do
               My_Msg := Msg;
               for nbr of Port_List loop
                  if nbr.Id = My_Routing_Table (My_Msg.Destination).Next_Hop then
                     nbr.Link.all.Router_Send (My_Msg);
                  end if;
               end loop;
            end Pass_Task_Parameter;
         exception
            when Tasking_Error =>
               begin
                  -- Not sure why after the task handled its neighbour's exception, it also dies. This will cause a chain effect. RIP.
                  Put_Line (Router_Range'Image (Task_Id) & " found " & Router_Range'Image (My_Routing_Table (My_Msg.Destination).Next_Hop) & " dead");
                  My_Routing_Table (My_Routing_Table (My_Msg.Destination).Next_Hop).Online := False;
                  My_Routing_Table (My_Routing_Table (My_Msg.Destination).Next_Hop).Cost := Offline_Cost;
                  My_Routing_Table (My_Routing_Table (My_Msg.Destination).Next_Hop).Next_Hop := Router_Range'Invalid_Value;
                  My_Msg := (Sender => Task_Id,
                             Destination => 1,
                             Offline_Broadcast => True,
                             The_Routing_Table => My_Routing_Table,
                             Core_Msg => Message_Strings.To_Bounded_String (""),
                             Hop_Counter => 0);
                  Router_Send (My_Msg);
               end;
         end Dynamic_Send2Next;

      begin
         -- Build routing table to include my neighbors and myself.
         My_Routing_Table (Task_Id).Cost := 0;
         My_Routing_Table (Task_Id).Next_Hop := Task_Id;
         for nbr of Port_List loop
            My_Routing_Table (nbr.Id).Cost := 1;
            My_Routing_Table (nbr.Id).Next_Hop := nbr.Id;
            My_Routing_Table (nbr.Id).Online := True;
         end loop;
         declare
            Dynamic_Send2Nbr_Instance : constant Dynamic_Send2Nbr_Ptr := new Dynamic_Send2Nbr;
            pragma Unreferenced (Dynamic_Send2Nbr_Instance);
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
                     if Task_Id /= Message.Sender then
                        for i in Message.The_Routing_Table'Range loop
                           if Message.The_Routing_Table (i).Cost /= Natural'Invalid_Value and then Message.The_Routing_Table (i).Online then
                              Dvy := Message.The_Routing_Table (i).Cost;
                              if My_Routing_Table (i).Cost = Natural'Invalid_Value or else My_Routing_Table (i).Cost > Cxv + Dvy then
                                 My_Routing_Table (i).Cost := Cxv + Dvy;
                                 My_Routing_Table (i).Next_Hop := Message.Sender;
                                 Need_2_Send := True;
                              end if;
                           elsif Message.The_Routing_Table (i).Cost = Natural'Invalid_Value and then My_Routing_Table (i).Cost /= Natural'Invalid_Value and then Message.The_Routing_Table (i).Online then
                              Need_2_Send := True;
                           elsif not Message.The_Routing_Table (i).Online then
                              Need_2_Send := True;
                              My_Routing_Table (i).Online := False;
                              My_Routing_Table (i).Cost := Offline_Cost;
                           else
                              Need_2_Send := False;
                           end if;
                        end loop;

                        if Need_2_Send then
                           declare
                              Dynamic_Send2Nbr_Instance : constant Dynamic_Send2Nbr_Ptr := new Dynamic_Send2Nbr;
                              pragma Unreferenced (Dynamic_Send2Nbr_Instance);
                           begin
                              null;
                           end;
                        end if;

                        if Message_Strings.Length (Message.Core_Msg) /= 0 and then Message.Destination = Task_Id then
                           -- This msg is for me
                           Message_Receive := (Sender => Message.Sender,
                                               The_Message => Message.Core_Msg,
                                               Hop_Counter => Message.Hop_Counter);
                        elsif Message_Strings.Length (Message.Core_Msg) /= 0 and then Message.Destination /= Task_Id then
                           -- This msg is for others
                           declare
                              RouterMsg_Out : Router_Messages;
                              Dynamic_Send2Next_Instance : constant Dynamic_Send2Next_Ptr := new Dynamic_Send2Next;
                           begin
                              RouterMsg_Out := (Sender => Message.Sender,
                                                Destination => Message.Destination,
                                                Offline_Broadcast => False,
                                                The_Routing_Table => My_Routing_Table,
                                                Core_Msg => Message.Core_Msg,
                                                Hop_Counter => Message.Hop_Counter + 1);
                              Dynamic_Send2Next_Instance.all.Pass_Task_Parameter (RouterMsg_Out);
                           end;
                        end if;

                     end if;
                  end Router_Send;
               or
                  accept Send_Message (Message : in Messages_Client) do
                     if My_Routing_Table (Message.Destination).Next_Hop'Valid then
                        declare
                           RouterMsg_Out : Router_Messages;
                           Dynamic_Send2Next_Instance : constant Dynamic_Send2Next_Ptr := new Dynamic_Send2Next;
                        begin
                           RouterMsg_Out := (Sender => Task_Id,
                                             Destination => Message.Destination,
                                             Offline_Broadcast => False,
                                             The_Routing_Table => My_Routing_Table,
                                             Core_Msg => Message.The_Message,
                                             Hop_Counter => 1);
                           Dynamic_Send2Next_Instance.all.Pass_Task_Parameter (RouterMsg_Out);
                        end;
                     end if;
                  end Send_Message;
               or
                  accept Receive_Message (Message : out Messages_Mailbox) do
                     Message := Message_Receive;
                  end Receive_Message;
               or
                  accept Shutdown;
                  exit receive;
               end select;
            end;
         end loop receive;
      end;

   exception
      when Exception_Id : others => Show_Exception (Exception_Id);
   end Router_Task;
end Generic_Router;
