--
--  Uwe R. Zimmer, Australia, September 2011
--

package body Generic_Id_Generator_Sequential is

   Current_Id : Integer := Integer (Id_Range'First) - 1;

   function Set_Id return Id_Range is

   begin
      Current_Id := Current_Id + 1;
      if Current_Id <= Integer (Id_Range'Last) then
         return Id_Range (Current_Id);
      else
         raise Out_Of_Ids;
      end if;
   end Set_Id;

end Generic_Id_Generator_Sequential;
