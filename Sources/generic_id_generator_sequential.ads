--
--  Uwe R. Zimmer, Australia, September 2011
--

generic
   type Id_Range is range <>;

package Generic_Id_Generator_Sequential is

   function Set_Id return Id_Range;

   Out_Of_Ids : exception;

end Generic_Id_Generator_Sequential;
