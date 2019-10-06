--
--  Uwe R. Zimmer, Australia, September 2011
--

with Exceptions;                      use Exceptions;

package body Generic_Router is

   task body Router_Task is

   begin

      --  Replace the following loop with the code of your router

      loop
         delay 1.0;
      end loop;

   exception
      when Exception_Id : others => Show_Exception (Exception_Id);
   end Router_Task;

end Generic_Router;
