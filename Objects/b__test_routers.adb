pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__test_routers.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__test_routers.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is

   E077 : Short_Integer; pragma Import (Ada, E077, "system__os_lib_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__soft_links_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exception_table_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "ada__io_exceptions_E");
   E057 : Short_Integer; pragma Import (Ada, E057, "ada__strings_E");
   E040 : Short_Integer; pragma Import (Ada, E040, "ada__containers_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "system__exceptions_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__secondary_stack_E");
   E059 : Short_Integer; pragma Import (Ada, E059, "ada__strings__maps_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "ada__strings__maps__constants_E");
   E045 : Short_Integer; pragma Import (Ada, E045, "interfaces__c_E");
   E083 : Short_Integer; pragma Import (Ada, E083, "system__object_reader_E");
   E052 : Short_Integer; pragma Import (Ada, E052, "system__dwarf_lines_E");
   E039 : Short_Integer; pragma Import (Ada, E039, "system__traceback__symbolic_E");
   E159 : Short_Integer; pragma Import (Ada, E159, "ada__numerics_E");
   E111 : Short_Integer; pragma Import (Ada, E111, "ada__tags_E");
   E109 : Short_Integer; pragma Import (Ada, E109, "ada__streams_E");
   E123 : Short_Integer; pragma Import (Ada, E123, "system__file_control_block_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "system__finalization_root_E");
   E120 : Short_Integer; pragma Import (Ada, E120, "ada__finalization_E");
   E119 : Short_Integer; pragma Import (Ada, E119, "system__file_io_E");
   E234 : Short_Integer; pragma Import (Ada, E234, "system__storage_pools_E");
   E230 : Short_Integer; pragma Import (Ada, E230, "system__finalization_masters_E");
   E228 : Short_Integer; pragma Import (Ada, E228, "system__storage_pools__subpools_E");
   E253 : Short_Integer; pragma Import (Ada, E253, "ada__strings__unbounded_E");
   E176 : Short_Integer; pragma Import (Ada, E176, "system__task_info_E");
   E170 : Short_Integer; pragma Import (Ada, E170, "system__task_primitives__operations_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "ada__calendar_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__calendar__delays_E");
   E161 : Short_Integer; pragma Import (Ada, E161, "ada__real_time_E");
   E107 : Short_Integer; pragma Import (Ada, E107, "ada__text_io_E");
   E257 : Short_Integer; pragma Import (Ada, E257, "gnat__directory_operations_E");
   E262 : Short_Integer; pragma Import (Ada, E262, "system__pool_global_E");
   E276 : Short_Integer; pragma Import (Ada, E276, "system__random_seed_E");
   E269 : Short_Integer; pragma Import (Ada, E269, "system__regexp_E");
   E251 : Short_Integer; pragma Import (Ada, E251, "gnat__command_line_E");
   E202 : Short_Integer; pragma Import (Ada, E202, "system__tasking__initialization_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "system__tasking__protected_objects_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "system__tasking__protected_objects__entries_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "system__tasking__queuing_E");
   E286 : Short_Integer; pragma Import (Ada, E286, "system__tasking__stages_E");
   E196 : Short_Integer; pragma Import (Ada, E196, "exceptions_E");
   E242 : Short_Integer; pragma Import (Ada, E242, "generic_router_links_E");
   E244 : Short_Integer; pragma Import (Ada, E244, "id_dispenser_E");
   E246 : Short_Integer; pragma Import (Ada, E246, "queue_pack_protected_generic_E");
   E224 : Short_Integer; pragma Import (Ada, E224, "topologies_E");
   E222 : Short_Integer; pragma Import (Ada, E222, "routers_configuration_structures_E");
   E240 : Short_Integer; pragma Import (Ada, E240, "generic_router_E");
   E248 : Short_Integer; pragma Import (Ada, E248, "generic_routers_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "topologies__finalize_body");
      begin
         E224 := E224 - 1;
         F1;
      end;
      E214 := E214 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F2;
      end;
      E269 := E269 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "system__regexp__finalize_spec");
      begin
         F3;
      end;
      E262 := E262 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "system__pool_global__finalize_spec");
      begin
         F4;
      end;
      E107 := E107 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "ada__text_io__finalize_spec");
      begin
         F5;
      end;
      E253 := E253 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "ada__strings__unbounded__finalize_spec");
      begin
         F6;
      end;
      E228 := E228 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "system__storage_pools__subpools__finalize_spec");
      begin
         F7;
      end;
      E230 := E230 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "system__finalization_masters__finalize_spec");
      begin
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__file_io__finalize_body");
      begin
         E119 := E119 - 1;
         F9;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, True, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (True, True, False, True, True, False, False, True, 
           False, False, True, True, True, True, False, False, 
           True, True, False, True, True, False, True, True, 
           False, True, True, True, True, False, True, False, 
           False, True, True, False, True, True, False, True, 
           False, True, True, False, True, False, True, True, 
           False, True, True, False, False, True, False, False, 
           True, False, True, False, True, True, True, False, 
           False, True, False, True, True, True, False, True, 
           True, False, True, True, True, True, False, True, 
           True, False, False, False, True, True, True, True, 
           False, True, False),
         Count => (0, 0, 0, 3, 2, 7, 2, 0, 3, 0),
         Unknown => (False, False, False, False, False, False, True, False, True, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      Runtime_Initialize (1);

      System.Scalar_Values.Initialize ('I', 'N');

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E025 := E025 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E072 := E072 + 1;
      Ada.Strings'Elab_Spec;
      E057 := E057 + 1;
      Ada.Containers'Elab_Spec;
      E040 := E040 + 1;
      System.Exceptions'Elab_Spec;
      E027 := E027 + 1;
      System.Soft_Links'Elab_Body;
      E015 := E015 + 1;
      System.Os_Lib'Elab_Body;
      E077 := E077 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E063 := E063 + 1;
      Interfaces.C'Elab_Spec;
      System.Secondary_Stack'Elab_Body;
      E019 := E019 + 1;
      E059 := E059 + 1;
      E045 := E045 + 1;
      System.Object_Reader'Elab_Spec;
      System.Dwarf_Lines'Elab_Spec;
      E052 := E052 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E039 := E039 + 1;
      E083 := E083 + 1;
      Ada.Numerics'Elab_Spec;
      E159 := E159 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E111 := E111 + 1;
      Ada.Streams'Elab_Spec;
      E109 := E109 + 1;
      System.File_Control_Block'Elab_Spec;
      E123 := E123 + 1;
      System.Finalization_Root'Elab_Spec;
      E122 := E122 + 1;
      Ada.Finalization'Elab_Spec;
      E120 := E120 + 1;
      System.File_Io'Elab_Body;
      E119 := E119 + 1;
      System.Storage_Pools'Elab_Spec;
      E234 := E234 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E230 := E230 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E228 := E228 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E253 := E253 + 1;
      System.Task_Info'Elab_Spec;
      E176 := E176 + 1;
      System.Task_Primitives.Operations'Elab_Body;
      E170 := E170 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E008 := E008 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E006 := E006 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E161 := E161 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E107 := E107 + 1;
      Gnat.Directory_Operations'Elab_Spec;
      Gnat.Directory_Operations'Elab_Body;
      E257 := E257 + 1;
      System.Pool_Global'Elab_Spec;
      E262 := E262 + 1;
      System.Random_Seed'Elab_Body;
      E276 := E276 + 1;
      System.Regexp'Elab_Spec;
      E269 := E269 + 1;
      Gnat.Command_Line'Elab_Spec;
      Gnat.Command_Line'Elab_Body;
      E251 := E251 + 1;
      System.Tasking.Initialization'Elab_Body;
      E202 := E202 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E212 := E212 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E214 := E214 + 1;
      System.Tasking.Queuing'Elab_Body;
      E210 := E210 + 1;
      System.Tasking.Stages'Elab_Body;
      E286 := E286 + 1;
      E196 := E196 + 1;
      E242 := E242 + 1;
      E244 := E244 + 1;
      E246 := E246 + 1;
      Topologies'Elab_Spec;
      Topologies'Elab_Body;
      E224 := E224 + 1;
      E222 := E222 + 1;
      E240 := E240 + 1;
      E248 := E248 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_test_routers");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /students/u6611178/git/Distributed-Routers-Simulator/Objects/exceptions.o
   --   /students/u6611178/git/Distributed-Routers-Simulator/Objects/generic_router_links.o
   --   /students/u6611178/git/Distributed-Routers-Simulator/Objects/id_dispenser.o
   --   /students/u6611178/git/Distributed-Routers-Simulator/Objects/queue_pack_protected_generic.o
   --   /students/u6611178/git/Distributed-Routers-Simulator/Objects/topologies.o
   --   /students/u6611178/git/Distributed-Routers-Simulator/Objects/routers_configuration_structures.o
   --   /students/u6611178/git/Distributed-Routers-Simulator/Objects/generic_routers_configuration.o
   --   /students/u6611178/git/Distributed-Routers-Simulator/Objects/generic_message_structures.o
   --   /students/u6611178/git/Distributed-Routers-Simulator/Objects/generic_router.o
   --   /students/u6611178/git/Distributed-Routers-Simulator/Objects/generic_routers.o
   --   /students/u6611178/git/Distributed-Routers-Simulator/Objects/test_routers.o
   --   -L/students/u6611178/git/Distributed-Routers-Simulator/Objects/
   --   -L/students/u6611178/git/Distributed-Routers-Simulator/Objects/
   --   -L/lab/usr/local/gnat/lib/gcc/x86_64-pc-linux-gnu/6.3.1/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
   --   -lpthread
   --   -lrt
   --   -ldl
--  END Object file/option list   

end ada_main;
