--
-- Uwe R. Zimmer, Australia 2015
--

with Ada.Real_Time;                 use Ada.Real_Time;
with ANU_Base_Board.LED_Interface;  use ANU_Base_Board.LED_Interface;
with Discovery_Board.LED_Interface; use Discovery_Board.LED_Interface;
with Generator_Controllers;         pragma Unreferenced (Generator_Controllers);
with Last_Chance_Handler;           pragma Unreferenced (Last_Chance_Handler);

with System; use System;
with Discovery_Board; use Discovery_Board;

procedure Generator with Priority => Priority'First is

   System_Startup : constant Time      := Clock;
   Reset_Flicker  : constant Time_Span := Milliseconds (20);

   package ABBL renames ANU_Base_Board.LED_Interface;
   package DBL  renames Discovery_Board.LED_Interface;

begin

   -- All tasks are running at this point.

   -- Briefly flickering all lights to acknowledge a successful start.

   DBL.All_On;
   ABBL.All_On;
   delay until System_Startup + Reset_Flicker;
   DBL.All_Off;
   ABBL.All_Off;
   Toggle (Red);

   loop
      null; -- Main task (at lowest priority) needs to be prevented from exiting
   end loop;

exception
      when others => Discovery_Board.LED_Interface.On (Blue);
end Generator;
