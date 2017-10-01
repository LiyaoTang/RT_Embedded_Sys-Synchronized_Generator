--
-- Uwe R. Zimmer, Australia 2015
--

with System;                              use System;
with Ada.Real_Time;                       use Ada.Real_Time;

with ANU_Base_Board;                      use ANU_Base_Board;
with ANU_Base_Board.Com_Interface;        use ANU_Base_Board.Com_Interface;
with ANU_Base_Board.LED_Interface;        use ANU_Base_Board.LED_Interface;
with Discovery_Board;                     use Discovery_Board;
with Discovery_Board.Button;              use Discovery_Board.Button;
with Discovery_Board.LED_Interface;       use Discovery_Board.LED_Interface;

with STM32F4;                             use type STM32F4.Bit, STM32F4.Bits_32;
with STM32F4.Interrupts_and_Events;       use STM32F4.Interrupts_and_Events;
with STM32F4.General_purpose_IOs;         use STM32F4.General_purpose_IOs;
with STM32F4.Random_number_generator.Ops; use STM32F4.Random_number_generator.Ops;
with STM32F4.Reset_and_clock_control.Ops; use STM32F4.Reset_and_clock_control.Ops;

package body Generator_Controllers is

   System_Start : constant Time := Clock;

   task Controller with
     Storage_Size => 4 * 1024,
     Priority     => Default_Priority;

   task body Controller is

      Period       : constant Time_Span   := Milliseconds (500);
      My_Port      : constant Com_Ports   := Com_Ports (1);
      Release_Time :          Time        := System_Start;

   begin
      delay until System_Start + Milliseconds (40);

      On ((My_Port, L));

      loop
         Toggle (Red); -- board LEDs
         Toggle (Orange);

         Toggle (My_Port); -- output data

         Release_Time := Release_Time + Period;
         delay until Release_Time;

      end loop;

   exception
         when others => Discovery_Board.LED_Interface.On (Blue);
   end Controller;

   task Follower with
     Storage_Size => 4 * 1024,
     Priority     => Default_Priority;

   task body Follower is

      Period       : constant Time_Span := Milliseconds (500) + Microseconds (150);
      My_Port      : constant Com_Ports := Com_Ports (2);
      In_Data      : STM32F4.Bit        := 0;
      Is_Mine      : Boolean            := False;
      Release_Time : Time               := System_Start;

   begin
      delay until System_Start + Milliseconds (30);

      loop
         declare
            Follower_Enabled : constant Boolean := Button.Current_Blue_Button_State = Off;
         begin
            if Follower_Enabled then

               -- read in data
--                 Receivers (My_Port).Read_Data (In_Data);
               while not Is_Mine loop
                  Port_Inspector.Read_Data (My_Port, In_Data, Is_Mine);
               end loop;
               Is_Mine := False;

               -- board LEDs
               Toggle (Green);
               Toggle (Orange);
            end if;

         end;
      end loop;

   exception
         when others => Discovery_Board.LED_Interface.On (Blue);
   end Follower;

end Generator_Controllers;
