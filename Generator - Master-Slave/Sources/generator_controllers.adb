--
-- Uwe R. Zimmer, Australia 2015
--

with System;                              use System;
with Ada.Real_Time;                       use Ada.Real_Time;
with Ada.Synchronous_Task_Control;        use Ada.Synchronous_Task_Control;

with ANU_Base_Board;                      use ANU_Base_Board;
with ANU_Base_Board.Com_Interface;        use ANU_Base_Board.Com_Interface;
with ANU_Base_Board.LED_Interface;        use ANU_Base_Board.LED_Interface;

with Discovery_Board;                     use Discovery_Board;
with Discovery_Board.LED_Interface;       use Discovery_Board.LED_Interface;

with STM32F4;                             use type STM32F4.Bit, STM32F4.Bits_32;
with STM32F4.Interrupts_and_Events;       use STM32F4.Interrupts_and_Events;
with STM32F4.General_purpose_IOs;         use STM32F4.General_purpose_IOs;
with STM32F4.Random_number_generator.Ops; use STM32F4.Random_number_generator.Ops;

package body Generator_Controllers is

   System_Start : constant Time := Clock;
   System_Ready : constant Time := System_Start + Milliseconds (30);

   function Sine (x : Float) return Float is
      x_2  : constant Float := x**2;
      x_3  : constant Float := x_2 * x;
      x_5  : constant Float := x_3 * x_2;
      x_7  : constant Float := x_5 * x_2;
      x_9  : constant Float := x_7 * x_2;
      x_11 : constant Float := x_9 * x_2;
   begin
      return (x - 1.0 / 6.0 * x_3 +  1.0 / 120.0 * x_5 -  1.0 / 5040.0 * x_7 + 1.0 / 362880.0 * x_9 - 0.000000025 * x_11);
   end Sine;

   procedure Sine_Cycle (Start_Time : Time; Period : Time_Span; Sample_Rate : Integer := 20) is

      Next_Step : Time           := Start_Time;
      Step      : constant Float := 2.0 * 3.1415926 / Float (Sample_Rate);
      Wave_x    : Float          := 0.0;
      Wave_y    : Float          := 0.0;
      pragma Unreferenced (Wave_y);
   begin
      loop
         Wave_y := Sine (Wave_x);
         Wave_x := Wave_x + Step;

         ---------------------------------
         -- do something with Sine wave --
         ---------------------------------

         if Wave_x > 2.0 * 3.1415926 then
            exit;
         end if;

         Next_Step := Next_Step + Period / Sample_Rate;
         delay until Next_Step;
      end loop;
   end Sine_Cycle;

   procedure Become_Master (Port : Com_Ports; Is_Master : out Boolean) with inline is
   begin
      Toggle (Port);
      Set_False (New_Arrival (Port));

      Off ((Port, R));
      On ((Port, L));
      Is_Master := True;
   end Become_Master;

   procedure Become_Slave (Port : Com_Ports; Is_Master : out Boolean) with Inline is
   begin
      Off ((Port, L));
      On ((Port, R));
      Is_Master := False;
   end Become_Slave;

   procedure Select_Master (Port_Num : Com_Ports; Is_Master : out Boolean;
                            Delay_Time : Time := Clock + Nanoseconds (Integer (Random_Data / 2 - 1))) is
   begin
      delay until Delay_Time;

      if Current_State (New_Arrival (Port_Num)) then

         -- has received signal => I'm Not master (port R LED)
         Become_Slave (Port_Num, Is_Master);
      else

         -- no signal coming in => I'm the master (port L LED)
         Become_Master (Port_Num, Is_Master);
      end if;
   end Select_Master;

   task type Controller (My_Port : Com_Ports; Period_Offset : Integer) with
     Storage_Size => 4 * 1024,
     Priority     => Default_Priority;

   task body Controller is

      Period       : constant Time_Span := Milliseconds (50) + Microseconds (Period_Offset);
      Release_Time :          Time      := System_Ready;
      Is_Master    :          Boolean   := False;

   begin
      delay until Release_Time; -- wait for system start
      Select_Master (My_Port, Is_Master);

      loop
         if Is_Master then

            -- output signal
            Toggle (My_Port);
            Toggle (Red);
            Toggle (Orange);

            -- start a Sine cycle
            Sine_Cycle (Release_Time, Period);

            -- check conflict
            if Current_State (New_Arrival (My_Port)) then
               -- Re-select Master => Master should not have incoming signal
               Select_Master (My_Port, Is_Master);
            end if;

            -- delay till next period
            Release_Time := Release_Time + Period;
            delay until Release_Time;

         else

            -- wait on Master
            Suspend_Until_True (New_Arrival (My_Port));
            Toggle (Red);
            Toggle (Orange);

            -- forward signal
            --              for Port in My_Port loop
            --                 if Port /= Direct_Master then
            --                    Toggle (Port);
            --                 end if;
            --              end loop;

            -- start a Sine cycle
            Sine_Cycle (Release_Time, Period);

         end if;

      end loop;

   end Controller;

   Controller_1 : Controller (1, 0);
   Controller_2 : Controller (2, 100);

end Generator_Controllers;
