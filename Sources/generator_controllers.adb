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
with Discovery_Board.Button;              use Discovery_Board.Button;
with Discovery_Board.LED_Interface;       use Discovery_Board.LED_Interface;

with STM32F4;                             use type STM32F4.Bit, STM32F4.Bits_32;
with STM32F4.Interrupts_and_Events;       use STM32F4.Interrupts_and_Events;
with STM32F4.General_purpose_IOs;         use STM32F4.General_purpose_IOs;

package body Generator_Controllers is

   System_Start   : constant Time      := Clock;
   System_Ready   : constant Time      := System_Start + Milliseconds (30);

   Allowed_Delay  : constant Time_Span := Nanoseconds (10);

   type In_Time_idx_T is mod 3;
   type In_Time_Arr   is array (In_Time_idx_T) of Time;
   type Signal_Record is record
      Time_Stamps : In_Time_Arr   := (others => System_Ready);
      Cur_idx     : In_Time_idx_T := 0;
   end record;

   protected type Incoming_Signal is
      procedure Record_Time (New_Arrive_Time : Time);
      function Get_Incoming_Signal_Info return Signal_Record;
   private
      Signal : Signal_Record;
   end Incoming_Signal;

   protected body Incoming_Signal is
      procedure Record_Time (New_Arrive_Time : Time) is
      begin
         Signal.Time_Stamps (Signal.Cur_idx) := New_Arrive_Time;
         Signal.Cur_idx := Signal.Cur_idx + 1;
      end Record_Time;

      function Get_Incoming_Signal_Info return Signal_Record is
        (Signal);
   end Incoming_Signal;

   Incoming_Signal_Register : array (Com_Ports) of Incoming_Signal;

   task type Receiver (My_Port : Com_Ports) with Priority => Default_Priority;
   task body Receiver is
   begin
      delay until System_Ready;

      loop
         -- Incoming signal
         Suspend_Until_True (New_Arrival (My_Port));
         Incoming_Signal_Register (My_Port).Record_Time (Clock); -- register the time

         -- Port LEDs (R => Incoming)
         Toggle ((My_Port, R));
      end loop;

   exception
         when others => On (Blue);
   end Receiver;

   Receiver_1 : Receiver (1);
   Receiver_2 : Receiver (2);
   Receiver_3 : Receiver (3);
   Receiver_4 : Receiver (4);

   type Changing_Status is mod 4;

   procedure Adjust_Phase (My_Port : Com_Ports; Next_Release_Time : in out Time; My_Period : in out Time_Span; Changing : out Changing_Status)
     with inline is

      In_Signal         : constant Signal_Record  := Incoming_Signal_Register (My_Port).Get_Incoming_Signal_Info;
      Time_Stamps       : constant In_Time_Arr    := In_Signal.Time_Stamps; -- if use 'renames' => huge difference! (How renames work?)
      Time_idx          : constant In_Time_idx_T  := In_Signal.Cur_idx;

      In_Peak           : constant Time           := Time_Stamps (Time_idx - 1);
      In_Recent_Period  : constant Time_Span      := Time_Stamps (Time_idx - 1) - Time_Stamps (Time_idx - 2);
      In_Remote_Period  : constant Time_Span      := Time_Stamps (Time_idx - 2) - Time_Stamps (Time_idx);
      In_Period         : constant Time_Span      := (In_Recent_Period + In_Remote_Period) / 2;

      Cur_Release_Time  : constant Time           := Next_Release_Time - My_Period;
      Last_Release_Time : constant Time           := Next_Release_Time - 2 * My_Period;
   begin

      Changing := 0;

      -- check only if incoming signal stable & phase shift detected
      if (for all t of Time_Stamps => t /= System_Ready) and then -- enough incoming signal
      abs (In_Recent_Period - In_Remote_Period) < Allowed_Delay and then -- stable incoming signal
        abs (In_Peak - Cur_Release_Time) > Allowed_Delay and then -- detected phase shift
        abs (In_Peak - Last_Release_Time) > Allowed_Delay
      then

         if abs (In_Period - My_Period) < Allowed_Delay then -- same period

            Next_Release_Time := In_Peak + In_Period;

            Changing := 1;

         else -- different period

            Next_Release_Time := In_Peak + In_Period;
            My_Period := In_Period;

            Changing := 1;

         end if;
      end if;

   end Adjust_Phase;

   task Controller with
     Storage_Size => 4 * 1024,
     Priority     => Default_Priority;

   task body Controller is

      My_Port      : constant Com_Ports   := Com_Ports (1);

      My_Period    :          Time_Span   := Milliseconds (50);
      Release_Time :          Time        := System_Ready;

      Changing     :          Changing_Status     := 0;

   begin
      delay until System_Ready;

      loop
         -- output data & LED (L => Outgoing)
         Toggle (My_Port);

         Toggle (Orange);
         Toggle (Red);

         -- Adjust Release_Time
         Release_Time := Release_Time + My_Period;
         if Changing = 0 then
            Adjust_Phase (My_Port, Release_Time, My_Period, Changing);
         else
            Changing := Changing + 1;
         end if;

         -- delay till next period
         delay until Release_Time;
      end loop;

   exception
         when others => On (Blue);
   end Controller;

   task Follower with
     Storage_Size => 4 * 1024,
     Priority     => Default_Priority;

   task body Follower is

      My_Port      : constant Com_Ports   := Com_Ports (2);

      My_Period    :          Time_Span   := Milliseconds (50) + Microseconds (150);
      Release_Time :          Time        := System_Ready;

      Changing     :          Changing_Status     := 0;

   begin
      delay until System_Ready + Milliseconds (10);

      loop
         declare
            Follower_Enabled : constant Boolean := Button.Current_Blue_Button_State = Off;
         begin
            if Follower_Enabled then

               -- output data & LED (L => Outgoing)
               Toggle (My_Port);

               Toggle (Orange);
               Toggle (Red);

               -- Adjust Release_Time
               Release_Time := Release_Time + My_Period;
               if Changing = 0 then
                  Adjust_Phase (My_Port, Release_Time, My_Period, Changing);
               else
                  Changing := Changing + 1;
               end if;

               -- delay till next period
               delay until Release_Time;

            else
               delay until Clock - Milliseconds (1); -- try not occupy the whole CPU
            end if;
         end;
      end loop;

   exception
         when others => On (Blue);
   end Follower;

end Generator_Controllers;
