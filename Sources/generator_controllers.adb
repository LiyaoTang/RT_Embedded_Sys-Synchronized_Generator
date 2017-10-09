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

   Allowed_Delay  : constant Time_Span := Nanoseconds (100);

   procedure Change_LED_for_Data (My_Data : STM32F4.Bit; LED : ANU_Base_Board.LEDs) with Inline is
   begin
      if My_Data = 1 then
         On (LED);
      else
         Off (LED);
      end if;
   end Change_LED_for_Data;

   procedure Send_Data_to_Port (My_Data : STM32F4.Bit; Port : Com_Ports) with Inline is
   begin
      if My_Data = 1 then
         Set (Port);
      else
         Reset (Port);
      end if;
   end Send_Data_to_Port;

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
      In_Data : STM32F4.Bit := 0;
   begin
      delay until System_Ready;

      loop
         -- Incoming signal
         Suspend_Until_True (New_Arrival (My_Port));
         Incoming_Signal_Register (My_Port).Record_Time (Clock); -- register the time
         In_Data := Read (My_Port); -- read data

         -- Port LEDs (R => Incoming)
         Change_LED_for_Data (In_Data, (My_Port, R));
      end loop;

   exception
         when others => On (Blue);
   end Receiver;

   Receiver_1 : Receiver (1);
   Receiver_2 : Receiver (2);
   Receiver_3 : Receiver (3);
   Receiver_4 : Receiver (4);

   procedure Adjust_Phase (My_Port : Com_Ports; Next_Release_Time : in out Time; My_Period : in out Time_Span; Changing : out Boolean)
     with inline is

      In_Signal         : constant Signal_Record  := Incoming_Signal_Register (My_Port).Get_Incoming_Signal_Info;
      Time_Stamps       : constant In_Time_Arr    := In_Signal.Time_Stamps; -- if use 'renames' => huge difference! (How renames work?)
      Time_idx          : constant In_Time_idx_T  := In_Signal.Cur_idx;

      In_Peak           : constant Time           := Time_Stamps (Time_idx - 1);
      In_Period         : constant Time_Span      := ((Time_Stamps (Time_idx - 1) - Time_Stamps (Time_idx - 2)) +
                                                     (Time_Stamps (Time_idx - 2) - Time_Stamps (Time_idx))) / 2;

      Cur_Release_Time  : constant Time           := Next_Release_Time - My_Period;
      Last_Release_Time : constant Time           := Next_Release_Time - 2 * My_Period;
   begin

      Changing := False;

      -- check only if incoming signal stable & phase shift detected
      if abs ((Time_Stamps (Time_idx - 1) - Time_Stamps (Time_idx - 2)) -
              (Time_Stamps (Time_idx - 2) - Time_Stamps (Time_idx))) < Allowed_Delay and then -- stable incoming signal
        abs (In_Peak - Cur_Release_Time) > Allowed_Delay and then -- detected phase shift
        abs (In_Peak - Last_Release_Time) > Allowed_Delay
      then

         if abs (In_Period - My_Period) < Allowed_Delay then -- same period => new one adjust (first detect first adjust)

            Next_Release_Time := In_Peak + In_Period;

            Changing := True;

         elsif My_Period < In_Period then -- different period => short one adjust

            Next_Release_Time := In_Peak + In_Period;
            My_Period := In_Period;

            Changing := True;
         end if;
      end if;

   end Adjust_Phase;

   task Controller with
     Storage_Size => 4 * 1024,
     Priority     => Default_Priority;

   task body Controller is

      My_Port      : constant Com_Ports   := Com_Ports (1);

      My_Period    :          Time_Span   := Milliseconds (500);
      My_Data      :          STM32F4.Bit := 0;
      Release_Time :          Time        := System_Ready;

      Changing     :          Boolean     := False;

   begin
      delay until System_Ready;
      Toggle (Red);

      loop
         -- output data & LED (L => Outgoing)
         Send_Data_to_Port (My_Data, My_Port);
         Change_LED_for_Data (My_Data, (My_Port, L));
         My_Data := My_Data + 1;

         Toggle (Orange);
         Toggle (Red);

         -- Adjust Release_Time
         Release_Time := Release_Time + My_Period;
         if not Changing then
            Adjust_Phase (My_Port, Release_Time, My_Period, Changing);
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

      My_Period    :          Time_Span   := Milliseconds (500) + Milliseconds (50);
      My_Data      :          STM32F4.Bit := 0;
      Release_Time :          Time        := System_Ready;

      Changing     :          Boolean     := False;

   begin
      delay until System_Ready + Milliseconds (100);

      loop
         declare
            Follower_Enabled : constant Boolean := Button.Current_Blue_Button_State = Off;
         begin
            if Follower_Enabled then

               -- output data & LED (L => Outgoing)
               Send_Data_to_Port (My_Data, My_Port);
               Change_LED_for_Data (My_Data, (My_Port, L));
               My_Data := My_Data + 1;

               Toggle (Orange);
               Toggle (Red);

               -- Adjust Release_Time
               Release_Time := Release_Time + My_Period;
               if not Changing then
                  Adjust_Phase (My_Port, Release_Time, My_Period, Changing);
               end if;

               -- delay till next period
               delay until Release_Time;

            else
               delay until Clock - Milliseconds (100); -- try not occupy the whole CPU
            end if;
         end;
      end loop;

   exception
         when others => On (Blue);
   end Follower;

end Generator_Controllers;
