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

package body Generator_Controllers is

   System_Start   : constant Time      := Clock;
   System_Ready   : constant Time      := System_Start + Milliseconds (30);

   Allowed_Delay  : constant Time_Span := Nanoseconds (1000);

   ----------------------------------------------------------------------------------------------------------
   -- Recording Incoming Signal => write: Receiver; Read: Sender
   ----------------------------------------------------------------------------------------------------------

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

   ----------------------------------------------------------------------------------------------------------
   -- Receiver: Record incoming signal
   ----------------------------------------------------------------------------------------------------------

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

   ----------------------------------------------------------------------------------------------------------
   -- Recording Generated Wave => Write: Generator, Sender; Read: Sender, Generator
   ----------------------------------------------------------------------------------------------------------

   protected type Wave_Record with Interrupt_Priority => Interrupt_Priority'Last is

      procedure Set_My_Phase (Next_Start_Time : Time; My_Period : Time_Span);
      procedure Get_My_Phase (Next_Start_Time : in out Time; Suggested_Period : out Time_Span);

   private

      Start_Time : Time      := System_Ready;
      Period     : Time_Span := Time_Span_Zero;
      Has_Update : Boolean   := False;

   end Wave_Record;

   protected body Wave_Record is

      procedure Set_My_Phase (Next_Start_Time : Time; My_Period : Time_Span) is
      begin
         Start_Time := Next_Start_Time;
         Period     := My_Period;
         Has_Update := True;
      end Set_My_Phase;

      procedure Get_My_Phase (Next_Start_Time : in out Time; Suggested_Period : out Time_Span) is
      begin
         if Has_Update then
            Next_Start_Time  := Start_Time;
            Suggested_Period := Period;
            Has_Update := False;
         end if;
      end Get_My_Phase;

   end Wave_Record;

   Wave_Records : array (1 .. 2) of Wave_Record;

   ----------------------------------------------------------------------------------------------------------
   -- Sender: Sync with incoming signal & Send signal
   ----------------------------------------------------------------------------------------------------------

   Generator_Ready : array (1 .. 2) of Ada.Synchronous_Task_Control.Suspension_Object;

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

         else  -- different period

            Next_Release_Time := In_Peak + In_Period;
            My_Period := In_Period;

            Changing := 1;

         end if;

         if Changing = 1 then
            Toggle (Green);
         end if;
      end if;

   end Adjust_Phase;

   task type Sender (Record_Num : Integer; Com_Ports_No : Com_Ports) with
     Storage_Size => 4 * 1024,
     Priority     => Default_Priority;
   task body Sender is

      My_Port      : constant Com_Ports           := Com_Ports_No;
      My_Period    :          Time_Span           := Time_Span_Zero;
      Release_Time :          Time                := System_Ready;
      Changing     :          Changing_Status     := 0;

   begin
      -- Get initial period
      Suspend_Until_True (Generator_Ready (Record_Num));
      Wave_Records (Record_Num).Get_My_Phase (Release_Time, My_Period);
      delay until System_Ready;

      loop
         -- output data
         Toggle (My_Port);

         -- Adjust Release_Time
         Release_Time := Release_Time + My_Period;
         if Changing = 0 then
            Adjust_Phase (My_Port, Release_Time, My_Period, Changing);

            -- Inform the generator
            if Changing = 1 then
               Wave_Records (Record_Num).Set_My_Phase (Release_Time, My_Period);
            end if;
         else
            Changing := Changing + 1;
         end if;

         -- delay till next period
         delay until Release_Time;
      end loop;

   exception
      when others => On (Blue);
   end Sender;

   Sender_1 : Sender (1, 1);
   Sender_2 : Sender (2, 2);

   ----------------------------------------------------------------------------------------------------------
   -- Generator: Generate local sine wave & adjust behaviour if needed
   ----------------------------------------------------------------------------------------------------------

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

   task type Generator (Record_Num : Integer; Period_Offset : Integer);
   task body Generator is
      Sample_Rate  : constant Integer := 20;
      Step_x       : constant Float   := 3.1415926 / Float (Sample_Rate);

      Period       : Time_Span := Milliseconds (50) + Microseconds (Period_Offset); -- Freq = 50Hz
      Release_Time : Time      := System_Ready;

      Wave_x       : Float     := 0.0;
      Wave_y       : Float     := 0.0;
      pragma Unreferenced (Wave_y);

   begin
      Wave_Records (Record_Num).Set_My_Phase (System_Ready, Period);
      Set_True (Generator_Ready (Record_Num));
      delay until System_Ready;

      loop
         Release_Time := Release_Time + Period / Sample_Rate;

         -- Calculate & Update wave val
         Wave_y := Sine (Wave_x);
         Wave_x := Wave_x + Step_x;

         ------------------------------------
         -- do something with my sine wave --
         ------------------------------------

         if Wave_x > 2.0 * 3.1415926 then
            Wave_x := 0.0;

            -- Adjust
            Wave_Records (Record_Num).Get_My_Phase (Release_Time, Period);
         end if;

         delay until Release_Time;

         if Wave_x = 0.0 then
            Toggle (Red);
            Toggle (Orange);
         end if;

      end loop;
   end Generator;

   Generator_1 : Generator (1, 0);
   Generator_2 : Generator (2, 1000);

end Generator_Controllers;
