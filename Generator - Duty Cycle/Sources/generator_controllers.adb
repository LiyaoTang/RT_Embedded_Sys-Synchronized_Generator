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

   ----------------------------------------------------------------------------------------------------------
   -- Record for Incoming Signal => write: Receiver; Read: Sender
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
   -- Record for incoming data (duty cycle)
   ----------------------------------------------------------------------------------------------------------

   protected type Duty_Cycle is

      procedure Record_Duty_Cycle (Start_T : Time; Mid_T : Time; End_T : Time);
      procedure Read_Data (Cur_Data : out Float);

   private
      Proportion_Of_One : Float   := -1.0;
      Has_Update        : Boolean := False;
   end Duty_Cycle;

   protected body Duty_Cycle is

      procedure Record_Duty_Cycle (Start_T : Time; Mid_T : Time; End_T : Time) is
      begin
         Proportion_Of_One := Float (To_Duration (Mid_T - Start_T)) / Float (To_Duration (End_T - Start_T));
         Has_Update := True;
      end Record_Duty_Cycle;

      procedure Read_Data (Cur_Data : out Float) is
      begin
         if Has_Update then
            Cur_Data := Proportion_Of_One;
            Has_Update := False;
         else
            Cur_Data := -1.0;
         end if;
      end Read_Data;

   end Duty_Cycle;

   Duty_Cycle_Data : array (Com_Ports) of Duty_Cycle;

   ----------------------------------------------------------------------------------------------------------
   -- Receiver: Record incoming signal
   ----------------------------------------------------------------------------------------------------------

   type Duty_Cycle_idx_T is mod 2;

   task type Receiver (My_Port : Com_Ports) with Priority => Default_Priority;
   task body Receiver is
      Duty_Cycle_Boundaries : array (Duty_Cycle_idx_T) of Time := (others => Time_First);

      Duty_Cycle_idx : Duty_Cycle_idx_T := 0;
      Duty_Cycle_End : Duty_Cycle_idx_T := 0;
      Mid_Point      : Time             := Time_First;

   begin
      delay until System_Ready;

      loop
         -- rising edge
         Suspend_Until_True (Rising_Arrival (My_Port));
         Duty_Cycle_Boundaries (Duty_Cycle_idx) := Clock;
         Duty_Cycle_idx := Duty_Cycle_idx + 1;

         Incoming_Signal_Register (My_Port).Record_Time (Duty_Cycle_Boundaries (Duty_Cycle_idx - 1)); -- register the time

         -- end of one duty cycle
         if Duty_Cycle_idx = Duty_Cycle_End then
            Duty_Cycle_End := Duty_Cycle_End + 1;

            Duty_Cycle_Data (My_Port).Record_Duty_Cycle (Duty_Cycle_Boundaries (Duty_Cycle_idx), Mid_Point,
                                                         Duty_Cycle_Boundaries (Duty_Cycle_idx - 1));
         end if;

         -- falling edge
         Suspend_Until_True (Falling_Arrival (My_Port));
         Mid_Point := Clock;

      end loop;

   exception
         when others => On (Blue);
   end Receiver;

   Receiver_1 : Receiver (1);
   Receiver_2 : Receiver (2);

   ----------------------------------------------------------------------------------------------------------
   -- Record for Generated Wave & Data => Write: Generator, Sender; Read: Sender, Generator
   ----------------------------------------------------------------------------------------------------------

   protected type Wave_Record with Interrupt_Priority => Interrupt_Priority'Last is

      entry Get_Init_Wave_Info (Init_Release_Time : out Time; Init_Period : out Time_Span);
      procedure Set_Init_Wave_Info (Init_Release_Time : Time; Init_Period : Time_Span);

      procedure Record_Adjustment (Next_Start_Time : Time; Suggested_Period : Time_Span; My_Data : out Float);
      procedure Sync_with_Sender (Last_release_Time : in out Time; Cur_Period : in out Time_Span;
                                     New_Update : out Boolean; My_Data : Float)
        with pre => (0.0 <= My_Data and then My_Data <= 1.0);

   private

      Start_Time : Time      := System_Ready;
      Period     : Time_Span := Time_Span_Zero;
      Has_Update : Boolean   := False;

      Data_Out   : Float     := -1.0;

   end Wave_Record;

   protected body Wave_Record is

      entry Get_Init_Wave_Info (Init_Release_Time : out Time; Init_Period : out Time_Span) when Has_Update is
      begin
         Has_Update := False;
         Init_Release_Time := Start_Time;
         Init_Period := Period;
      end Get_Init_Wave_Info;

      procedure Set_Init_Wave_Info (Init_Release_Time : Time; Init_Period : Time_Span) is
      begin
         Start_Time := Init_Release_Time;
         Period := Init_Period;
         Has_Update := True;
      end Set_Init_Wave_Info;

      procedure Record_Adjustment (Next_Start_Time : Time; Suggested_Period : Time_Span; My_Data : out Float) is
      begin
         Start_Time := Next_Start_Time;
         Period     := Suggested_Period;
         Has_Update := True;

         if Data_Out >= 0.0 then
            My_Data := Data_Out;
         else
            My_Data := -1.0;
         end if;

      end Record_Adjustment;

      procedure Sync_with_Sender (Last_release_Time : in out Time; Cur_Period : in out Time_Span;
                                     New_Update : out Boolean; My_Data : Float) is
      begin
         New_Update := Has_Update;

         if Has_Update then

            -- make sure sync time is valid
            while Start_Time < Last_release_Time + Cur_Period loop
               Start_Time := Start_Time + Period;
            end loop;

            Last_release_Time  := Start_Time;
            Cur_Period := Period;
            Has_Update := False;
         end if;

         Data_Out := My_Data;

      end Sync_with_Sender;

   end Wave_Record;

   Wave_Records : array (1 .. 2) of Wave_Record;

   ----------------------------------------------------------------------------------------------------------
   -- Sender: Sync with incoming signal & Send signal
   ----------------------------------------------------------------------------------------------------------

   type Changing_Status is mod 3;

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
        abs (In_Remote_Period - In_Recent_Period) < Microseconds (5) and then -- stable incomingming Signal_Record
        abs (In_Peak - Cur_Release_Time)  > Microseconds (5) and then -- detected phase shift
        abs (In_Peak - Last_Release_Time) > Microseconds (5)
      then

         -- disrupt output signal
         Reset (My_Port);
         Set   (My_Port);
         Reset (My_Port);
         Set   (My_Port);

         Next_Release_Time := In_Peak + In_Period;
         My_Period := In_Period;

         Toggle (((My_Port + 2), R));

         Changing := 1;
      end if;

   end Adjust_Phase;

   task type Sender (My_Port : Com_Ports) with
     Storage_Size => 4 * 1024,
     Priority     => Default_Priority;
   task body Sender is

      Record_Num   : constant Integer         := Integer (My_Port);
      My_Period    :          Time_Span       := Time_Span_Zero;
      Release_Time :          Time            := System_Ready;
      Changing     :          Changing_Status := 0;

      Data_Out     :          Float           := -1.0;

   begin
      -- Get initial period
      Wave_Records (Record_Num).Get_Init_Wave_Info (Release_Time, My_Period);
      delay until System_Ready;

      loop
         -- output signal & make sure sync on rising edge
         Set (My_Port);

         Toggle (Green);
         Toggle (Blue);

         -- Adjust Release_Time
         Release_Time := Release_Time + My_Period;

         if Changing = 0 then
            Adjust_Phase (My_Port, Release_Time, My_Period, Changing);
         end if;

         if Changing = 0 then
            -- Has been synchronized => Inform Generator & Start duty cycle
            Wave_Records (Record_Num).Record_Adjustment (Release_Time, My_Period, Data_Out);

            -- Send valid duty cycle (only when data from Generator is valid)
            if 0.0 <= Data_Out and then Data_Out <= 1.0 then
               declare
                  Switch_To_Zero : constant Time  := Release_Time - (To_Time_Span (Duration (Float (To_Duration (My_Period)) *
                                                                     (1.0 - Data_Out))));
               begin

                  delay until Switch_To_Zero;
                  Reset (My_Port);
               end;
            else
               Reset (My_Port);
            end if;

         else
            -- synchronizing => disrupt output signal & wait for Generator to adjust
            Changing := Changing + 1;

            Reset (My_Port);
            Set (My_Port);
            Reset (My_Port);
         end if;

         -- delay till next period
         delay until Release_Time;
      end loop;

   exception
      when others => On (Blue);
   end Sender;

   Sender_1 : Sender (1);
   Sender_2 : Sender (2);

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

   task type Generator (Record_Num : Integer; Period_Offset : Integer) with Priority => Priority'Last - 1;
   task body Generator is

      My_Port      : constant Com_Ports := Com_Ports (Record_Num);
      Sample_Rate  : constant Integer  := 20;
      Step_x       : constant Float    := 3.1415926 / Float (Sample_Rate);

      Period       : Time_Span := Milliseconds (50) + Microseconds (Period_Offset); -- Freq = 50Hz
      Release_Time : Time      := System_Ready;

      Wave_x       : Float     := 0.0;
      Wave_y       : Float     := 0.0;
      Data_In      : Float     := 0.0;
      pragma Unreferenced (Wave_y);

      New_Update   : Boolean   := False;

   begin
      Wave_Records (Record_Num).Set_Init_Wave_Info (System_Ready, Period);
      delay until System_Ready;

      loop
         -- Calculate & Update wave val;
         Wave_y := Sine (Wave_x);
         Wave_x := Wave_x + Step_x;

         Duty_Cycle_Data (My_Port).Read_Data (Data_In);

         -- do something with my income data & sine value => better to be non-blocking
         if Data_In > 0.1 and then Data_In < 0.5 then
            Toggle ((My_Port, R));
         end if;

         -- end of current sine cycle
         if Wave_x > 2.0 * 3.1415926 then
            Wave_x := 0.0;

            -- Adjust
            Wave_Records (Record_Num).Sync_with_Sender (Release_Time, Period, New_Update, 0.3);

            if not New_Update then
               Release_Time := Release_Time + Period / Sample_Rate;
            end if;

         else
            Release_Time := Release_Time + Period / Sample_Rate;
         end if;

         delay until Release_Time;

         if Wave_x = 0.0 then
            Toggle (Red);
            Toggle (Orange);
         end if;
      end loop;
   end Generator;

   Generator_1 : Generator (1, 0);
   Generator_2 : Generator (2, 100);

end Generator_Controllers;
