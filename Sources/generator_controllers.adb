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
with STM32F4.Random_number_generator.Ops; use STM32F4.Random_number_generator.Ops;

package body Generator_Controllers is

   System_Start : constant Time := Clock;

   procedure Become_Master (Port : Com_Ports; Is_Master : out Boolean) with inline is
   begin
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

   procedure Change_LED_for_Data (My_Data : STM32F4.Bit; LED : Discovery_Board.LEDs) with Inline is
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

   procedure Select_Master (Port_Num : Com_Ports; Is_Master : out Boolean;
                            Delay_Time : Time := Clock + Nanoseconds (Integer (Random_Data / 2 - 1))) is
   begin
      delay until Delay_Time;

      if Current_State (New_Arrival (Port_Num)) then

         -- has received signal => I'm Not master (port R LED)
         Become_Slave (Port_Num, Is_Master);
      else

         -- no signal coming in => I'm the master (port L LED)
         Toggle (Port_Num);
         Become_Master (Port_Num, Is_Master);
      end if;
   end Select_Master;

   task Controller with
     Storage_Size => 4 * 1024,
     Priority     => Default_Priority;

   task body Controller is

      Period       : constant Time_Span            := Milliseconds (500);
      My_Port      : constant Com_Ports            := Com_Ports (1);
      My_LED       : constant Discovery_Board.LEDs := Red;
      My_Data      :          STM32F4.Bit          := 0;
      Release_Time :          Time                 := System_Start + Milliseconds (30);
      Is_Master    :          Boolean              := False;
      Changed      :          Boolean              := True;

   begin
      delay until Release_Time; -- wait for system start
      Release_Time := Release_Time + Milliseconds (40) + Nanoseconds (1);
      Select_Master (My_Port, Is_Master, Delay_Time => Release_Time);

      loop
         if Is_Master then

            -- board LEDs
            Change_LED_for_Data (My_Data, My_LED);
            On (Orange);

            -- output data
            Send_Data_to_Port (My_Data, My_Port);
            My_Data := My_Data + 1;

            delay until Clock + Milliseconds (1);

            -- check conflict
            if Current_State (New_Arrival (My_Port)) then
               -- Re-select Master => Master should not have incoming signal
               Select_Master (My_Port, Is_Master);
            end if;

            -- delay till next period
            Release_Time := Release_Time + Period;
            delay until Release_Time; -- something wrong here...

         else

            -- read in data
            Suspend_Until_True (New_Arrival (My_Port));
            My_Data := Read (My_Port);

            -- board LEDs
            Change_LED_for_Data (My_Data, My_LED);
            Off (Orange);

            if not Changed then
               Become_Master (My_Port, Is_Master);
               Changed := True;
            end if;
         end if;

      end loop;

   end Controller;

   task Follower with
     Storage_Size => 4 * 1024,
     Priority     => Default_Priority;

   task body Follower is

      Period       : constant Time_Span            := Milliseconds (500) + Microseconds (150);
      My_Port      : constant Com_Ports            := Com_Ports (2);
      My_LED       : constant Discovery_Board.LEDs := Green;
      My_Data      :          STM32F4.Bit          := 0;
      Release_Time :          Time                 := System_Start + Milliseconds (30);
      Is_Master    :          Boolean              := False;

   begin
      delay until Release_Time;
      Release_Time := Release_Time + Milliseconds (400) + Nanoseconds (0);
      Select_Master (My_Port, Is_Master, Delay_Time => Release_Time);

      loop
         declare
            Follower_Enabled : constant Boolean := Button.Current_Blue_Button_State = Off;
         begin
            if Follower_Enabled then

               if Is_Master then

                  -- board LEDs
                  Change_LED_for_Data (My_Data, My_LED);
                  On (Orange);

                  -- output data
                  Send_Data_to_Port (My_Data, My_Port);
                  My_Data := My_Data + 1;

                  Release_Time := Release_Time + Period;
                  delay until Release_Time;

                  -- check conflict
                  if Current_State (New_Arrival (My_Port)) then

                     -- Re-select Master => Master should not have incoming signal
                     Select_Master (My_Port, Is_Master);
                  end if;

               else

                  -- read in data
                  Suspend_Until_True (New_Arrival (My_Port));
                  My_Data := Read (My_Port);

                  -- board LEDs
                  Change_LED_for_Data (My_Data, My_LED);
                  Off (Orange);

               end if;

            else
               delay until Clock - Milliseconds (100); -- try not occupy the whole CPU
            end if;
         end;
      end loop;

   end Follower;

end Generator_Controllers;
