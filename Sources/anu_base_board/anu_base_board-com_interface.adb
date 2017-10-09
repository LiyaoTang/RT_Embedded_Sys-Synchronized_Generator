--
-- Uwe R. Zimmer, Australia 2015
--

with ANU_Base_Board.Config;                       use ANU_Base_Board.Config;
with ANU_Base_Board;                              use ANU_Base_Board;
with Discovery_Board;                             use Discovery_Board;
with Discovery_Board.LED_Interface;               use Discovery_Board.LED_Interface;

with STM32F4.General_purpose_IOs.Ops;             use STM32F4.General_purpose_IOs.Ops;
with STM32F4.Reset_and_clock_control.Ops;         use STM32F4.Reset_and_clock_control.Ops;
with STM32F4.System_configuration_controller.Ops; use STM32F4.System_configuration_controller.Ops;
with STM32F4.Interrupts_and_Events;               use STM32F4.Interrupts_and_Events;
with STM32F4.Interrupts_and_Events.Ops;           use STM32F4.Interrupts_and_Events.Ops;
with STM32F4.Random_number_generator.Ops;         use STM32F4.Random_number_generator.Ops;

package body ANU_Base_Board.Com_Interface is

   procedure Set (Port : Com_Ports) is

   begin
      Set (Com_Wires (Port, Tx, Data));
   end Set;

   procedure Reset (Port : Com_Ports) is

   begin
      Reset (Com_Wires (Port, Tx, Data));
   end Reset;

   procedure Toggle (Port : Com_Ports) is

   begin
      Atomic_Switch.Toggle (Com_Wires (Port, Tx, Data));
   end Toggle;

   function Read (Port : Com_Ports) return Bit is

     (Input_Read (Com_Wires (Port, Rx, Data)));

   protected body Port_Inspector is

      procedure Invoking_Receiver is
      begin
         for Port_Num in Com_Ports loop
            declare
               Wire : constant Port_Pin := Com_Wires (Port_Num, Rx, Da);
            begin
               if Happened (Wire.Pin) then
                  Clear_Interrupt (Wire.Pin);
                  Ada.Synchronous_Task_Control.Set_True (New_Arrival (Port_Num));
               end if;
            end;
         end loop;

      end Invoking_Receiver;

      procedure Initialize_Interrupt is
      begin
         for Port_Num in Com_Ports loop
            declare
               Wire : constant Port_Pin := Com_Wires (Port_Num, Rx, Da);
            begin
               Enable (System_Configuration_Contr);

               Set_Interrupt_Source (External_Interrupt_No (Wire.Pin), Wire.Port);

               Set_Trigger (Line    => Wire.Pin,
                            Raising => Enable,
                            Falling => Enable);
               Masking (Line  => Wire.Pin,
                        State => Unmasked);
            end;
         end loop;

      end Initialize_Interrupt;

   end Port_Inspector;

begin
   for Port in Com_Ports loop
      for Dir in Com_Directions loop
         for Feature in Com_Features loop
            Enable (Com_Wires (Port, Dir, Feature).Port);
         end loop;
         Initialize_Output_Pin (Com_Wires (Port, Dir, Enable));

      end loop;
      Initialize_Output_Pin (Com_Wires (Port, Tx, Data));
      Initialize_Input_Pin  (Com_Wires (Port, Rx, Data), Pull_Down);   -- used to be 'Pull_Down'
      Reset                 (Com_Wires (Port, Rx, Enable)); -- Active low!
      Set                   (Com_Wires (Port, Tx, Enable));
      Set                   (Com_Wires (Port, Tx, Data));
   end loop;

   Enable (STM32F4.Reset_and_clock_control.Ops.Random_number_generator);
   Random_Enable;

   Port_Inspector.Initialize_Interrupt;
exception
   when others => Discovery_Board.LED_Interface.On (Blue);
end ANU_Base_Board.Com_Interface;
