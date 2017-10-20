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
with STM32F4.DMA_controller;                      use STM32F4.DMA_controller;
with STM32F4.Timers;                              use STM32F4.Timers;
-- with STM32F4.Timers.Ops;                          use STM32F4.Timers.Ops;

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
                  declare
                     Is_Rising : constant Boolean := (Read (Port_Num) = 1);
                  begin
                     if Is_Rising then
                        Ada.Synchronous_Task_Control.Set_True (Rising_Arrival (Port_Num));
                        Ada.Synchronous_Task_Control.Set_False (Falling_Arrival (Port_Num));
                     else
                        Ada.Synchronous_Task_Control.Set_True (Falling_Arrival (Port_Num));
                        Ada.Synchronous_Task_Control.Set_False (Rising_Arrival (Port_Num));
                     end if;
                  end;
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

--     TIM6_Semaphore : Ada.Synchronous_Task_Control.Suspension_Object;
--
--     protected body TIM6_Interrupt is
--
--        procedure Enable_TIM6 is
--        begin
--
--           STM32F4.Reset_and_clock_control.Ops.Enable (Timer_No (6));
--           STM32F4.Timers.Ops.Enable (Timer_No (6));
--           STM32F4.Timers.Ops.Enable (Timer_No (6), Update);
--
--           STM32F4.Timers.Ops.Set_Prescaler (6, 10000);
--           STM32F4.Timers.Ops.Set_Auto_Reload_16 (6, 20_000); -- interrupt period : 20 milliseconds
--        end Enable_TIM6;
--
--        procedure TIM6_Interrupt_Handler is
--        begin
--           Set_True (TIM6_Semaphore);
--        end TIM6_Interrupt_Handler;
--
--     end TIM6_Interrupt;
--
--     task TIM6_Slave;
--     task body TIM6_Slave is
--     begin
--        loop
--           Toggle (Green);
--           Suspend_Until_True (TIM6_Semaphore);
--        end loop;
--     end TIM6_Slave;

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

--     Enable (DMA1);
--     -- request from TIM6_UP
--     Configure (DMA                         => 1;
--                Stream                      => 1;
--                Stream_Enable               => STM32F4.Enable;
--  --                Direct_mode_error_interrupt : Enabler        := Disable;
--  --                Transfer_error_interrupt    : Enabler        := Disable;
--  --                Half_transfer_interrupt     : Enabler        := Disable;
--  --                Transfer_complete_interrupt : Enabler        := Disable;
--                Peripheral_flow_controller  => PFCTRL_Options := DMA_Flowcontrol;
--                Data_transfer_direction     => Memory_to_Peripheral;
--                Circular_mode               => STM32F4.Enable;
--  --                Peripheral_increment_mode   : INC_Options    := Address_Fixed;
--                Memory_increment_mode       => Address_Fixed;
--                Peripheral_data_size        => Byte_Size;
--                Memory_data_size            => Byte_Size;
--  --                Peripheral_increment_offset : PINCOS_Options := Linked_to_PSIZE;
--  --                Priority_levely_level       : PL_Options     := Low;
--  --                Double_buffer_modee_buffer_mode : Enabler    := Disable;
--  --                Peripheral_burst            : BURST_Options  := Single;
--  --                Memory_burst                : BURST_Options  := Single;
--                Channel                     : => 7);

   -- enable TIM6

   -- TIM6_Interrupt.Enable_TIM6;

   Port_Inspector.Initialize_Interrupt;
exception
   when others => Discovery_Board.LED_Interface.On (Blue);
end ANU_Base_Board.Com_Interface;
