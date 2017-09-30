--
-- Uwe R. Zimmer, Australia 2015
--

with Ada.Interrupts.Names;                        use Ada.Interrupts.Names;
with Ada.Real_Time;                               use Ada.Real_Time;
with Discovery_Board.Config;                      use Discovery_Board.Config;
with STM32F4;                                     use STM32F4;
with STM32F4.General_purpose_IOs;                 use STM32F4.General_purpose_IOs;
with STM32F4.General_purpose_IOs.Ops;             use STM32F4.General_purpose_IOs.Ops;
with STM32F4.Interrupts_and_Events;               use STM32F4.Interrupts_and_Events;
with STM32F4.Interrupts_and_Events.Ops;           use STM32F4.Interrupts_and_Events.Ops;
with STM32F4.Reset_and_clock_control.Ops;         use STM32F4.Reset_and_clock_control.Ops;
with STM32F4.System_configuration_controller.Ops; use STM32F4.System_configuration_controller.Ops;
with System;                                      use System;
with ANU_Base_Board.LED_Interface;
with ANU_Base_Board; use ANU_Base_Board;

package body Discovery_Board.Button is

   protected Blue_Button with Interrupt_Priority => Interrupt_Priority'First is

      function Current_State return Blue_Button_State;

   private
      procedure Interrupt_Handler with Attach_Handler => EXTI0_Interrupt;
      -- Hardwired to a pin 0 - need to be changed if the button finds itself on another pin
      pragma Unreferenced (Interrupt_Handler);

      State     : Blue_Button_State := Off;
      Last_Time : Time              := Clock;
   end Blue_Button;

   Debounce_Time : constant Time_Span := Milliseconds (500);

   protected body Blue_Button is

      function Current_State return Blue_Button_State is (State);

      procedure Interrupt_Handler is

         Now : constant Time := Clock;

      begin
         Clear_Interrupt (Button_Wire.Pin);

         if Now - Last_Time >= Debounce_Time then
            State     := (if State = On then Off else On);
            Last_Time := Now;
         end if;
      end Interrupt_Handler;

   end Blue_Button;

   function Current_Blue_Button_State return Blue_Button_State is (Blue_Button.Current_State);

   procedure Initialize is

      Port : GPIO_Ports renames Button_Wire.Port;
      Pin  : GPIO_Pins  renames Button_Wire.Pin;

   begin
      Enable (Port);

      Set_Mode (Button_Wire, Input);
      Set_Pull (Button_Wire, No_Pull);

      Enable (System_Configuration_Contr);

      Set_Interrupt_Source (External_Interrupt_No (Pin), Port);

      Set_Trigger (Line    => Pin,
                   Raising => Enable,
                   Falling => Disable);
      Masking (Line  => Pin,
               State => Unmasked);
   end Initialize;

begin
   Initialize;
end Discovery_Board.Button;
