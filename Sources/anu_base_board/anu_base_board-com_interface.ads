--
-- Uwe R. Zimmer, Australia 2015
--

with System;                       use System;
with STM32F4;                      use STM32F4;
with STM32F4.General_purpose_IOs;  use STM32F4.General_purpose_IOs;

with Ada.Interrupts.Names;         use Ada.Interrupts.Names;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

package ANU_Base_Board.Com_Interface is

   pragma Elaborate_Body;

   procedure Set    (Port : Com_Ports);
   procedure Reset  (Port : Com_Ports);
   procedure Toggle (Port : Com_Ports);

   function Read (Port : Com_Ports) return Bit;

   New_Arrival : array (Com_Ports) of Suspension_Object;

   -- need to decrease concurency (1 inspector for all port instead of 1 for each) because:
   --   Coms_Ports 1 & 2 are triggering the same EXTI interrupt (EXTI9_5_Interrupt)
   --   => need to check pending register anyway (when scaled)
   protected Port_Inspector with Interrupt_Priority => Interrupt_Priority'Last is

      procedure Initialize_Interrupt;

   private

      procedure Invoking_Receiver;
      pragma Attach_Handler (Port_Inspector.Invoking_Receiver, EXTI9_5_Interrupt);
      pragma Unreferenced (Invoking_Receiver);

   end Port_Inspector;

end ANU_Base_Board.Com_Interface;
