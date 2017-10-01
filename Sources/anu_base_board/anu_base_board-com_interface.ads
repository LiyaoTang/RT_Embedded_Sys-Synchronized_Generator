--
-- Uwe R. Zimmer, Australia 2015
--

with System;                      use System;
with STM32F4;                     use STM32F4;
with STM32F4.General_purpose_IOs; use STM32F4.General_purpose_IOs;
with Ada.Interrupts.Names;        use Ada.Interrupts.Names;

package ANU_Base_Board.Com_Interface is

   pragma Elaborate_Body;

   procedure Set    (Port : Com_Ports);
   procedure Reset  (Port : Com_Ports);
   procedure Toggle (Port : Com_Ports);

   function Read (Port : Com_Ports) return Bit;

   protected type Receiver is

      entry Read_Data (New_Data : out Bit);

      procedure Set_Data (New_Data : Bit);

   private

      Data : Bit;
      New_Arrival : Boolean := False;

   end Receiver;

   Receivers : array (Com_Ports) of Receiver;

   type New_Data_T is array (Com_Ports) of Bit;
   type New_Arrival_T is array (Com_Ports) of Boolean;

   -- need to decrease concurency (1 inspector for all port instead of 1 for each) because:
   --   Coms_Ports 1 & 2 are triggering the same EXTI interrupt (EXTI9_5_Interrupt)
   --   => need to check pending register anyway (when scaled)
   protected Port_Inspector with Interrupt_Priority => Interrupt_Priority'Last is

      entry Read_Data (Port_Num : Com_Ports; New_Data : out Bit; Is_Mine : out Boolean);

      procedure Initialize_Interrupt;

   private

      procedure Handling_Input; -- attach to all pins for Com_Ports input pins
      pragma Attach_Handler (Port_Inspector.Handling_Input, EXTI9_5_Interrupt);
      pragma Unreferenced (Handling_Input);

      procedure Invoking_Receiver;
--        pragma Attach_Handler (Port_Inspector.Invoking_Receiver, EXTI9_5_Interrupt);
      pragma Unreferenced (Invoking_Receiver);

      Data : New_Data_T;
      Arrival : New_Arrival_T := (others => False);
      Triggered : Boolean := False;

   end Port_Inspector;

end ANU_Base_Board.Com_Interface;
