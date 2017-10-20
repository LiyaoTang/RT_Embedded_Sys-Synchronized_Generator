--
-- Uwe R. Zimmer, Australia 2015
--

with STM32F4.Interrupts_and_Events; use STM32F4.Interrupts_and_Events;

-- STM32F405xx/07xx, STM32F415xx/17xx System configuration controller Operations

package STM32F4.System_configuration_controller.Ops is

   procedure Set_Interrupt_Source  (Interrupt_No : External_Interrupt_No; Port : GPIO_Ports);
   function  Read_Interrupt_Source (Interrupt_No : External_Interrupt_No) return GPIO_Ports;

end STM32F4.System_configuration_controller.Ops;
