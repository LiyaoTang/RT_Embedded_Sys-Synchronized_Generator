--
-- Uwe R. Zimmer, Australia 2015
--

package body STM32F4.System_configuration_controller.Ops is

   procedure Set_Interrupt_Source (Interrupt_No : External_Interrupt_No; Port : GPIO_Ports) is

   begin
      case Interrupt_No is
         when  0 => SYSCFG.EXTICR1.EXTI0  := Port;
         when  1 => SYSCFG.EXTICR1.EXTI1  := Port;
         when  2 => SYSCFG.EXTICR1.EXTI2  := Port;
         when  3 => SYSCFG.EXTICR1.EXTI3  := Port;
         when  4 => SYSCFG.EXTICR2.EXTI4  := Port;
         when  5 => SYSCFG.EXTICR2.EXTI5  := Port;
         when  6 => SYSCFG.EXTICR2.EXTI6  := Port;
         when  7 => SYSCFG.EXTICR2.EXTI7  := Port;
         when  8 => SYSCFG.EXTICR3.EXTI8  := Port;
         when  9 => SYSCFG.EXTICR3.EXTI9  := Port;
         when 10 => SYSCFG.EXTICR3.EXTI10 := Port;
         when 11 => SYSCFG.EXTICR3.EXTI11 := Port;
         when 12 => SYSCFG.EXTICR4.EXTI12 := Port;
         when 13 => SYSCFG.EXTICR4.EXTI13 := Port;
         when 14 => SYSCFG.EXTICR4.EXTI14 := Port;
         when 15 => SYSCFG.EXTICR4.EXTI15 := Port;
      end case;
   end Set_Interrupt_Source;

   function Read_Interrupt_Source (Interrupt_No : External_Interrupt_No) return GPIO_Ports is

     (case Interrupt_No is
         when  0 => SYSCFG.EXTICR1.EXTI0,
         when  1 => SYSCFG.EXTICR1.EXTI1,
         when  2 => SYSCFG.EXTICR1.EXTI2,
         when  3 => SYSCFG.EXTICR1.EXTI3,
         when  4 => SYSCFG.EXTICR2.EXTI4,
         when  5 => SYSCFG.EXTICR2.EXTI5,
         when  6 => SYSCFG.EXTICR2.EXTI6,
         when  7 => SYSCFG.EXTICR2.EXTI7,
         when  8 => SYSCFG.EXTICR3.EXTI8,
         when  9 => SYSCFG.EXTICR3.EXTI9,
         when 10 => SYSCFG.EXTICR3.EXTI10,
         when 11 => SYSCFG.EXTICR3.EXTI11,
         when 12 => SYSCFG.EXTICR4.EXTI12,
         when 13 => SYSCFG.EXTICR4.EXTI13,
         when 14 => SYSCFG.EXTICR4.EXTI14,
         when 15 => SYSCFG.EXTICR4.EXTI15);

end STM32F4.System_configuration_controller.Ops;
