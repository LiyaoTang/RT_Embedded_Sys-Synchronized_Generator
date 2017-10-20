--
-- Uwe R. Zimmer, Australia 2015
--

package body STM32F4.Interrupts_and_Events.Ops is

   procedure Set_Trigger (Line : Lines; Raising, Falling : Enabler) is

   begin
      EXTI.RTSR (Line) := Raising; -- Rising trigger
      EXTI.FTSR (Line) := Falling; -- Falling trigger
   end Set_Trigger;

   procedure Masking (Line : Lines; State : Mask; What : Intr_or_Event := Interrupts) is

   begin
      case What is
         when Interrupts => EXTI.IMR (Line) := State;
         when Events     => EXTI.EMR (Line) := State;
      end case;
   end Masking;

   procedure Generate (Line : Lines) is

   begin
      EXTI.SWIER (Line) := Trigger;
   end Generate;

   function Happened (Line : Lines) return Boolean is

      (EXTI.PR (Line) = Occured_Clear);

   procedure Clear_Interrupt (Line : Lines) is

   begin
      EXTI.PR (Line) := Occured_Clear;
   end Clear_Interrupt;

end STM32F4.Interrupts_and_Events.Ops;
