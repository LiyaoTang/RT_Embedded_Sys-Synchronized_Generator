--
-- Uwe R. Zimmer, Australia 2015
--

with STM32F4.General_purpose_IOs; use STM32F4.General_purpose_IOs;

-- STM32F4xx Interrupts and Events Operations

package STM32F4.Interrupts_and_Events.Ops is

   type Intr_or_Event is (Interrupts, Events);

   procedure Set_Trigger     (Line : Lines; Raising, Falling : Enabler)                       with Inline;
   procedure Masking         (Line : Lines; State : Mask; What : Intr_or_Event := Interrupts) with Inline;
   procedure Generate        (Line : Lines)                                                   with Inline;
   function  Happened        (Line : Lines) return Boolean                                    with Inline;
   procedure Clear_Interrupt (Line : Lines)                                                   with Inline;

end STM32F4.Interrupts_and_Events.Ops;
