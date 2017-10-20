--
-- Uwe R. Zimmer, Australia 2015
--

-- STM32F4xx Timers Operations

package STM32F4.Timers.Ops is

   procedure Enable  (No : Timer_No) with Inline; -- All timers
   procedure Disable (No : Timer_No) with Inline; -- All timers

   procedure Set_Prescaler      (No : Timer_No; Prescaler   : Bits_16) with Inline; -- All timers
   procedure Set_Auto_Reload_16 (No : Timer_No; Auto_Reload : Bits_16) with Inline; -- All timers
   procedure Set_Auto_Reload_32 (No : Timer_No; Auto_Reload : Bits_32) with Inline; -- Timer 2, 5 only

   type Timer_Interrupts is (Update,                -- All timers
                             Capture_Compare_1,     -- Timer 1, 2 .. 5, 8 .. 14 only
                             Capture_Compare_2,     -- Timer 1, 2 .. 5, 8, 9, 12 only
                             Capture_Compare_3,     -- Timer 1, 2 .. 5, 8 only
                             Capture_Compare_4,     -- Timer 1, 2 .. 5, 8 only
                             COM,                   -- Timer 1, 8 only
                             Trigger,               -- Timer 1, 2 .. 5, 8, 9, 12 only
                             Break,                 -- Timer 1, 8 only
                             Update_DMA,            -- Timer 1, 2 .. 8 only
                             Capture_Compare_1_DMA, -- Timer 1, 2 .. 5, 8 only
                             Capture_Compare_2_DMA, -- Timer 1, 2 .. 5, 8 only
                             Capture_Compare_3_DMA, -- Timer 1, 2 .. 5, 8 only
                             Capture_Compare_4_DMA, -- Timer 1, 2 .. 5, 8 only
                             COM_DMA,               -- Timer 1, 8 only
                             Trigger_DMA);          -- Timer 1, 2 .. 5, 8 only

   procedure Enable  (No : Timer_No; Int : Timer_Interrupts) with Inline;
   procedure Disable (No : Timer_No; Int : Timer_Interrupts) with Inline;

   type Timer_Event is (Update,                -- All timers
                        Capture_Compare_1,     -- Timer 1, 2 .. 5, 8 .. 14 only
                        Capture_Compare_2,     -- Timer 1, 2 .. 5, 8, 9, 12 only
                        Capture_Compare_3,     -- Timer 1, 2 .. 5, 8 only
                        Capture_Compare_4,     -- Timer 1, 2 .. 5, 8 only
                        COM,                   -- Timer 1, 8 only
                        Trigger,               -- Timer 1, 2 .. 5, 8, 9, 12 only
                        Break);                -- Timer 1, 8 only

   procedure Generate (No : Timer_No; This_Event : Timer_Event) with Inline;

   type Timer_Flags is (Update,                  -- All timers
                        Capture_Compare_1,       -- Timer 1, 2 .. 5, 8 .. 14 only
                        Capture_Compare_2,       -- Timer 1, 2 .. 5, 8, 9, 12 only
                        Capture_Compare_3,       -- Timer 1, 2 .. 5, 8 only
                        Capture_Compare_4,       -- Timer 1, 2 .. 5, 8 only
                        COM,                     -- Timer 1, 8 only
                        Trigger,                 -- Timer 1, 2 .. 5, 8, 9, 12 only
                        Break,                   -- Timer 1, 8 only
                        Capture_Compare_1_Over,  -- Timer 1, 2 .. 5, 8 .. 14 only
                        Capture_Compare_2_Over,  -- Timer 1, 2 .. 5, 8, 9, 12 only
                        Capture_Compare_3_Over,  -- Timer 1, 2 .. 5, 8 only
                        Capture_Compare_4_Over); -- Timer 1, 2 .. 5, 8 only

   function  Flag       (No : Timer_No; This_Flag : Timer_Flags) return Boolean with Inline;
   procedure Clear_Flag (No : Timer_No; This_Flag : Timer_Flags)                with Inline;

   Incompatible_Timer : exception;

end STM32F4.Timers.Ops;
