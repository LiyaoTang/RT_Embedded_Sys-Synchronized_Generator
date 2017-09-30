--
-- Uwe R. Zimmer, Australia 2015
--

-- STM32F4xx Timers Operations

package body STM32F4.Timers.Ops is

   procedure Enable_Disable (No : Timer_No; State : Enabler) is

   begin
      case No is
         when  1 |  8           => TIM_1_8 (No).all.CR1.CEN         := State;
         when  2 .. 5           => TIM_2_to_5 (No).all.CR1.CEN      := State;
         when  6 |  7           => TIM_6_7 (No).all.CR1.CEN         := State;
         when  9 | 12           => TIM_9_12 (No).all.CR1.CEN        := State;
         when 10 | 11 | 13 | 14 => TIM_10_11_13_14 (No).all.CR1.CEN := State;
      end case;
   end Enable_Disable;

   procedure Enable  (No : Timer_No) is

   begin
      Enable_Disable (No, Enable);
   end Enable;

   procedure Disable (No : Timer_No) is

   begin
      Enable_Disable (No, Disable);
   end Disable;

   procedure Set_Prescaler (No : Timer_No; Prescaler : Bits_16) is

   begin
      case No is
         when  1 |  8           => TIM_1_8 (No).all.PSC         := Prescaler;
         when  2 .. 5           => TIM_2_to_5 (No).all.PSC      := Prescaler;
         when  6 |  7           => TIM_6_7 (No).all.PSC         := Prescaler;
         when  9 | 12           => TIM_9_12 (No).all.PSC        := Prescaler;
         when 10 | 11 | 13 | 14 => TIM_10_11_13_14 (No).all.PSC := Prescaler;
      end case;
   end Set_Prescaler;

   procedure Set_Auto_Reload_16 (No : Timer_No; Auto_Reload : Bits_16) is

   begin
      case No is
         when  1 |  8           => TIM_1_8 (No).all.ARR         := Auto_Reload;
         when  2 .. 5           => TIM_2_to_5 (No).all.ARR      := Bits_32 (Auto_Reload);
         when  6 |  7           => TIM_6_7 (No).all.ARR         := Auto_Reload;
         when  9 | 12           => TIM_9_12 (No).all.ARR        := Auto_Reload;
         when 10 | 11 | 13 | 14 => TIM_10_11_13_14 (No).all.ARR := Auto_Reload;
      end case;
   end Set_Auto_Reload_16;

   procedure Set_Auto_Reload_32 (No : Timer_No; Auto_Reload : Bits_32) is

   begin
      case No is
         when  2 | 5 => TIM_2_to_5 (No).all.ARR := Auto_Reload;
         when others => raise Incompatible_Timer;
      end case;
   end Set_Auto_Reload_32;

   procedure Enable_Disable (No : Timer_No; Int : Timer_Interrupts; State : Enabler) is

   begin
      case Int is
         when Update                 =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.DIER.UIE           := State;
               when  2 .. 5           => TIM_2_to_5 (No).all.DIER.UIE        := State;
               when  6 |  7           => TIM_6_7 (No).all.DIER.UIE           := State;
               when  9 | 12           => TIM_9_12 (No).all.DIER.UIE          := State;
               when 10 | 11 | 13 | 14 => TIM_10_11_13_14 (No).all.DIER.UIE   := State;
            end case;
         when Capture_Compare_1      =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.DIER.CC1IE         := State;
               when  2 .. 5           => TIM_2_to_5 (No).all.DIER.CC1IE      := State;
               when  9 | 12           => TIM_9_12 (No).all.DIER.CC1IE        := State;
               when 10 | 11 | 13 | 14 => TIM_10_11_13_14 (No).all.DIER.CC1IE := State;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_2      =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.DIER.CC2IE         := State;
               when  2 .. 5           => TIM_2_to_5 (No).all.DIER.CC2IE      := State;
               when  9 | 12           => TIM_9_12 (No).all.DIER.CC2IE        := State;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_3      =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.DIER.CC3IE         := State;
               when  2 .. 5           => TIM_2_to_5 (No).all.DIER.CC3IE      := State;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_4      =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.DIER.CC4IE         := State;
               when  2 .. 5           => TIM_2_to_5 (No).all.DIER.CC4IE      := State;
               when others            => raise Incompatible_Timer;
            end case;
         when COM                    =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.DIER.COMIE        := State;
               when others            => raise Incompatible_Timer;
            end case;
         when Trigger                =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.DIER.TIE           := State;
               when  2 .. 5           => TIM_2_to_5 (No).all.DIER.TIE        := State;
               when  9 | 12           => TIM_9_12 (No).all.DIER.TIE          := State;
               when others            => raise Incompatible_Timer;
            end case;
         when Break                  =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.DIER.BIE           := State;
               when others            => raise Incompatible_Timer;
            end case;
         when Update_DMA             =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.DIER.UDE           := State;
               when  2 .. 5           => TIM_2_to_5 (No).all.DIER.UDE        := State;
               when  6 |  7           => TIM_6_7 (No).all.DIER.UDE           := State;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_1_DMA  =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.DIER.CC1DE         := State;
               when  2 .. 5           => TIM_2_to_5 (No).all.DIER.CC1DE      := State;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_2_DMA  =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.DIER.CC2DE         := State;
               when  2 .. 5           => TIM_2_to_5 (No).all.DIER.CC2DE      := State;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_3_DMA  =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.DIER.CC3DE         := State;
               when  2 .. 5           => TIM_2_to_5 (No).all.DIER.CC3DE      := State;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_4_DMA  =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.DIER.CC4DE         := State;
               when  2 .. 5           => TIM_2_to_5 (No).all.DIER.CC4DE      := State;
               when others            => raise Incompatible_Timer;
            end case;
         when COM_DMA                =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.DIER.COMDE         := State;
               when others            => raise Incompatible_Timer;
            end case;
         when Trigger_DMA            =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.DIER.TDE           := State;
               when  2 .. 5           => TIM_2_to_5 (No).all.DIER.TDE        := State;
               when others            => raise Incompatible_Timer;
            end case;
      end case;
   end Enable_Disable;

   procedure Enable (No : Timer_No; Int : Timer_Interrupts) is

   begin
      Enable_Disable (No, Int, Enable);
   end Enable;

   procedure Disable (No : Timer_No; Int : Timer_Interrupts) is

   begin
      Enable_Disable (No, Int, Disable);
   end Disable;

   procedure Generate (No : Timer_No; This_Event : Timer_Event) is

   begin
      case This_Event is
         when Update                 =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.EGR.UG         := Generate;
               when  2 .. 5           => TIM_2_to_5 (No).all.EGR.UG      := Generate;
               when  6 |  7           => TIM_6_7 (No).all.EGR.UG         := Generate;
               when  9 | 12           => TIM_9_12 (No).all.EGR.UG        := Generate;
               when 10 | 11 | 13 | 14 => TIM_10_11_13_14 (No).all.EGR.UG := Generate;
            end case;
         when Capture_Compare_1      =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.EGR.CC1G       := Generate;
               when  2 .. 5           => TIM_2_to_5 (No).all.EGR.CC1G    := Generate;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_2      =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.EGR.CC2G       := Generate;
               when  2 .. 5           => TIM_2_to_5 (No).all.EGR.CC2G    := Generate;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_3      =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.EGR.CC3G       := Generate;
               when  2 .. 5           => TIM_2_to_5 (No).all.EGR.CC3G    := Generate;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_4      =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.EGR.CC4G       := Generate;
               when  2 .. 5           => TIM_2_to_5 (No).all.EGR.CC4G    := Generate;
               when others            => raise Incompatible_Timer;
            end case;
         when COM                    =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.EGR.COMG       := Generate;
               when others            => raise Incompatible_Timer;
            end case;
         when Trigger                =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.EGR.TG         := Generate;
               when  2 .. 5           => TIM_2_to_5 (No).all.EGR.TG      := Generate;
               when  9 | 12           => TIM_9_12 (No).all.EGR.TG        := Generate;
               when others            => raise Incompatible_Timer;
            end case;
         when Break                  =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.EGR.BG         := Generate;
               when others            => raise Incompatible_Timer;
            end case;
      end case;
   end Generate;

   function Flag (No : Timer_No; This_Flag : Timer_Flags) return Boolean is

   begin
      case This_Flag is
         when Update                 =>
            case No is
               when  1 |  8           => return TIM_1_8 (No).all.SR.UIF           = Occured;
               when  2 .. 5           => return TIM_2_to_5 (No).all.SR.UIF        = Occured;
               when  6 |  7           => return TIM_6_7 (No).all.SR.UIF           = Occured;
               when  9 | 12           => return TIM_9_12 (No).all.SR.UIF          = Occured;
               when 10 | 11 | 13 | 14 => return TIM_10_11_13_14 (No).all.SR.UIF   = Occured;
            end case;
         when Capture_Compare_1      =>
            case No is
               when  1 |  8           => return TIM_1_8 (No).all.SR.CC1IF         = Occured;
               when  2 .. 5           => return TIM_2_to_5 (No).all.SR.CC1IF      = Occured;
               when  9 | 12           => return TIM_9_12 (No).all.SR.CC1IF        = Occured;
               when 10 | 11 | 13 | 14 => return TIM_10_11_13_14 (No).all.SR.CC1IF = Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_2      =>
            case No is
               when  1 |  8           => return TIM_1_8 (No).all.SR.CC2IF         = Occured;
               when  2 .. 5           => return TIM_2_to_5 (No).all.SR.CC2IF      = Occured;
               when  9 | 12           => return TIM_9_12 (No).all.SR.CC2IF        = Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_3      =>
            case No is
               when  1 |  8           => return TIM_1_8 (No).all.SR.CC3IF         = Occured;
               when  2 .. 5           => return TIM_2_to_5 (No).all.SR.CC3IF      = Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_4      =>
            case No is
               when  1 |  8           => return TIM_1_8 (No).all.SR.CC4IF         = Occured;
               when  2 .. 5           => return TIM_2_to_5 (No).all.SR.CC4IF      = Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when COM                    =>
            case No is
               when  1 |  8           => return TIM_1_8 (No).all.SR.COMIF         = Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Trigger                =>
            case No is
               when  1 |  8           => return TIM_1_8 (No).all.SR.TIF           = Occured;
               when  2 .. 5           => return TIM_2_to_5 (No).all.SR.TIF        = Occured;
               when  9 | 12           => return TIM_9_12 (No).all.SR.TIF          = Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Break                  =>
            case No is
               when  1 |  8           => return TIM_1_8 (No).all.SR.BIF           = Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_1_Over =>
            case No is
               when  1 |  8           => return TIM_1_8 (No).all.SR.CC10F         = Occured;
               when  2 .. 5           => return TIM_2_to_5 (No).all.SR.CC10F      = Occured;
               when  9 | 12           => return TIM_9_12 (No).all.SR.CC10F        = Occured;
               when 10 | 11 | 13 | 14 => return TIM_10_11_13_14 (No).all.SR.CC10F = Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_2_Over =>
            case No is
               when  1 |  8           => return TIM_1_8 (No).all.SR.CC20F         = Occured;
               when  2 .. 5           => return TIM_2_to_5 (No).all.SR.CC20F      = Occured;
               when  9 | 12           => return TIM_9_12 (No).all.SR.CC20F        = Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_3_Over =>
            case No is
               when  1 |  8           => return TIM_1_8 (No).all.SR.CC30F         = Occured;
               when  2 .. 5           => return TIM_2_to_5 (No).all.SR.CC30F      = Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_4_Over =>
            case No is
               when  1 |  8           => return TIM_1_8 (No).all.SR.CC40F         = Occured;
               when  2 .. 5           => return TIM_2_to_5 (No).all.SR.CC40F      = Occured;
               when others            => raise Incompatible_Timer;
            end case;
      end case;
   end Flag;

   procedure Clear_Flag (No : Timer_No; This_Flag : Timer_Flags) is

   begin
      case This_Flag is
         when Update                 =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.SR.UIF           := Not_Occured;
               when  2 .. 5           => TIM_2_to_5 (No).all.SR.UIF        := Not_Occured;
               when  6 |  7           => TIM_6_7 (No).all.SR.UIF           := Not_Occured;
               when  9 | 12           => TIM_9_12 (No).all.SR.UIF          := Not_Occured;
               when 10 | 11 | 13 | 14 => TIM_10_11_13_14 (No).all.SR.UIF   := Not_Occured;
            end case;
         when Capture_Compare_1      =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.SR.CC1IF         := Not_Occured;
               when  2 .. 5           => TIM_2_to_5 (No).all.SR.CC1IF      := Not_Occured;
               when  9 | 12           => TIM_9_12 (No).all.SR.CC1IF        := Not_Occured;
               when 10 | 11 | 13 | 14 => TIM_10_11_13_14 (No).all.SR.CC1IF := Not_Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_2      =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.SR.CC2IF         := Not_Occured;
               when  2 .. 5           => TIM_2_to_5 (No).all.SR.CC2IF      := Not_Occured;
               when  9 | 12           => TIM_9_12 (No).all.SR.CC2IF        := Not_Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_3      =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.SR.CC3IF         := Not_Occured;
               when  2 .. 5           => TIM_2_to_5 (No).all.SR.CC3IF      := Not_Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_4      =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.SR.CC4IF         := Not_Occured;
               when  2 .. 5           => TIM_2_to_5 (No).all.SR.CC4IF      := Not_Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when COM                    =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.SR.COMIF         := Not_Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Trigger                =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.SR.TIF           := Not_Occured;
               when  2 .. 5           => TIM_2_to_5 (No).all.SR.TIF        := Not_Occured;
               when  9 | 12           => TIM_9_12 (No).all.SR.TIF          := Not_Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Break                  =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.SR.BIF           := Not_Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_1_Over =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.SR.CC10F         := Not_Occured;
               when  2 .. 5           => TIM_2_to_5 (No).all.SR.CC10F      := Not_Occured;
               when  9 | 12           => TIM_9_12 (No).all.SR.CC10F        := Not_Occured;
               when 10 | 11 | 13 | 14 => TIM_10_11_13_14 (No).all.SR.CC10F := Not_Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_2_Over =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.SR.CC20F         := Not_Occured;
               when  2 .. 5           => TIM_2_to_5 (No).all.SR.CC20F      := Not_Occured;
               when  9 | 12           => TIM_9_12 (No).all.SR.CC20F        := Not_Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_3_Over =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.SR.CC30F         := Not_Occured;
               when  2 .. 5           => TIM_2_to_5 (No).all.SR.CC30F      := Not_Occured;
               when others            => raise Incompatible_Timer;
            end case;
         when Capture_Compare_4_Over =>
            case No is
               when  1 |  8           => TIM_1_8 (No).all.SR.CC40F         := Not_Occured;
               when  2 .. 5           => TIM_2_to_5 (No).all.SR.CC40F      := Not_Occured;
               when others            => raise Incompatible_Timer;
            end case;
      end case;
   end Clear_Flag;

end STM32F4.Timers.Ops;
