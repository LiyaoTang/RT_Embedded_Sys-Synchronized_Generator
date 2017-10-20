package body STM32F4.ADC.Ops is

   procedure Configure_Common_Properties
     (Mode           : MULTI_Options;
      Prescaler      : ADCPRE_Options;
      DMA_Mode       : DMA_Options;
      Sampling_Delay : Bits_4) is

   begin
      ADC123.CCR.MULTI    := Mode;
      ADC123.CCR.DELAYADC := Sampling_Delay;
      ADC123.CCR.DMA      := DMA_Mode;
      ADC123.CCR.ADCPRE   := Prescaler;
   end Configure_Common_Properties;

   procedure Multi_Enable_DMA_After_Last_Transfer is

   begin
      ADC123.CCR.DMA := Mode_1;
   end Multi_Enable_DMA_After_Last_Transfer;

   procedure Multi_Disable_DMA_After_Last_Transfer is

   begin
      ADC123.CCR.DMA := Disabled;
   end Multi_Disable_DMA_After_Last_Transfer;

   function Multi_DMA_Enabled_After_Last_Transfer return Boolean is (ADC123.CCR.DMA = Mode_1);

   procedure Enable (No : ADC_No) is

   begin
      ADC123.ADC (No).CR2.ADON := Enable;
      delay until Clock + ADC_Stabilization;
   end Enable;

   procedure Disable (No : ADC_No) is

   begin
      ADC123.ADC (No).CR2.ADON := Disable;
   end Disable;

   function Enabled (No : ADC_No) return Boolean is (ADC123.ADC (No).CR2.ADON = Enable);

   procedure Configure_ADC (No : ADC_No; Resolution : RES_Options; Alignment : ALIGN_Options := Right) is

   begin
      ADC123.ADC (No).CR1.RES   := Resolution;
      ADC123.ADC (No).CR2.ALIGN := Alignment;
   end Configure_ADC;

   function Current_Resolution (No : ADC_No) return RES_Options   is (ADC123.ADC (No).CR1.RES);
   function Current_Alignment  (No : ADC_No) return ALIGN_Options is (ADC123.ADC (No).CR2.ALIGN);

   procedure Set_Sampling_Time
     (No          : ADC_No;
      Channel     : AWDCH_Range;
      Sample_Time : SMP_Options) is

   begin
      if Channel > 9 then
         ADC123.ADC (No).SMPR1 (Channel) := Sample_Time;
      else
         ADC123.ADC (No).SMPR2 (Channel) := Sample_Time;
      end if;
   end Set_Sampling_Time;

   procedure Set_Sequence_Position
     (No      : ADC_No;
      Channel : AWDCH_Range;
      Rank    : Regular_Channel_Rank) is

   begin
      case Rank is
         when 1 .. 6 =>
            ADC123.ADC (No).SQR3.SQ (Rank) := Channel;
         when 7 .. 12 =>
            ADC123.ADC (No).SQR2.SQ (Rank) := Channel;
         when 13 .. 16 =>
            ADC123.ADC (No).SQR1.SQ (Rank) := Channel;
      end case;
   end Set_Sequence_Position;

   procedure Set_Injected_Channel_Sequence_Position
     (No      : ADC_No;
      Channel : AWDCH_Range;
      Rank    : Injected_Channel_Rank) is

   begin
      ADC123.ADC (No).JSQR.JSQ (Rank) := Channel;
   end Set_Injected_Channel_Sequence_Position;

   procedure Set_Injected_Channel_Offset
     (No     : ADC_No;
      Rank   : Injected_Channel_Rank;
      Offset : Injected_Data_Offset) is

   begin
      ADC123.ADC (No).JOFR (Rank).JOFFSETx := Offset;
   end Set_Injected_Channel_Offset;

   procedure Configure_Regular_Channel
     (No          : ADC_No;
      Channel     : AWDCH_Range;
      Rank        : Regular_Channel_Rank;
      Sample_Time : SMP_Options) is

   begin
      Set_Sampling_Time (No, Channel, Sample_Time);
      Set_Sequence_Position (No, Channel, Rank);
   end Configure_Regular_Channel;

   procedure Configure_Injected_Channel
     (No          : ADC_No;
      Channel     : AWDCH_Range;
      Rank        : Injected_Channel_Rank;
      Sample_Time : SMP_Options;
      Offset      : Injected_Data_Offset) is

   begin
      Set_Sampling_Time (No, Channel, Sample_Time);
      Set_Injected_Channel_Sequence_Position (No, Channel, Rank);
      Set_Injected_Channel_Offset (No, Rank, Offset);
   end Configure_Injected_Channel;

   function VBat_Conversion                   (No : ADC_No; Channel : AWDCH_Range) return Boolean is (No = 1 and then Channel = VBat_Channel);
   function VRef_TemperatureSensor_Conversion (No : ADC_No; Channel : AWDCH_Range) return Boolean is (No = 1 and then Channel = VRef_Channel);

   procedure Configure_Regular_Conversions
     (No             : ADC_No;
      Reg_Continuous : CONT_Options;
      Enable_EOC     : Enabler;                      -- Enable to cause an end-of-conversion flag after each conversion (instead of after each sequence)
      Conversions    : Regular_Channel_Conversions;  -- The sequence of conversions for this ADC; at least one conversion required
      Reg_Trigger    : Regular_Channel_Conversion_Trigger := Software_Triggered) is
      Total_Regular_Conversions : Bits_5 := 0;

   begin
      ADC123.ADC (No).CR2.CONT  := Reg_Continuous;
      ADC123.ADC (No).CR2.EOCS  := Enable_EOC;
      ADC123.ADC (No).CR1.SCAN  := (if Conversions'Length > 0 then Enable else Disable);
      ADC123.ADC (No).CR2.EXTEN := Reg_Trigger.Trigger_Enabler;

      if Reg_Trigger.Trigger_Enabler /= Disabled then
         ADC123.ADC (No).CR2.EXTSEL := Reg_Trigger.Event;
      end if;

      for Rank in Conversions'Range loop
         declare
            Conversion : Regular_Channel_Conversion renames Conversions (Rank);
         begin
            Configure_Regular_Channel (No, Conversion.Channel, Rank, Conversion.Sample_Time);

            if VBat_Conversion (No, Conversion.Channel) then
               Enable_VBat_Connection;
            elsif VRef_TemperatureSensor_Conversion (No, Conversion.Channel) then
               Enable_VRef_TemperatureSensor_Conversion;
            end if;

            Total_Regular_Conversions := Bits_5'Succ (Total_Regular_Conversions);
         end;
      end loop;

      ADC123.ADC (No).SQR1.L := Bits_4 (Total_Regular_Conversions - 1);
   end Configure_Regular_Conversions;

   procedure Configure_Injected_Conversions
     (No            : ADC_No;
      AutoInjection : Enabler;
      Enable_EOC    : Enabler;                       -- Enable to cause an end-of-conversion flag after each conversion (instead of after each sequence)
      Conversions   : Injected_Channel_Conversions;  -- The sequence of injected conversions for this ADC; at least one conversion required
      Inj_Trigger   : Injected_Channel_Conversion_Trigger := Software_Triggered_Injected) is

      Total_Injected_Conversions : Bits_5 := 0;

   begin
      ADC123.ADC (No).CR2.EOCS   := Enable_EOC;
      ADC123.ADC (No).CR1.JAUTO  := AutoInjection;
      ADC123.ADC (No).CR2.JEXTEN := Inj_Trigger.Trigger_Enabler;

      if Inj_Trigger.Trigger_Enabler /= Disabled then
         ADC123.ADC (No).CR2.JEXTSEL := Inj_Trigger.Event;
      end if;

      for Rank in Conversions'Range loop
         declare
            Conversion : Injected_Channel_Conversion renames Conversions (Rank);
         begin
            Configure_Injected_Channel (No, Conversion.Channel, Rank, Conversion.Sample_Time, Conversion.Offset);

            if VBat_Conversion (No, Conversion.Channel) then
               Enable_VBat_Connection;
            elsif VRef_TemperatureSensor_Conversion (No, Conversion.Channel) then
               Enable_VRef_TemperatureSensor_Conversion;
            end if;

            Total_Injected_Conversions := Bits_5'Succ (Total_Injected_Conversions);
         end;
      end loop;

      ADC123.ADC (No).JSQR.L := Bits_2 (Total_Injected_Conversions - 1);
   end Configure_Injected_Conversions;

   function Regular_Conversions_Expected  (No : ADC_No) return Bits_4 is (ADC123.ADC (No).SQR1.L + 1);
   function Injected_Conversions_Expected (No : ADC_No) return Bits_2 is (ADC123.ADC (No).JSQR.L + 1);

   function Scan_Mode_Enabled     (No : ADC_No) return Enabler is (ADC123.ADC (No).CR1.SCAN);
   function EOC_Selection_Enabled (No : ADC_No) return Enabler is (ADC123.ADC (No).CR2.EOCS);

   procedure Disable_Discontinuous_Mode_Regular (No : ADC_No) is

   begin
     ADC123.ADC (No).CR1.DISCEN := Disable;
   end Disable_Discontinuous_Mode_Regular;

   procedure Disable_Discontinuous_Mode_Injected (No : ADC_No) is

   begin
      ADC123.ADC (No).CR1.JDISCEN := Disable;
   end Disable_Discontinuous_Mode_Injected;

   function Discontinuous_Mode_Regular_Enabled  (No : ADC_No) return Enabler is (ADC123.ADC (No).CR1.DISCEN);
   function Discontinuous_Mode_Injected_Enabled (No : ADC_No) return Enabler is (ADC123.ADC (No).CR1.JDISCEN);
   function AutoInjection_Enabled (No : ADC_No) return Enabler is (ADC123.ADC (No).CR1.JAUTO);

   procedure Enable_VBat_Connection is

   begin
      ADC123.CCR.VBATE := Enable;
   end Enable_VBat_Connection;

   procedure Enable_VRef_TemperatureSensor_Conversion is

   begin
      ADC123.CCR.TSVREFE := Enable;
      delay until Clock + Temperature_Sensor_Stabilization;
   end Enable_VRef_TemperatureSensor_Conversion;

   function VBat_Enabled                   return Enabler is (ADC123.CCR.VBATE);
   function VRef_TemperatureSensor_Enabled return Enabler is (ADC123.CCR.TSVREFE);

   procedure Start_Conversion (No : ADC_No) is

   begin
      if ADC123.ADC (No).CR2.EXTEN = Disabled and then (ADC123.CCR.MULTI = Independent or else No = 1) then
         ADC123.ADC (No).CR2.SWSTART := Enable;
      end if;
   end Start_Conversion;

   procedure Start_Injected_Conversion (No : ADC_No) is

   begin
     ADC123.ADC (No).CR2.JSWSTART := Enable;
   end Start_Injected_Conversion;

   function Flag (No : ADC_No; Probed_Flag : ADC_Flags) return Boolean is

     (case Probed_Flag is
         when Analog_Watchdog_Event_Occurred       => ADC123.ADC (No).SR.AWD   = Occured,
         when Regular_Channel_Conversion_Complete  => ADC123.ADC (No).SR.EOC   = Complete,
         when Injected_Channel_Conversion_Complete => ADC123.ADC (No).SR.JEOC  = Complete,
         when Injected_Channel_Conversion_Started  => ADC123.ADC (No).SR.JSTRT = Started,
         when Regular_Channel_Conversion_Started   => ADC123.ADC (No).SR.STRT  = Started,
         when Overrun                              => ADC123.ADC (No).SR.OVR   = Occured);

   procedure Clear_Flag (No : ADC_No; Flag_to_Clear : ADC_Flags) is

   begin
      case Flag_to_Clear is
         when Analog_Watchdog_Event_Occurred       => ADC123.ADC (No).SR.AWD   := Not_Occured;
         when Regular_Channel_Conversion_Complete  => ADC123.ADC (No).SR.EOC   := Not_Complete;
         when Injected_Channel_Conversion_Complete => ADC123.ADC (No).SR.JEOC  := Not_Complete;
         when Injected_Channel_Conversion_Started  => ADC123.ADC (No).SR.JSTRT := Not_Started;
         when Regular_Channel_Conversion_Started   => ADC123.ADC (No).SR.STRT  := Not_Started;
         when Overrun                              => ADC123.ADC (No).SR.OVR   := Not_Occured;
      end case;
   end Clear_Flag;

   function Flag_is_set_before_Timeout (No : ADC_No; Probed_Flag : ADC_Flags; Timeout : Time_Span := Time_Span_Last) return Boolean is

      Deadline : constant Time := Clock + Timeout;

   begin
      while Clock < Deadline loop
         if Flag (No, Probed_Flag) then
            return True;
         end if;
      end loop;
      return False;
   end Flag_is_set_before_Timeout;

   function Conversion_Value          (No : ADC_No)                               return Bits_16 is (ADC123.ADC (No).DR);
   function Injected_Conversion_Value (No : ADC_No; Rank : Injected_Channel_Rank) return Bits_16 is (ADC123.ADC (No).JDR (Rank).JDATAx);

   function Data_Register_Address (No : ADC_No) return System.Address is (ADC123.ADC (No).DR'Address);

   procedure Enable_DMA (No : ADC_No) is

   begin
     ADC123.ADC (No).CR2.DMA := Enable;
   end Enable_DMA;

   procedure Disable_DMA (No : ADC_No) is

   begin
     ADC123.ADC (No).CR2.DMA := Disable;
   end Disable_DMA;

   function DMA_Enabled (No : ADC_No) return Enabler is (ADC123.ADC (No).CR2.DMA);

   procedure Enable_DMA_After_Last_Transfer (No : ADC_No) is

   begin
      ADC123.ADC (No).CR2.DDS := Enable;
   end Enable_DMA_After_Last_Transfer;

   procedure Disable_DMA_After_Last_Transfer (No : ADC_No) is

   begin
      ADC123.ADC (No).CR2.DDS := Disable;
   end Disable_DMA_After_Last_Transfer;

   function DMA_Enabled_After_Last_Transfer (No : ADC_No) return Enabler is (ADC123.ADC (No).CR2.DDS);

   procedure Enable_Interrupts (No : ADC_No; Source : ADC_Interrupts) is

   begin
      case Source is
         when Regular_Channel_Conversion_Complete  => ADC123.ADC (No).CR1.EOCIE  := Enable;
         when Injected_Channel_Conversion_Complete => ADC123.ADC (No).CR1.JEOCIE := Enable;
         when Analog_Watchdog_Event                => ADC123.ADC (No).CR1.AWDIE  := Enable;
         when Overrun                              => ADC123.ADC (No).CR1.OVRIE  := Enable;
      end case;
   end Enable_Interrupts;

   procedure Disable_Interrupts (No : ADC_No; Source : ADC_Interrupts) is

   begin
      case Source is
         when Regular_Channel_Conversion_Complete  => ADC123.ADC (No).CR1.EOCIE  := Disable;
         when Injected_Channel_Conversion_Complete => ADC123.ADC (No).CR1.JEOCIE := Disable;
         when Analog_Watchdog_Event                => ADC123.ADC (No).CR1.AWDIE  := Disable;
         when Overrun                              => ADC123.ADC (No).CR1.OVRIE  := Disable;
      end case;
   end Disable_Interrupts;

   function Interrupt_Enabled (No : ADC_No; Source : ADC_Interrupts) return Enabler is

     (case Source is
         when Regular_Channel_Conversion_Complete  => ADC123.ADC (No).CR1.EOCIE,
         when Injected_Channel_Conversion_Complete => ADC123.ADC (No).CR1.JEOCIE,
         when Analog_Watchdog_Event                => ADC123.ADC (No).CR1.AWDIE,
         when Overrun                              => ADC123.ADC (No).CR1.OVRIE);

   procedure Clear_Interrupt_Pending (No : ADC_No; Source : ADC_Interrupts) is

   begin
      case Source is
         when Regular_Channel_Conversion_Complete  => ADC123.ADC (No).SR.EOC  := Not_Complete;
         when Injected_Channel_Conversion_Complete => ADC123.ADC (No).SR.JEOC := Not_Complete;
         when Analog_Watchdog_Event                => ADC123.ADC (No).SR.AWD  := Not_Occured;
         when Overrun                              => ADC123.ADC (No).SR.OVR  := Not_Occured;
      end case;
   end Clear_Interrupt_Pending;

   procedure Watchdog_Enable_Channels (No : ADC_No;  Mode : Multiple_Channels_Watchdog; Low, High  : Bits_12) is

   begin
      ADC123.ADC (No).LTR := Low;
      ADC123.ADC (No).HTR := High;

      ADC123.ADC (No).CR1.AWDSGL := Disable;

      case Mode is
         when Watchdog_All_Regular_Channels  => ADC123.ADC (No).CR1.AWDEN  := Enable;
         when Watchdog_All_Injected_Channels => ADC123.ADC (No).CR1.JAWDEN := Enable;
         when Watchdog_All_Both_Kinds        => ADC123.ADC (No).CR1.AWDEN  := Enable; ADC123.ADC (No).CR1.JAWDEN := Enable;
      end case;
   end Watchdog_Enable_Channels;

   procedure Watchdog_Enable_Channel (No : ADC_No; Mode : Single_Channel_Watchdog; Channel : AWDCH_Range; Low, High  : Bits_12) is

   begin
      ADC123.ADC (No).LTR := Low;
      ADC123.ADC (No).HTR := High;

      ADC123.ADC (No).CR1.AWDSGL := Enable;
      ADC123.ADC (No).CR1.AWDCH  := Channel;

      case Mode is
         when Watchdog_Single_Regular_Channel  => ADC123.ADC (No).CR1.AWDEN  := Enable;
         when Watchdog_Single_Injected_Channel => ADC123.ADC (No).CR1.JAWDEN := Enable;
         when Watchdog_Single_Both_Kinds       => ADC123.ADC (No).CR1.AWDEN  := Enable; ADC123.ADC (No).CR1.JAWDEN := Enable;
      end case;
   end Watchdog_Enable_Channel;

   procedure Watchdog_Disable (No : ADC_No) is

   begin
      ADC123.ADC (No).CR1.AWDEN  := Disable;
      ADC123.ADC (No).CR1.JAWDEN := Disable;
      ADC123.ADC (No).CR1.AWDSGL := Disable;
   end Watchdog_Disable;

   function Watchdog_Enabled (No : ADC_No) return Boolean is (ADC123.ADC (No).CR1.AWDEN = Enable or else ADC123.ADC (No).CR1.JAWDEN = Enable);

end STM32F4.ADC.Ops;
