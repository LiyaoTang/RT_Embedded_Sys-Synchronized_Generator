--
-- Uwe R. Zimmer, Australia 2016
--

-- STM32F4xx Analog to digital converter Operations

with Ada.Real_Time; use Ada.Real_Time;

package STM32F4.ADC.Ops is

   -- Common properties

   procedure Configure_Common_Properties
     (Mode           : MULTI_Options;    -- Controls the timing/synchronization of parallel conversions.
      Prescaler      : ADCPRE_Options;   -- Determines the common, maximal sampling frequency; common clock for all ADC operations.
      DMA_Mode       : DMA_Options;      -- Determines how results are grouped to words and transferred.
      Sampling_Delay : Bits_4);          -- Sets the sampling window to 5 + Samples_Delay value ADC clock cycles.

   procedure Multi_Enable_DMA_After_Last_Transfer  with Post =>     Multi_DMA_Enabled_After_Last_Transfer, Inline;
   procedure Multi_Disable_DMA_After_Last_Transfer with Post => not Multi_DMA_Enabled_After_Last_Transfer, Inline;

   function Multi_DMA_Enabled_After_Last_Transfer return Boolean with Inline;

   -- Enable spcific ADCs

   procedure Enable  (No : ADC_No) with Post =>     Enabled (No);         -- Involves a warming up time of 3 micro-s (ADC_Stabilization)
   procedure Disable (No : ADC_No) with Post => not Enabled (No), Inline; -- Powers the ADC off instantaneously

   function Enabled (No : ADC_No) return Boolean with Inline;

   -- Configure individual ADCs

   procedure Configure_ADC (No : ADC_No; Resolution : RES_Options; Alignment  : ALIGN_Options := Right) with
     Post => Current_Resolution (No) = Resolution and then Current_Alignment (No) = Alignment,
     Inline;

   function Current_Resolution (No : ADC_No) return RES_Options   with Inline;
   function Current_Alignment  (No : ADC_No) return ALIGN_Options with Inline;

   -- Configure Conversion (sequence) - setting required for any ADC conversion

   type Regular_Channel_Conversion_Trigger (Trigger_Enabler : EXTEN_Options) is
      record
         case Trigger_Enabler is
            when Disabled => null;
            when others   => Event : EXTSEL_Options;
         end case;
      end record;

   type Injected_Channel_Conversion_Trigger (Trigger_Enabler : EXTEN_Options) is
      record
         case Trigger_Enabler is
            when Disabled => null;
            when others   => Event : JEXTSEL_Options;
         end case;
      end record;

   Software_Triggered          : constant Regular_Channel_Conversion_Trigger  := (Trigger_Enabler => Disabled);
   Software_Triggered_Injected : constant Injected_Channel_Conversion_Trigger := (Trigger_Enabler => Disabled);

   type Regular_Channel_Conversion is record
      Channel     : AWDCH_Range;
      Sample_Time : SMP_Options;
   end record;

   subtype Injected_Data_Offset is Bits_12;

   type Injected_Channel_Conversion is record
      Channel     : AWDCH_Range;
      Sample_Time : SMP_Options;
      Offset      : Injected_Data_Offset := 0;
   end record;

   type Injected_Channel_Conversions is array (Injected_Channel_Rank range <>) of Injected_Channel_Conversion;
   type Regular_Channel_Conversions  is array (Regular_Channel_Rank  range <>) of Regular_Channel_Conversion;

   procedure Configure_Regular_Conversions
     (No             : ADC_No;
      Reg_Continuous : CONT_Options;                 -- Determines whether conversions stop after one sequence or repeat infinitely.
      Enable_EOC     : Enabler;                      -- Enable to cause an end-of-conversion flag after each conversion (instead of after each sequence)
      Conversions    : Regular_Channel_Conversions;  -- The sequence of conversions for this ADC; at least one conversion required
      Reg_Trigger    : Regular_Channel_Conversion_Trigger := Software_Triggered) with
     Pre =>
       Conversions'Length > 0;

   procedure Configure_Injected_Conversions
     (No            : ADC_No;
      AutoInjection : Enabler;
      Enable_EOC    : Enabler;                       -- Enable to cause an end-of-conversion flag after each conversion (instead of after each sequence)
      Conversions   : Injected_Channel_Conversions;  -- The sequence of injected conversions for this ADC; at least one conversion required
      Inj_Trigger   : Injected_Channel_Conversion_Trigger := Software_Triggered_Injected) with
     Pre =>
       Conversions'Length > 0 and then
       (if AutoInjection = Enable then Inj_Trigger = Software_Triggered_Injected) and then (if AutoInjection = Enable then
                                                                                               Discontinuous_Mode_Injected_Enabled (No) = Disable);

   function Regular_Conversions_Expected  (No : ADC_No) return Bits_4 with Inline;
   function Injected_Conversions_Expected (No : ADC_No) return Bits_2 with Inline;

   function Scan_Mode_Enabled            (No : ADC_No) return Enabler with Inline;
   function EOC_Selection_Enabled        (No : ADC_No) return Enabler with Inline;

   procedure Disable_Discontinuous_Mode_Regular  (No : ADC_No) with Post => Discontinuous_Mode_Regular_Enabled  (No) = Disable, Inline;
   procedure Disable_Discontinuous_Mode_Injected (No : ADC_No) with Post => Discontinuous_Mode_Injected_Enabled (No) = Disable, Inline;

   function Discontinuous_Mode_Regular_Enabled  (No : ADC_No) return Enabler with Inline;
   function Discontinuous_Mode_Injected_Enabled (No : ADC_No) return Enabler with Inline;

   function AutoInjection_Enabled (No : ADC_No) return Enabler with Inline;

   procedure Enable_VBat_Connection                   with Post => VBat_Enabled = Enable;
   procedure Enable_VRef_TemperatureSensor_Conversion with Post => VRef_TemperatureSensor_Enabled = Enable;

   function VBat_Enabled                   return Enabler with Inline;
   function VRef_TemperatureSensor_Enabled return Enabler with Inline;

   -- Start conversions by software

   procedure Start_Conversion          (No : ADC_No) with Pre => Regular_Conversions_Expected  (No) > 0;
   procedure Start_Injected_Conversion (No : ADC_No) with Pre => Injected_Conversions_Expected (No) > 0;

   -- Check and clear ADC status

   type ADC_Flags is
     (Analog_Watchdog_Event_Occurred,
      Regular_Channel_Conversion_Complete,
      Injected_Channel_Conversion_Complete,
      Injected_Channel_Conversion_Started,
      Regular_Channel_Conversion_Started,
      Overrun);

   function  Flag       (No : ADC_No; Probed_Flag   : ADC_Flags) return Boolean with Inline;
   procedure Clear_Flag (No : ADC_No; Flag_to_Clear : ADC_Flags)                with Inline;

   function Flag_is_set_before_Timeout (No : ADC_No; Probed_Flag : ADC_Flags; Timeout : Time_Span := Time_Span_Last) return Boolean;

   -- Read results

   function Conversion_Value          (No : ADC_No)                               return Bits_16 with Inline;
   function Injected_Conversion_Value (No : ADC_No; Rank : Injected_Channel_Rank) return Bits_16 with Inline;

   function Data_Register_Address (No : ADC_No) return System.Address with Inline; -- Used to hand over a peripheral address to the DMA module

   -- DMA configuration

   procedure Enable_DMA  (No : ADC_No) with Post => DMA_Enabled (No) = Enable,  Inline;
   procedure Disable_DMA (No : ADC_No) with Post => DMA_Enabled (No) = Disable, Inline;

   function DMA_Enabled (No : ADC_No) return Enabler with Inline;

   procedure Enable_DMA_After_Last_Transfer  (No : ADC_No) with Post => DMA_Enabled_After_Last_Transfer (No) = Enable, Inline;
   procedure Disable_DMA_After_Last_Transfer (No : ADC_No) with Post => DMA_Enabled_After_Last_Transfer (No) = Disable, Inline;

   function DMA_Enabled_After_Last_Transfer (No : ADC_No) return Enabler with Inline;

   -- Interrupt configuration

   type ADC_Interrupts is
     (Regular_Channel_Conversion_Complete,
      Injected_Channel_Conversion_Complete,
      Analog_Watchdog_Event,
      Overrun);

   procedure Enable_Interrupts  (No : ADC_No; Source : ADC_Interrupts) with Post => Interrupt_Enabled (No, Source) = Enable, Inline;
   procedure Disable_Interrupts (No : ADC_No; Source : ADC_Interrupts) with Post => Interrupt_Enabled (No, Source) = Disable, Inline;

   function Interrupt_Enabled (No : ADC_No; Source : ADC_Interrupts) return Enabler with Inline;

   procedure Clear_Interrupt_Pending (No : ADC_No; Source : ADC_Interrupts) with Inline;

   -- Watchdog configuration

   type Analog_Watchdog_Modes is
     (Watchdog_All_Regular_Channels,
      Watchdog_All_Injected_Channels,
      Watchdog_All_Both_Kinds,
      Watchdog_Single_Regular_Channel,
      Watchdog_Single_Injected_Channel,
      Watchdog_Single_Both_Kinds);

   subtype Multiple_Channels_Watchdog is Analog_Watchdog_Modes range Watchdog_All_Regular_Channels   .. Watchdog_All_Both_Kinds;
   subtype Single_Channel_Watchdog    is Analog_Watchdog_Modes range Watchdog_Single_Regular_Channel .. Watchdog_Single_Both_Kinds;

   procedure Watchdog_Enable_Channels (No : ADC_No; Mode : Multiple_Channels_Watchdog;                     Low, High  : Bits_12) with Post => Watchdog_Enabled (No);
   procedure Watchdog_Enable_Channel  (No : ADC_No; Mode : Single_Channel_Watchdog; Channel : AWDCH_Range; Low, High  : Bits_12) with Post => Watchdog_Enabled (No);

   procedure Watchdog_Disable (No : ADC_No) with Post => not Watchdog_Enabled (No), Inline;

   function Watchdog_Enabled (No : ADC_No) return Boolean with Inline;

private

   -- Timing; Values are somewhat arbitrary and environment dependent.

   ADC_Stabilization                : constant Time_Span := Microseconds (3);
   Temperature_Sensor_Stabilization : constant Time_Span := Microseconds (10);

end STM32F4.ADC.Ops;
