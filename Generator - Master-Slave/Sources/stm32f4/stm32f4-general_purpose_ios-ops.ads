--
-- Uwe R. Zimmer, Australia 2015
--

-- STM32F4xx General purpose IOs Operations

package STM32F4.General_purpose_IOs.Ops is

   type Pin_Sets is array (Positive range <>) of GPIO_Pins;

   procedure Set   (Wire : Port_Pin)                        with Inline;
   procedure Set   (Port : GPIO_Ports; Pins_Set : Pin_Sets) with Inline; -- Set multiple pins atomically

   procedure Reset (Wire : Port_Pin)                          with Inline;
   procedure Reset (Port : GPIO_Ports; Pins_Reset : Pin_Sets) with Inline; -- Reset multiple pins atomically

   procedure Set_Reset (Port : GPIO_Ports; Pins_Set, Pins_Reset : Pin_Sets) with Inline; -- Set & reset multiple pins atomically

   procedure Output_Write (Wire : Port_Pin; State : Bit) with Inline;

   function Output_Read (Wire : Port_Pin) return Bit with Inline;
   function Input_Read  (Wire : Port_Pin) return Bit with Inline;

   protected Atomic_Switch is
      procedure Toggle (Wire : Port_Pin);
   end Atomic_Switch;

   function Read_Mode         (Wire : Port_Pin) return MODER_Options   with Inline;
   function Read_Output_Type  (Wire : Port_Pin) return OTYPER_Options  with Inline;
   function Read_Output_Speed (Wire : Port_Pin) return OSPEEDR_Options with Inline;
   function Read_Pull         (Wire : Port_Pin) return PUPDR_Options   with Inline;
   function Is_Locked         (Wire : Port_Pin) return Boolean         with Inline;

   procedure Initialize_Pin (Wire    : Port_Pin;
                             Mode    : MODER_Options;
                             Out_T   : OTYPER_Options;
                             Speed   : OSPEEDR_Options;
                             Pull    : PUPDR_Options;
                             Lock_it : Boolean         := False) with Inline, Pre => not Is_Locked (Wire);

   procedure Initialize_Output_Pin (Wire    : Port_Pin;
                                    Mode    : MODER_Options   := General_purpose_output;
                                    Out_T   : OTYPER_Options  := Push_Pull;
                                    Speed   : OSPEEDR_Options := Speed_High;
                                    Pull    : PUPDR_Options   := No_Pull;
                                    Lock_it : Boolean         := False) with Inline, Pre => not Is_Locked (Wire);

   procedure Initialize_Input_Pin (Wire    : Port_Pin;
                                   Pull    : PUPDR_Options := No_Pull;
                                   Lock_it : Boolean       := False) with Inline, Pre => not Is_Locked (Wire);

   procedure Set_Mode         (Wire : Port_Pin; Mode  : MODER_Options)   with Inline, Pre => not Is_Locked (Wire);
   procedure Set_Output_Type  (Wire : Port_Pin; Out_T : OTYPER_Options)  with Inline, Pre => not Is_Locked (Wire) and then Read_Mode (Wire) /= Input;
   procedure Set_Output_Speed (Wire : Port_Pin; Speed : OSPEEDR_Options) with Inline, Pre => not Is_Locked (Wire) and then Read_Mode (Wire) /= Input;
   procedure Set_Pull         (Wire : Port_Pin; Pull  : PUPDR_Options)   with Inline, Pre => not Is_Locked (Wire);
   procedure Lock_Pin         (Wire : Port_Pin)                          with Inline, Pre => not Is_Locked (Wire);

   function Set_Reset_Register_Address (Port : GPIO_Ports) return Bits_32;

end STM32F4.General_purpose_IOs.Ops;
