--
-- Uwe R. Zimmer, Australia 2015
--
with System.Storage_Elements; use System.Storage_Elements;

package body STM32F4.General_purpose_IOs.Ops is

   procedure Set (Wire : Port_Pin) is

   begin
      GPIOx (Wire.Port).all.BSRR.Set (Wire.Pin) := Set;
   end Set;

   procedure Set (Port : GPIO_Ports; Pins_Set : Pin_Sets) is

      Pins_To_Set : Setting_Array (GPIO_Pins) := (others => Do_Nothing);

   begin
      for Pin of Pins_Set loop
         Pins_To_Set (Pin) := Set;
      end loop;
      GPIOx (Port).all.BSRR.Set := Pins_To_Set;
   end Set;

   procedure Reset (Wire : Port_Pin) is

   begin
      GPIOx (Wire.Port).all.BSRR.Reset (Wire.Pin) := Set;
   end Reset;

   procedure Reset (Port : GPIO_Ports; Pins_Reset : Pin_Sets) is

      Pins_To_Reset : Setting_Array (GPIO_Pins) := (others => Do_Nothing);

   begin
      for Pin of Pins_Reset loop
         Pins_To_Reset (Pin) := Set;
      end loop;
      GPIOx (Port).all.BSRR.Reset := Pins_To_Reset;
   end Reset;

   procedure Set_Reset (Port : GPIO_Ports; Pins_Set, Pins_Reset : Pin_Sets) is

      Set_Reset_Register : GPIOx_BSRR := (Set   => (others => Do_Nothing),
                                          Reset => (others => Do_Nothing));

   begin
      for Pin of Pins_Set loop
         Set_Reset_Register.Set (Pin) := Set;
      end loop;
      for Pin of Pins_Reset loop
         Set_Reset_Register.Reset (Pin) := Set;
      end loop;
      GPIOx (Port).all.BSRR := Set_Reset_Register;
   end Set_Reset;

   procedure Output_Write (Wire : Port_Pin; State : Bit) is

   begin
      GPIOx (Wire.Port).all.ODR (Wire.Pin) := State;
   end Output_Write;

   function Output_Read (Wire : Port_Pin) return Bit is

      (GPIOx (Wire.Port).all.ODR (Wire.Pin));

   function Input_Read  (Wire : Port_Pin) return Bit is

     (GPIOx (Wire.Port).all.IDR (Wire.Pin));

   protected body Atomic_Switch is

      procedure Toggle (Wire : Port_Pin) is

      begin
         case Output_Read (Wire) is
            when 0 => Output_Write (Wire, 1);
            when 1 => Output_Write (Wire, 0);
         end case;
      end Toggle;

   end Atomic_Switch;

   function Read_Mode (Wire : Port_Pin) return MODER_Options is
     (GPIOx (Wire.Port).all.MODER (Wire.Pin));

   function Read_Output_Type (Wire : Port_Pin) return OTYPER_Options is
     (GPIOx (Wire.Port).all.OTYPER (Wire.Pin));

   function Read_Output_Speed (Wire : Port_Pin) return OSPEEDR_Options is
     (GPIOx (Wire.Port).all.OSPEEDR (Wire.Pin));

   function Read_Pull (Wire : Port_Pin) return PUPDR_Options is
     (GPIOx (Wire.Port).all.PUPDR (Wire.Pin));

   function Is_Locked (Wire : Port_Pin) return Boolean is
      (GPIOx (Wire.Port).all.LCKR.Locked_Pins (Wire.Pin) = Locked);

   procedure Initialize_Pin (Wire    : Port_Pin;
                             Mode    : MODER_Options;
                             Out_T   : OTYPER_Options;
                             Speed   : OSPEEDR_Options;
                             Pull    : PUPDR_Options;
                             Lock_it : Boolean         := False) is

   begin
      Set_Mode         (Wire, Mode);
      Set_Output_Type  (Wire, Out_T);
      Set_Output_Speed (Wire, Speed);
      Set_Pull         (Wire, Pull);
      if Lock_it then
         Lock_Pin (Wire);
      end if;
   end Initialize_Pin;

   procedure Initialize_Output_Pin (Wire    : Port_Pin;
                                    Mode    : MODER_Options   := General_purpose_output;
                                    Out_T   : OTYPER_Options  := Push_Pull;
                                    Speed   : OSPEEDR_Options := Speed_High;
                                    Pull    : PUPDR_Options   := No_Pull;
                                    Lock_it : Boolean         := False) is

   begin
      Initialize_Pin (Wire, Mode, Out_T, Speed, Pull, Lock_it);
   end Initialize_Output_Pin;

   procedure Initialize_Input_Pin (Wire    : Port_Pin;
                                   Pull    : PUPDR_Options := No_Pull;
                                   Lock_it : Boolean       := False) is

   begin
      Set_Mode (Wire, Input);
      Set_Pull (Wire, Pull);
      if Lock_it then
         Lock_Pin (Wire);
      end if;
   end Initialize_Input_Pin;

   procedure Set_Mode (Wire : Port_Pin; Mode : MODER_Options) is

   begin
      GPIOx (Wire.Port).all.MODER (Wire.Pin) := Mode;
   end Set_Mode;

   procedure Set_Output_Type (Wire : Port_Pin; Out_T : OTYPER_Options) is

   begin
      GPIOx (Wire.Port).all.OTYPER (Wire.Pin) := Out_T;
   end Set_Output_Type;

   procedure Set_Output_Speed (Wire : Port_Pin; Speed : OSPEEDR_Options) is

   begin
      GPIOx (Wire.Port).all.OSPEEDR (Wire.Pin) := Speed;
   end Set_Output_Speed;

   procedure Set_Pull  (Wire : Port_Pin; Pull  : PUPDR_Options) is

   begin
      GPIOx (Wire.Port).all.PUPDR (Wire.Pin) := Pull;
   end Set_Pull;

   procedure Lock_Pin (Wire : Port_Pin) is

      Lock_Register : GPIOx_LCKR := (Locked_Pins => (others => Unlocked),
                                     Lock_Key    => Locked,
                                     Reserved    => 0);

   begin
      Lock_Register.Locked_Pins (Wire.Pin) := Locked;
      GPIOx (Wire.Port).all.LCKR := Lock_Register;
      Lock_Register.Lock_Key := Unlocked;
      GPIOx (Wire.Port).all.LCKR := Lock_Register;
      Lock_Register.Lock_Key := Locked;
      GPIOx (Wire.Port).all.LCKR := Lock_Register;
   end Lock_Pin;

   function Set_Reset_Register_Address (Port : GPIO_Ports) return Bits_32 is

     (case Port is
         when A => Bits_32 (To_Integer (GPIOA.BSRR'Address)),
         when B => Bits_32 (To_Integer (GPIOB.BSRR'Address)),
         when C => Bits_32 (To_Integer (GPIOC.BSRR'Address)),
         when D => Bits_32 (To_Integer (GPIOD.BSRR'Address)),
         when E => Bits_32 (To_Integer (GPIOE.BSRR'Address)),
         when F => Bits_32 (To_Integer (GPIOF.BSRR'Address)),
         when G => Bits_32 (To_Integer (GPIOG.BSRR'Address)),
         when H => Bits_32 (To_Integer (GPIOH.BSRR'Address)),
         when I => Bits_32 (To_Integer (GPIOI.BSRR'Address))
     );

end STM32F4.General_purpose_IOs.Ops;
