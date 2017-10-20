package body STM32F4.DMA_controller.Ops is

   procedure Configure
     (DMA                         : DMAs;
      Stream                      : Streams;
      Stream_Enable               : Enabler;
      Direct_mode_error_interrupt : Enabler        := Disable;
      Transfer_error_interrupt    : Enabler        := Disable;
      Half_transfer_interrupt     : Enabler        := Disable;
      Transfer_complete_interrupt : Enabler        := Disable;
      Peripheral_flow_controller  : PFCTRL_Options := DMA_Flowcontrol;
      Data_transfer_direction     : DIR_Options    := Peripheral_to_Memory;
      Circular_mode               : Enabler        := Disable;
      Peripheral_increment_mode   : INC_Options    := Address_Fixed;
      Memory_increment_mode       : INC_Options    := Address_Fixed;
      Peripheral_data_size        : SIZE_Options   := Byte_Size;
      Memory_data_size            : SIZE_Options   := Byte_Size;
      Peripheral_increment_offset : PINCOS_Options := Linked_to_PSIZE;
      Priority_level              : PL_Options     := Low;
      Double_buffer_mode          : Enabler        := Disable;
      Peripheral_burst            : BURST_Options  := Single;
      Memory_burst                : BURST_Options  := Single;
      Channel                     : Bits_3         := 0) is

      DMA_CR : DMA_SxCR := DMAx (DMA).all.DMA_S (Stream).CR;

   begin
      DMA_CR := (EN     => Stream_Enable,
                 DMEIE  => Direct_mode_error_interrupt,
                 TEIE   => Transfer_error_interrupt,
                 HTIE   => Half_transfer_interrupt,
                 TCIE   => Transfer_complete_interrupt,
                 PFCTRL => Peripheral_flow_controller,
                 DIR    => Data_transfer_direction,
                 CIRC   => Circular_mode,
                 PINC   => Peripheral_increment_mode,
                 MINC   => Memory_increment_mode,
                 PSIZE  => Peripheral_data_size,
                 MSIZE  => Memory_data_size,
                 PINCOS => Peripheral_increment_offset,
                 PL     => Priority_level,
                 DBM    => Double_buffer_mode,
                 CT     => DMA_CR.CT,
                 PBURST => Peripheral_burst,
                 MBURST => Memory_burst,
                 CHSEL  => Channel,
                 Reserved_1 => DMA_CR.Reserved_1,
                 Reserved_2 => DMA_CR.Reserved_2);
      DMAx (DMA).all.DMA_S (Stream).CR := DMA_CR;
   end Configure;

   procedure Set_Peripheral_Address (DMA : DMAs; Stream : Streams; Addr : Bits_32) is

   begin
      DMAx (DMA).all.DMA_S (Stream).PAR := Addr;
   end Set_Peripheral_Address;

   procedure Set_Memory_0_Address   (DMA : DMAs; Stream : Streams; Addr : Bits_32) is

   begin
      DMAx (DMA).all.DMA_S (Stream).M0AR := Addr;
   end Set_Memory_0_Address;

   procedure Set_Memory_1_Address   (DMA : DMAs; Stream : Streams; Addr : Bits_32) is

   begin
      DMAx (DMA).all.DMA_S (Stream).M1AR := Addr;
   end Set_Memory_1_Address;

   procedure Set_No_of_Transfers    (DMA : DMAs; Stream : Streams; No : Bits_16) is

   begin
      DMAx (DMA).all.DMA_S (Stream).NDTR := (NDT => No, Reserved => 0);
   end Set_No_of_Transfers;

   function  Flag (DMA : DMAs; Stream : Streams; This_Flag : DMA_Flags) return Boolean is

      LISR : constant DMA_LISR := DMAx (DMA).all.LISR;
      HISR : constant DMA_HISR := DMAx (DMA).all.HISR;

   begin
      return
        (case Stream is
            when 0 => (case This_Flag is
                          when FIFO_error        => LISR.IF0.FE  = Occured,
                          when Direct_mode_error => LISR.IF0.DME = Occured,
                          when Transfer_error    => LISR.IF0.TE  = Occured,
                          when Half_transfer     => LISR.IF0.HT  = Occured,
                          when Transfer_complete => LISR.IF0.TC  = Occured),
            when 1 => (case This_Flag is
                          when FIFO_error        => LISR.IF1.FE  = Occured,
                          when Direct_mode_error => LISR.IF1.DME = Occured,
                          when Transfer_error    => LISR.IF1.TE  = Occured,
                          when Half_transfer     => LISR.IF1.HT  = Occured,
                          when Transfer_complete => LISR.IF1.TC  = Occured),
            when 2 => (case This_Flag is
                          when FIFO_error        => LISR.IF2.FE  = Occured,
                          when Direct_mode_error => LISR.IF2.DME = Occured,
                          when Transfer_error    => LISR.IF2.TE  = Occured,
                          when Half_transfer     => LISR.IF2.HT  = Occured,
                          when Transfer_complete => LISR.IF2.TC  = Occured),
            when 3 => (case This_Flag is
                          when FIFO_error        => LISR.IF3.FE  = Occured,
                          when Direct_mode_error => LISR.IF3.DME = Occured,
                          when Transfer_error    => LISR.IF3.TE  = Occured,
                          when Half_transfer     => LISR.IF3.HT  = Occured,
                          when Transfer_complete => LISR.IF3.TC  = Occured),
            when 4 => (case This_Flag is
                          when FIFO_error        => HISR.IF4.FE  = Occured,
                          when Direct_mode_error => HISR.IF4.DME = Occured,
                          when Transfer_error    => HISR.IF4.TE  = Occured,
                          when Half_transfer     => HISR.IF4.HT  = Occured,
                          when Transfer_complete => HISR.IF4.TC  = Occured),
            when 5 => (case This_Flag is
                          when FIFO_error        => HISR.IF5.FE  = Occured,
                          when Direct_mode_error => HISR.IF5.DME = Occured,
                          when Transfer_error    => HISR.IF5.TE  = Occured,
                          when Half_transfer     => HISR.IF5.HT  = Occured,
                          when Transfer_complete => HISR.IF5.TC  = Occured),
            when 6 => (case This_Flag is
                          when FIFO_error        => HISR.IF6.FE  = Occured,
                          when Direct_mode_error => HISR.IF6.DME = Occured,
                          when Transfer_error    => HISR.IF6.TE  = Occured,
                          when Half_transfer     => HISR.IF6.HT  = Occured,
                          when Transfer_complete => HISR.IF6.TC  = Occured),
            when 7 => (case This_Flag is
                          when FIFO_error        => HISR.IF7.FE  = Occured,
                          when Direct_mode_error => HISR.IF7.DME = Occured,
                          when Transfer_error    => HISR.IF7.TE  = Occured,
                          when Half_transfer     => HISR.IF7.HT  = Occured,
                          when Transfer_complete => HISR.IF7.TC  = Occured)
        );
   end Flag;

   procedure Clear_Flag (DMA : DMAs; Stream : Streams; This_Flag : DMA_Flags) is

      LIFCR : DMA_LIFCR := DMA_LIFCR_Reset;
      HIFCR : DMA_HIFCR := DMA_HIFCR_Reset;

   begin
      case Stream is
         when 0 =>
            case This_Flag is
               when FIFO_error        => LIFCR.IF0.CFE  := Clear;
               when Direct_mode_error => LIFCR.IF0.CDME := Clear;
               when Transfer_error    => LIFCR.IF0.CTE  := Clear;
               when Half_transfer     => LIFCR.IF0.CHT  := Clear;
               when Transfer_complete => LIFCR.IF0.CTC  := Clear;
            end case;
         when 1 =>
            case This_Flag is
               when FIFO_error        => LIFCR.IF1.CFE  := Clear;
               when Direct_mode_error => LIFCR.IF1.CDME := Clear;
               when Transfer_error    => LIFCR.IF1.CTE  := Clear;
               when Half_transfer     => LIFCR.IF1.CHT  := Clear;
               when Transfer_complete => LIFCR.IF1.CTC  := Clear;
            end case;
         when 2 =>
            case This_Flag is
               when FIFO_error        => LIFCR.IF2.CFE  := Clear;
               when Direct_mode_error => LIFCR.IF2.CDME := Clear;
               when Transfer_error    => LIFCR.IF2.CTE  := Clear;
               when Half_transfer     => LIFCR.IF2.CHT  := Clear;
               when Transfer_complete => LIFCR.IF2.CTC  := Clear;
            end case;
         when 3 =>
            case This_Flag is
               when FIFO_error        => LIFCR.IF3.CFE  := Clear;
               when Direct_mode_error => LIFCR.IF3.CDME := Clear;
               when Transfer_error    => LIFCR.IF3.CTE  := Clear;
               when Half_transfer     => LIFCR.IF3.CHT  := Clear;
               when Transfer_complete => LIFCR.IF3.CTC  := Clear;
            end case;
         when 4 =>
            case This_Flag is
               when FIFO_error        => HIFCR.IF4.CFE  := Clear;
               when Direct_mode_error => HIFCR.IF4.CDME := Clear;
               when Transfer_error    => HIFCR.IF4.CTE  := Clear;
               when Half_transfer     => HIFCR.IF4.CHT  := Clear;
               when Transfer_complete => HIFCR.IF4.CTC  := Clear;
            end case;
         when 5 =>
            case This_Flag is
               when FIFO_error        => HIFCR.IF5.CFE  := Clear;
               when Direct_mode_error => HIFCR.IF5.CDME := Clear;
               when Transfer_error    => HIFCR.IF5.CTE  := Clear;
               when Half_transfer     => HIFCR.IF5.CHT  := Clear;
               when Transfer_complete => HIFCR.IF5.CTC  := Clear;
            end case;
         when 6 =>
            case This_Flag is
               when FIFO_error        => HIFCR.IF6.CFE  := Clear;
               when Direct_mode_error => HIFCR.IF6.CDME := Clear;
               when Transfer_error    => HIFCR.IF6.CTE  := Clear;
               when Half_transfer     => HIFCR.IF6.CHT  := Clear;
               when Transfer_complete => HIFCR.IF6.CTC  := Clear;
            end case;
         when 7 =>
            case This_Flag is
               when FIFO_error        => HIFCR.IF7.CFE  := Clear;
               when Direct_mode_error => HIFCR.IF7.CDME := Clear;
               when Transfer_error    => HIFCR.IF7.CTE  := Clear;
               when Half_transfer     => HIFCR.IF7.CHT  := Clear;
               when Transfer_complete => HIFCR.IF7.CTC  := Clear;
            end case;
      end case;

      case Stream is
         when 0 .. 3 => DMAx (DMA).all.LIFCR := LIFCR;
         when 4 .. 7 => DMAx (DMA).all.HIFCR := HIFCR;
      end case;
   end Clear_Flag;

end STM32F4.DMA_controller.Ops;
