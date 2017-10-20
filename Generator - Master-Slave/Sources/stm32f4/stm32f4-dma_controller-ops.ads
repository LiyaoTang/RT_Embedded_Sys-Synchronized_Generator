--
-- Uwe R. Zimmer, Australia 2015
--

-- STM32F4xx DMA controller Operations

package STM32F4.DMA_controller.Ops is

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
      Channel                     : Bits_3         := 0
     ) with Inline;

   procedure Set_Peripheral_Address (DMA : DMAs; Stream : Streams; Addr : Bits_32) with Inline;
   procedure Set_Memory_0_Address   (DMA : DMAs; Stream : Streams; Addr : Bits_32) with Inline;
   procedure Set_Memory_1_Address   (DMA : DMAs; Stream : Streams; Addr : Bits_32) with Inline;

   procedure Set_No_of_Transfers    (DMA : DMAs; Stream : Streams; No : Bits_16) with Inline;

   type DMA_Flags is
     (FIFO_error,
      Direct_mode_error,
      Transfer_error,
      Half_transfer,
      Transfer_complete);

   function  Flag       (DMA : DMAs; Stream : Streams; This_Flag : DMA_Flags) return Boolean with Inline;
   procedure Clear_Flag (DMA : DMAs; Stream : Streams; This_Flag : DMA_Flags)                with Inline;

end STM32F4.DMA_controller.Ops;
