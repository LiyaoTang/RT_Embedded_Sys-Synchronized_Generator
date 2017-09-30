--
-- Uwe R. Zimmer, Australia 2015
--

with STM32F4.General_purpose_IOs; use STM32F4.General_purpose_IOs;
with STM32F4.USART;               use STM32F4.USART;

package ANU_Base_Board.Config is

   LED_Wires : constant array (Com_Ports, Indicators) of Port_Pin :=

     (1 => (L => (Port => E, Pin =>  6),
            R => (Port => E, Pin =>  4)),
      2 => (L => (Port => E, Pin =>  5),
            R => (Port => E, Pin =>  2)),
      3 => (L => (Port => A, Pin =>  4),
            R => (Port => A, Pin =>  2)),
      4 => (L => (Port => A, Pin =>  3),
            R => (Port => A, Pin =>  1)));

   Com_Wires : constant array (Com_Ports,
                               Com_Directions,
                               Com_Features) of Port_Pin :=

     (1 => (Rx => (En => (Port => B, Pin =>  9),
                   Da => (Port => B, Pin =>  7)),
            Tx => (En => (Port => B, Pin =>  8),
                   Da => (Port => B, Pin =>  6))),
      2 => (Rx => (En => (Port => D, Pin =>  1),
                   Da => (Port => D, Pin =>  6)),
            Tx => (En => (Port => D, Pin =>  3),
                   Da => (Port => D, Pin =>  5))),
      3 => (Rx => (En => (Port => C, Pin => 15),
                   Da => (Port => C, Pin => 11)),
            Tx => (En => (Port => C, Pin => 13),
                   Da => (Port => C, Pin => 10))),
      4 => (Rx => (En => (Port => D, Pin =>  7),
                   Da => (Port => D, Pin =>  2)),
            Tx => (En => (Port => B, Pin =>  5),
                   Da => (Port => C, Pin => 12))));

   UART_Wires : constant array (Com_Ports) of USARTs :=
     (1 => 1,
      2 => 2,
      3 => 4,
      4 => 5);

end ANU_Base_Board.Config;
