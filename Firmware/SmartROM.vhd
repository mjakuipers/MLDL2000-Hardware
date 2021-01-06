-------------------------------------------------------------------------------
--    Copyright (c) 2005-2008  Meindert Kuipers, Netherlands                 --
--    meindert@kuipers.to                www.kuipers.to                      --
--                                                                           --
-- This program is free software; you can redistribute it and/or             --
-- modify it under the terms of the GNU General Public License               --
-- as published by the Free Software Foundation; either version 2            --
-- of the License, or (at your option) any later version.                    --
--                                                                           --
-- This program is distributed in the hope that it will be useful,           --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of            --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             --
-- GNU General Public License for more details.                              --
--                                                                           --
-- You should have received a copy of the GNU General Public License         --
-- along with this program; if not, write to the Free Software               --
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--             SmartROM.vhdl                                                 --
--  Ver  Date     Description                                                --
--  0.99 Feb 2005 First version for Beta release                             --
--  1.00 Jun 2005 First full release, no code changes from Beta              --
--                Synthesis with Xilinx WebISE 7.1i                          --
--                I/O is not tested                                          --
--  1.01 Oct 2006 Added LED support for blingbling version                   --
--  1.02 Sep 2007 Synthesis with Xilinx ISE 9.2, first XSVF release          --
--                Blingbling commented out, must be tested better            --
--                Removed SPAREx signals                                     --
--  1.03 Oct 2007 Synthesis with Xilinx ISE 9.2.03i                          --
--                BlingBling is back                                         --
--                Bank Switching bug is now fixed                            --
--  1.50 May 2008 Synthesis for final release 1.50                           --
--                All phase references in comments are now decimal           --
--  1.51 Dec 2008 Starting point for V2:                                     --
--                - take care of propagation delay in levelshifter           --
--                - fix OSR timing issue and dependency on synthesis mode    --
--                - modified State Machine for I/O, SPI and Data Shifter     --
--                - changed Data Shift Register                              --
--                - added I/O register and MLDLStatus Register               --
--                - fixed X_BSY/X_DAV handshaking                            --
--                - Instruction Detector is now register instead of latch    --
--                - removed direct memory and SR access from HP41            --
--                - IOR clock is now external clock from OSR                 --
--                - adapted for V1 release 1.51                              --
--                - PWO now resets the Enabled Bank registers                --
--                - removed blingbling driver but kept signals               --
--  1.60 Oct 2010 Update 2010                                                --
--                - added 12-bit Settings Registers                          --
--                - register inversion for I/O reg x807                      --
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity SmartROM is Port (

-- HP41 Bus Signals
  SYNC,         -- HP41 Signal: Indicates start of bus cycle
  CLK01,        -- HP41 Signal: Rising edge clocks data
  CLK02,        -- HP41 Signal: Count clock cycle state and output clock
  DATA,         -- HP41 Signal: Data from C-register
  ISA,          -- HP41 Signal: Address and Instruction line
  PWO:          -- HP41 Signal: Power On line
                in std_logic;

-- HP41 Output Control Signals
  ISA_out,      -- ISA Output to driver
  ISA_OE,       -- ISA Output Enable for driver
  DATA_out,     -- DATA Output to driver
  DATA_OE,      -- DATA Output Enable for driver
  SYNC_out,     -- SYNC Output to driver
  SYNC_OE,      -- SYNC Output Enable fordriver
  FI_Out,       -- Peripheral Flag Output
  FI_OE:        -- Peripheral Flag Output Enable
                out std_logic;

-- Memory Control Signals
  XA:           -- 19-bits External Address Bus for FLASH and SRAM
                out std_logic_vector(19 downto 00);
  XD:           -- 16-bit external Data Bus
                inout std_logic_vector(15 downto 00);

  FLASH_CE,     -- FLASH Chip Enable (active low)
  FLASH_OE,     -- FLASH Output Enable (active low)
  FLASH_WE,     -- FLASH Write Enable (active low signal)
  SRAM_CE1,     -- SRAM Chip Enable 1 (active low signal)
  SRAM_CE2,     -- SRAM Chip Enable 2 (active high signal)
  SRAM_OE,      -- SRAM Output Enable (active low signal)
  SRAM_WE,      -- SRAM Write Enable (active low signal)
  MEM_RESET:    -- FLASH reset signal
                out std_logic;
  RY_BY:        -- FLASH RY/BY# signal
                in std_logic;

-- Utility Signals connected to DIPSWITCH
  MLDL_SR:      in std_logic;  -- Status Registers from SRAM (1) or FLASH (0)
  SR_SET:       in std_logic_vector(1 downto 0);
                               -- select 1 of 4 Settings Register Sets
  MLDL_DIS:     in std_logic;  -- disable MLDL output to HP41

-- External I/O Interface Signals
  X_EN,                         -- Enable eXternal Interface
  X_CLK,                        -- eXternal Interface Clock   on JTAG-TCK!
  X_STROBE,                     -- eXternal Strobe            on JTAG-TMS!
  X_DIN:        in std_logic;   -- eXternal Input             on JTAG-TDI!
  X_DOUT:       out std_logic;  -- eXternal Output            on JTAG-TDO!
  X_DAV,                        -- Data Available in MLDL, connects with H_DAV
  X_BSY:        out std_logic;  -- MLDL Busy on current data, connects with H_BSY

  DSR_clk,
  OSR_clk:      in std_logic;  -- to use global clocks here
  DSR_clk_out,
  OSR_clk_out:  out std_logic;

  SPARE0,
  SPARE1,
  SPARE2,
  SPARE3:       out std_logic

  );

  end SmartROM;

architecture Behavioral of SmartROM is

signal  ISR:      std_logic_vector(15 downto 00);   -- ISA Shift Register
alias   I:        std_logic_vector(09 downto 00) is ISR(09 downto 00);
alias   EBR_A:    std_logic_vector(02 downto 00) is ISR(15 downto 13);
alias   EBR_D:    std_logic_vector(01 downto 00) is ISR(07 downto 06);
alias   IOCheck:  std_logic_vector(07 downto 00) is ISR(11 downto 04);
alias   IOReg:    std_logic_vector(03 downto 00) is ISR(03 downto 00);

signal  OSR:      std_logic_vector(09 downto 00);   -- ISA Output Shift Register
signal  ISA_O:    std_logic;                        -- ISA Output internal

signal  DSR:      std_logic_vector(29 downto 00);   -- DATA Shift Register
alias   DSR_A:    std_logic_vector(15 downto 00) is DSR(27 downto 12); -- address part for WROM
alias   DSR_Chk:  std_logic_vector(07 downto 00) is DSR(23 downto 16); -- address part for IOCheck
alias   DSR_IO:   std_logic_vector(03 downto 00) is DSR(15 downto 12); -- address part for register
signal  XSR:      std_logic_vector(07 downto 00);   -- DATA Shift Register

-- HP41 I/O Register
signal  IOR:      std_logic_vector(15 downto 00);   -- HP41 I/O Register
signal  H_DAV:    std_logic;    -- I/O status bit: X_DAV, Data Available for reading
signal  H_BSY:    std_logic;    -- I/O status bit: X_BSY, Data Pending in I/O


-- temp storage Settings Register
signal  SR:       std_logic_vector(11 downto 0);    -- ROM Address Translation

-- Phase Counter and clocks
signal  ph_count:   std_logic_vector(5 downto 0);
signal  ph_reset,
        t1_reset,
        t2_reset:   std_logic;

signal  SRAM_CE:    std_logic;      -- Internal SRAM CE

signal  SPR:                        -- internal signals for SPARE/LED driver
                    std_logic_vector(03 downto 00);

-- Main MLDL State Machine
type MLDLState is (
      Idle,                   -- nothing to do ....
      MLDL_disable,           -- MLDL is disabled and ready for I/O
      ROMReadSR,              -- Read Status Register for FLASH/SRAM Access
      ROMRead,                -- Read data for FLASH/SRAM access
      ShiftInISA,             -- Regular ISA ISR read, no OSR shifting
      ShiftOutISA,            -- Move data in OSR out to ISA (and read back!)
      DecodeInstr,            -- Decode Instruction in ISR
      DecodeInstr2,           -- Idle phase
      ShiftInData,            -- time to shift in Data
      ReadAddrISA,            -- Read Address on ISA
      WROMReadSR,             -- Read SR for WROM preparation
      WROMWrite,              -- Write for WROM
      IORead,                 -- Read from IO Register
      IOWrite,                -- Write to IO Register
      MLDLStatRead,           -- Read from MLDL Status register
      MLDLStatWRite           -- Write to MLDL Status register for control
    );
signal MLDLStat, next_MLDLStat : MLDLState;

-- Instruction Decoder state machine
type  I_State is (
      I_Idle,           -- no instruction recognized
      I_WROM,           -- instruction: WROM      040
      I_FETCH,          --              FETCH S&X 030
      I_ENBANK          --              ENBANKx   100/180/140/1C0
      );
signal IStat: I_State;

-- registers for Bank Switching
type   Bank_type is array(7 downto 0) of std_logic_vector (1 downto 0);
signal EBR: Bank_type := ("00","00","00","00","00","00","00","00");
signal B: std_logic_vector (1 downto 0);  -- temp storage for Active Bank


begin

-------------------------------------------------------------------------------
--             PHASE COUNTER                                                 --
-------------------------------------------------------------------------------

sync_detect: process(SYNC, CLK01, CLK02, t1_reset, t2_reset)
-- Generates the actual counter reset after detecting SYNC sequence
-- and on phase 67 (octal) if no SYNC is present
begin
  if (CLK01'event and CLK01 = '1') then
    t1_reset <= SYNC;
    if ((ph_count = "110111") or (t1_reset = '1')) then     -- Phase 55
      t2_reset <= '1';
    else
      t2_reset <= '0';
    end if;
  end if;
  ph_reset <= (t2_reset and CLK02 and (not t1_reset));
end process;

phase_counter: process(CLK02, ph_reset)
-- 6-bit phase counter
begin
  if (ph_reset = '1') then
    ph_count <= "000000";                  -- reset
  elsif (CLK02'event and CLK02 = '1') then
    ph_count <= ph_count + 1;
  end if;
end process;

-------------------------------------------------------------------------------
--           MLDL2000 MAIN STATE MACHINE                                     --
-------------------------------------------------------------------------------

MLDL_state_machine: process (MLDLStat, X_EN, MLDL_DIS, IStat, ISR, DSR,
                             SYNC, SR, ph_count, IOCheck, IOReg, DSR_chk, DSR_IO)
begin
  next_MLDLStat <= Idle;                -- default state value

  case MLDLStat is

    when Idle =>                        -- Idle state, wait with nothing to do
      if (ph_count = "001110") then     -- phase 14
        next_MLDLStat <= ReadAddrISA;
      end if;
      if (ph_count = "101100") then     -- ISA Instruction time, phase 46
        next_MLDLStat <= ShiftInISA;
      end if;
      if (ph_count = "101011") and (SR(11) = '0') then
          -- one clock before ISA instruction, phase 45 due to delay in levelshifter
          -- valid ROM or IO access, OSR shift out
          next_MLDLStat <= ShiftOutISA;
      end if;
      if X_EN = '0' then
        next_MLDLStat <= MLDL_Disable;
      end if;

    when MLDL_disable =>              -- MLDL is disabled and ready for I/O
      if X_EN = '1' then
        next_MLDLStat <= Idle;
      else
        next_MLDLStat <= MLDL_disable;
      end if;

    when ROMReadSR =>                 -- Read Status Register for FLASH/SRAM Access
      if SR(11) = '1' then            -- if disabled bank ....
        next_MLDLStat   <= Idle;
      elsif ((SR(10) = '0') and (IOCheck = "10000000")) then
        -- This is an I/O access
        if ISR(3) = '0' then                  -- $x800..$x807
          next_MLDLStat <= IORead;            -- IO Register
        else
          case IOReg is
            when "1000" =>
              next_MLDLStat <= MLDLStatRead;  -- MLDL Status register
            when others =>
              next_MLDLStat <= Idle;          -- reserved registers, do nothing
          end case;
        end if;
      else
        next_MLDLStat <= ROMRead;
      end if;

    when ROMRead =>                   -- Read L8 data for FLASH/SRAM access
      next_MLDLStat <= Idle;

    when ShiftInISA =>                -- Regular ISA ISR read, no OSR shifting
      if (ph_count = "110110") then   -- phase 46
        if SYNC = '1' then
          next_MLDLStat <= DecodeInstr;
        else
          next_MLDLStat <= Idle;
        end if;
      else
        next_MLDLStat <= ShiftInISA;
      end if;

    when ShiftOutISA =>               -- Move data in OSR out to ISA and read back
      if (ph_count = "110110") then   -- phase 46
        if SYNC = '1' then
          next_MLDLStat <= DecodeInstr;
        else
          next_MLDLStat <= Idle;
        end if;
      else
        next_MLDLStat <= ShiftOutISA;
      end if;

    when DecodeInstr =>               -- Decode Instruction in ISR
      next_MLDLStat <= DecodeInstr2;

    when DecodeInstr2 =>              -- Idle phase
      next_MLDLStat <= ShiftInData;

    when ShiftInData =>               -- State for shifting in data
      if (ph_count = "001110") then     -- phase 14
        next_MLDLStat <= ReadAddrISA;
      else
        next_MLDLStat <= ShiftInData;
      end if;

    when ReadAddrISA =>               -- Read Address on ISA
      if (ph_count = "011110") then   -- phase 31
        if IStat = I_WROM then
          next_MLDLStat <= WROMReadSR;
        else
          next_MLDLStat <= ROMReadSR;
        end if;
      else
        next_MLDLStat <= ReadAddrISA;
      end if;
                         
    when WROMReadSR =>                  -- Read SR for WROM preparation
                                        -- writing to a disabled bank is OK!
                                        --I/O writes are never write protected
      if ((SR(10) = '0') and (DSR_Chk = "10000000")) then  -- valid I/O access
        if DSR(15) = '0' then                              -- $x800..$x807
          next_MLDLStat <= IOWrite;                        -- IO Registers
        else 
      	  case DSR_IO is
            when "1000" =>                                 -- $x808   
              next_MLDLStat <= MLDLStatWrite;              -- MLDL Status Register, control LEDs
            when "1001" =>                                 -- $x809
              next_MLDLStat <= ROMReadSR;                  -- reserved registers, do nothing
            when others =>
              next_MLDLStat <= ROMReadSR;                  -- reserved registers, do nothing              
          end case;
        end if;                                            
      elsif ((SR(08) = '1') and (SR(09) = '0')) then       -- SRAM, Write Enabled
        next_MLDLStat <= WROMWrite;                        -- write to SRAM
      else
        next_MLDLStat <= ROMReadSR;                        -- do nothing
      end if;                                 
                                                  
    when WROMWrite  =>                --  Write for WROM
      next_MLDLStat <= ROMReadSR;

    when IORead =>
      next_MLDLStat <= Idle;

    when IOWrite =>
      next_MLDLStat <= ROMReadSR;

    when MLDLStatRead =>
      next_MLDLStat <= Idle;

    when MLDLStatWrite =>
      next_MLDLStat <= ROMReadSR;

  end case;
end process;

process (CLK02, ph_reset, X_EN, next_MLDLStat)
begin
  if X_EN = '0' then
    MLDLStat <= MLDL_disable;
  elsif (CLK02'event and CLK02 = '1') then
    MLDLStat <= next_MLDLStat;
  end if ;
end process ;

-------------------------------------------------------------------------------
--           INSTRUCTION DETECTOR                                            --
-------------------------------------------------------------------------------

I_detect: process(I, MLDLStat, ph_count)
begin
-- only when a SYNC has been present
  if (MLDLStat = DecodeInstr) then
   if I = "0001000000" then             --  WROM    $040
      IStat <= I_WROM;
    elsif I = "1100110000" then         --  FETCH   $330
       IStat <= I_FETCH;
    elsif I(5 downto 0) = "000000" and I(9) = '0' and I(8) = '1' then
      IStat <= I_ENBANK;                --  ENBANKx:  $100/$180/$140/$1C0
    else
      IStat <= I_Idle;
    end if;
  elsif (ph_count = "101100") then      -- phase 45
    IStat <= I_Idle;
  end if;
end process;

--    Instructions currently supported:
--       $040     WROM           Write S&X to ROM location
--       $330     FETCH S&X      Reads ROM location, no actions needed
--       $100     ENBANK1        Switches active bank
--       $180     ENBANK2        Switches active bank
--       $140     ENBANK3        Switches active bank
--       $1C0     ENBANK4        Switches active bank

--    Instructions not supported, for possible future use
--       $1F0     WPTOG          Toggle Write Protection (HEPAX)
--                               - need to toggle WP bit in indicated page
--                               - special SR_Read
--                               - toggle bit
--                               - SR_Write
--       $030     BLKMOV         Move page (HEPAX)
--                               - read 4 SR's for HEPAX (all banks)
--                               - move to new SR location
--                               - erase previous SR's
--       $270     RAMSLCT        Select RAM Block in C[2..0]
--       $2F0     WRITDATA       Writes C[13..0] to selected RAM register
--       $038     READDATA       reads selected RAM register to C[13..0]
--       $028     WRIT 0..F      write to RAM register in slected block
--       $038     READ 0..F      read from RAM register in selected block
--       $3F0     PRPHSLCT       Select Peripheral in C[1..0]
--       $024     SELPF 0..F     alow peripheral to take control

-------------------------------------------------------------------------------
--           ENABLED BANK REGISTERS                                          --
-------------------------------------------------------------------------------

EBR_control: process(CLK01, IStat, MLDLStat, ISR, EBR_D, PWO)
-- Controls access to Enabled Bank Registers
begin
  if PWO = '0' then
    EBR(0) <= "00";
    EBR(1) <= "00";
    EBR(2) <= "00";
    EBR(3) <= "00";
    EBR(4) <= "00";
    EBR(5) <= "00";
    EBR(6) <= "00";
    EBR(7) <= "00";
  elsif (CLK01'event and CLK01 = '1') then
    if ((IStat = I_ENBANK) and (MLDLStat = DecodeInstr)) then
      -- works in all Pages, Even and Odd pages share the same EBR!
      EBR(conv_integer(EBR_A)) <= EBR_D;
    end if;
  end if;
end process;

EBR_output: process(EBR, EBR_A)
-- Controls the EBR output for the correct page selection in SR access
begin
  B <= EBR(conv_integer(EBR_A));
end process;

-------------------------------------------------------------------------------
--           ISA INPUT SHIFT REGISTER                                        --
-------------------------------------------------------------------------------

ISA_input: process(CLK01, ISA, ISR, MLDLStat)
-- ISA INPUT SHIFT REGISTER
begin
  if (CLK01'event and CLK01 = '1') then
    -- this construction is chosen to keep the page address in ISR
    -- to facilitate ENBANK instructions
    if (MLDLStat = ReadAddrISA) then
      ISR(00) <= ISR(01); ISR(01) <= ISR(02);
      ISR(02) <= ISR(03); ISR(03) <= ISR(04);
      ISR(04) <= ISR(05); ISR(05) <= ISR(06);
      ISR(06) <= ISR(07); ISR(07) <= ISR(08);
      ISR(08) <= ISR(09); ISR(09) <= ISR(10);
      ISR(10) <= ISR(11); ISR(11) <= ISR(12);
      ISR(12) <= ISR(13); ISR(13) <= ISR(14);
      ISR(14) <= ISR(15); ISR(15) <= ISA;
    elsif ((MLDLStat = ShiftInISA) or (MLDLStat = ShiftOutISA)) then
    -- and (ph_count /= "101100")) then
      ISR(00) <= ISR(01); ISR(01) <= ISR(02);
      ISR(02) <= ISR(03); ISR(03) <= ISR(04);
      ISR(04) <= ISR(05); ISR(05) <= ISR(06);
      ISR(06) <= ISR(07); ISR(07) <= ISR(08);
      ISR(08) <= ISR(09); ISR(09) <= ISA;
    end if;
  end if;
end process;

-------------------------------------------------------------------------------
--           DATA INPUT SHIFT REGISTER                                       --
-------------------------------------------------------------------------------

DATA_clock: process(CLK01, MLDLStat, X_EN, X_STROBE, X_CLK)
-- controls clock for Data shifter
begin
   -- DSR_clk_out is an output, connect externally to DSR_clk !!!
   -- DSR_clk should be a global clock for easier routing
  if ((X_EN = '1') and ((MLDLStat = ShiftInData) or (MLDLStat = ReadAddrISA))) then
    -- input of DATA
    DSR_clk_out <= CLK01;
  elsif (X_EN = '0' and X_STROBE = '1') then  -- no clocks when X_STROBE = '0'
    DSR_clk_out <= X_CLK;
  else                                        -- no clocks at all
    DSR_clk_out <= '0';
  end if;
end process;

DATA_input: process(DATA, DSR, DSR_clk, X_EN, X_DIN)
-- DATA INPUT SHIFT REGISTER
begin
  if (DSR_clk'event and DSR_clk = '1') then   -- regular Data Shift Register
    XSR(00) <= XSR(01);   XSR(01) <= XSR(02);   -- for 16-bit memory access
    XSR(02) <= XSR(03);   XSR(03) <= XSR(04);
    XSR(04) <= XSR(05);   XSR(05) <= XSR(06);
    XSR(06) <= XSR(07);   XSR(07) <= DSR(00);

    DSR(00) <= DSR(01);   DSR(01) <= DSR(02);   -- normal DSR
    DSR(02) <= DSR(03);   DSR(03) <= DSR(04);
    DSR(04) <= DSR(05);   DSR(05) <= DSR(06);
    DSR(06) <= DSR(07);   DSR(07) <= DSR(08);
    DSR(08) <= DSR(09);   DSR(09) <= DSR(10);
    DSR(10) <= DSR(11);   DSR(11) <= DSR(12);
    DSR(12) <= DSR(13);   DSR(13) <= DSR(14);
    DSR(14) <= DSR(15);   DSR(15) <= DSR(16);
    DSR(16) <= DSR(17);   DSR(17) <= DSR(18);
    DSR(18) <= DSR(19);   DSR(19) <= DSR(20);
    DSR(20) <= DSR(21);   DSR(21) <= DSR(22);
    DSR(22) <= DSR(23);   DSR(23) <= DSR(24);
    DSR(24) <= DSR(25);   DSR(25) <= DSR(26);
    DSR(26) <= DSR(27);   DSR(27) <= DSR(28);
    DSR(28) <= DSR(29);
    if (X_EN = '1') then
      DSR(29) <= DATA;     -- normal situation,data from HP41
    else
      DSR(29) <= X_DIN;    -- eXternal Data Input, DSR_clk <= X_CLK !!
    end if;
  end if;
end process;

-------------------------------------------------------------------------------
--           IO SHIFT REGISTER                                       --
-------------------------------------------------------------------------------

IOR_clock: process(X_STROBE, X_CLK, CLK01, MLDLStat, SR, ISR)
-- generates the clock for the IO Register
begin
  if (MLDLStat = IOWrite) then   -- parallel loading of IOR when writing to I/O Register
    OSR_clk_out <= CLK01;
  else
    OSR_clk_out <= X_CLK;
  end if;
end process;

IOSR: process(OSR_clk, IOR, X_DIN, XD, X_EN, X_STROBE, DSR)
begin
  if OSR_clk'event and OSR_clk = '1' then
    if (X_EN = '1' and MLDLStat = IOWrite) then
      -- HP41 loads data in IO Register
      IOR(00) <= DSR(00);   IOR(01) <= DSR(01);
      IOR(02) <= DSR(02);   IOR(03) <= DSR(03);
      IOR(04) <= DSR(04);   IOR(05) <= DSR(05);
      IOR(06) <= DSR(06);   IOR(07) <= DSR(07);
      IOR(08) <= DSR(08);   IOR(09) <= DSR(09);
      IOR(10) <= DSR(10);   IOR(11) <= DSR(11);
      IOR(12) <= DSR(12);   IOR(13) <= DSR(13);
      IOR(14) <= DSR(14);   IOR(15) <= DSR(15);  -- DSR(15) is always here, use other bit?
    elsif (X_EN = '0' and X_STROBE = '0' and DSR(09) = '1') then
      IOR(00) <= XD(00);    IOR(01) <= XD(01);
      IOR(02) <= XD(02);    IOR(03) <= XD(03);
      IOR(04) <= XD(04);    IOR(05) <= XD(05);
      IOR(06) <= XD(06);    IOR(07) <= XD(07);
      IOR(08) <= XD(08);    IOR(09) <= XD(09);
      IOR(10) <= XD(10);    IOR(11) <= XD(11);
      IOR(12) <= XD(12);    IOR(13) <= XD(13);
      IOR(14) <= XD(14);    IOR(15) <= XD(15);
    elsif (X_STROBE = '1') then
      -- serial shift register, always shift data in
      IOR(00) <= IOR(01);   IOR(01) <= IOR(02);
      IOR(02) <= IOR(03);   IOR(03) <= IOR(04);
      IOR(04) <= IOR(05);   IOR(05) <= IOR(06);
      IOR(06) <= IOR(07);   IOR(07) <= IOR(08);
      IOR(08) <= IOR(09);   IOR(09) <= IOR(10);
      IOR(10) <= IOR(11);   IOR(11) <= IOR(12);
      IOR(12) <= IOR(13);   IOR(13) <= IOR(14);
      IOR(14) <= IOR(15);   IOR(15) <= X_DIN;
    end if;
  end if;
end process;

Handshake_XBSY: process(CLK01, X_EN, X_STROBE, X_CLK, MLDLStat, H_DAV)
-- controls H_DAV/X_DAV, indicates that HP41 has written data
-- and that USB has not read the data when high
begin
  if (X_EN = '1' and X_STROBE = '0' and X_CLK = '1' and H_DAV = '0') then
    -- asynchronous reset when USB reads data
    -- data should only be read by USB when no write data was pending
    -- otherwise the handshake mechanism will fail
    H_BSY <= '1';
  elsif (CLK01'event and CLK01 = '0') then
    if (MLDLStat = IORead) then
      H_BSY <= '0';
    end if;
  end if;
end process;

Handshake_X_DAV: process(CLK01, X_EN, X_STROBE, X_CLK, MLDLStat, H_BSY)
-- controls H_BSY/X_BSY, indicates that USB has written data
-- and that HP41 has not read data yet
begin
  if (MLDLStat = IOWrite) then
    H_DAV <= '1';
  elsif (X_STROBE'event and X_STROBE = '1') then
    if (X_EN = '1') then
      H_DAV <= '0';
    end if;
  end if;
end process;

Handshake: process(H_DAV, H_BSY, RY_BY, X_EN, IOR(00))
-- drives X_BSY and X_DAV lines for I/O handshaking
begin
  if X_EN = '0' then
    X_BSY <= RY_BY;    -- the FLASH EPROM READY/BUSY signal
  else
    X_BSY <= H_BSY;    -- normal use
  end if;
  X_DAV <= H_DAV;
  X_DOUT <= IOR(00);
end process;

-------------------------------------------------------------------------------
--           STATUS REGISTER                                                 --
-------------------------------------------------------------------------------

SR_Read: process(CLK01, XD, ISR, MLDLStat)
-- Read and latch the SR contents on rising edge of CLK01 and SR access
begin
--    OLD Settings register layout:
--    SR  | 09 | 08 | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 |
--          EN    0   IO   WP   A17  A16  A15  A14  A13  A12   for SRAM/IO
--          EN    1   A19  A18  A17  A16  A15  A14  A13  A12   for FLASH
--
--        | 09 | 08 | 07 | 06 |
--           0    0    0    0   FLASH, 1st quart
--           0    0    0    1   FLASH, 2nd quart
--           0    0    1    0   FLASH, 3rd quart
--           0    0    1    1   FLASH, 4th quart
--           0    1    0    0   SRAM, write enable
--           0    1    0    1   SRAM, write protected
--           0    1    1    0   IO, mapped to SRAM
--           0    1    1    1   IO, mapped to FLASH (only in 1st quart)
--
--    SR(09):   1- disabled       0- enabled
--    SR(08):   1- SRAM           0- FLASH
--    SR(07):   1- IO Bank        0- SRAM           only if SRAM
--    SR(06):   1- Write Protect  0- Write Enable   only if SRAM
--    SR(06):   1- IO Bank+SRAM   0- IO Bank+FLASH  only if IO bank

-- NEW Settings rgister layout:
-- SR | 11 | 10 | 09 | 08 | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 | 
--      EN   IO   WP FL/SR  A19  A18  A17  A16  A15  A14  A13  A12
--
--    SR(11):   1- disabled       0- enabled
--    SR(10):   1- normal page    0- I/O Page
--    SR(09):   1- Write Protect  0- Write Enable   effective only if SRAM
--    SR(08):   1- SRAM           0- FLASH

  if  (CLK01'event and CLK01 = '0') then
    if (MLDLStat = ROMReadSR) or (MLDLStat = WROMReadSR) then
      SR(00) <= XD(00);       -- A12
      SR(01) <= XD(01);       -- A13
      SR(02) <= XD(02);       -- A14
      SR(03) <= XD(03);       -- A15
      SR(04) <= XD(04);       -- A16
      SR(05) <= XD(05);       -- A17
      SR(06) <= XD(06);       -- A18
      SR(07) <= XD(07);       -- A19
      SR(08) <= XD(08);       -- FLASH or SRAM
      SR(09) <= XD(09);       -- Write Protect
      SR(10) <= XD(10);       -- I/O Page
      SR(11) <= XD(11);       -- Enable/Disable                
    end if;
  end if;
end process;

-------------------------------------------------------------------------------
--           ISA OUTPUT SHIFT REGISTER                                       --
-------------------------------------------------------------------------------

ISA_Output: process (MLDLStat, MLDL_DIS)
begin
  if ((MLDLStat = ShiftOutISA) and (MLDL_DIS = '0')) then
    ISA_OE <= '0';        -- enable  HCT125 Output
  else
    ISA_OE <= 'Z';        -- disable HCT125 Output
  end if;
end process;

ISA_Out <= ISA_O;

ISA_OSR: process (MLDLStat, CLK01, OSR, XD, IOR, SR, H_DAV, H_BSY)
  -- ISA Output Shift Register
  -- read XD into the OSR and shift out ISA
begin
  if (CLK01'event and CLK01 = '0') then

    case MLDLStat is

      when ShiftOutISA =>         -- need to move OSR to the ISA output
        ISA_O   <= OSR(00);       OSR(00) <= OSR(01);
        OSR(01) <= OSR(02);       OSR(02) <= OSR(03);
        OSR(03) <= OSR(04);       OSR(04) <= OSR(05);
        OSR(05) <= OSR(06);       OSR(06) <= OSR(07);
        OSR(07) <= OSR(08);       OSR(08) <= OSR(09);

      when ROMRead =>             -- regular read of ROM
        OSR(00) <= XD(00);        OSR(01) <= XD(01);
        OSR(02) <= XD(02);        OSR(03) <= XD(03);
        OSR(04) <= XD(04);        OSR(05) <= XD(05);
        OSR(06) <= XD(06);        OSR(07) <= XD(07);
        OSR(08) <= XD(08);        OSR(09) <= XD(09);

      when IORead =>              -- read I/O register
        if IOReg = "0111" then
          OSR(00) <= not IOR(00); OSR(01) <= not IOR(01);
          OSR(02) <= not IOR(02); OSR(03) <= not IOR(03);
          OSR(04) <= not IOR(04); OSR(05) <= not IOR(05);
          OSR(06) <= not IOR(06); OSR(07) <= not IOR(07);          
        else
          OSR(00) <= IOR(00);     OSR(01) <= IOR(01);
          OSR(02) <= IOR(02);     OSR(03) <= IOR(03);
          OSR(04) <= IOR(04);     OSR(05) <= IOR(05);
          OSR(06) <= IOR(06);     OSR(07) <= IOR(07);
        end if;         
        OSR(08) <= H_DAV;       OSR(09) <= H_BSY;

      when MLDLStatRead =>          -- read MLDL Status register
        OSR(00) <= not SR_SET(0);   -- SR switches
        OSR(01) <= not SR_SET(1);   -- SR switches
        OSR(02) <= not MLDL_SR;     -- SR from FLASH or SRAM
        OSR(03) <= SPR(0);
        OSR(04) <= SPR(1);
        OSR(05) <= SPR(2);
        OSR(06) <= SPR(3);
        OSR(07) <= '0';
        OSR(08) <= H_DAV;
        OSR(09) <= H_BSY;       -- X_DAV/H_BSY

      when others => NULL;
    end case;
  end if;

end process;

-------------------------------------------------------------------------------
--           MEMORY ACCESS LOGIC                                             --
-------------------------------------------------------------------------------

memory_access: process (MLDLStat, ISR, DSR, XSR, CLK02, SR, X_EN,
                        MLDL_SR, X_STROBE, IOR, B, SR_SET, X_CLK)
-- memory access engine, controls FLASH and SRAM

begin

  case MLDLStat is

    when ROMReadSR =>
      XA(19) <= '1';        XA(18) <= '1';
      XA(17) <= '1';        XA(16) <= '1';
      XA(15) <= '1';        XA(14) <= '1';
      XA(13) <= '1';        XA(12) <= '1';
      XA(11) <= '1';        XA(10) <= '1';
      XA(09) <= '1';        XA(08) <= '1';
      XA(07) <= not SR_SET(1);  XA(06) <= not SR_SET(0);

      XA(05) <= ISR(15);    XA(04) <= ISR(14);
      XA(03) <= ISR(13);    XA(02) <= ISR(12);

      XA(01) <= B(0);       XA(00) <= B(1);         -- fixes Bank Switching bug

      XD <= (others => 'Z');
      SRAM_WE <= '1';     FLASH_WE <= '1';

      if MLDL_SR = '0' then                         -- SR in SRAM
        SRAM_CE <= '0';     FLASH_CE <= '1';
        SRAM_OE <= '0';     FLASH_OE <= '1';
      else                                          -- SR in FLASH
        SRAM_CE <= '1';     FLASH_CE <= '0';
        SRAM_OE <= '1';     FLASH_OE <= '0';
      end if;

    when WROMReadSR =>
      XA(19) <= '1';        XA(18) <= '1';
      XA(17) <= '1';        XA(16) <= '1';
      XA(15) <= '1';        XA(14) <= '1';
      XA(13) <= '1';        XA(12) <= '1';
      XA(11) <= '1';        XA(10) <= '1';
      XA(09) <= '1';        XA(08) <= '1';
      XA(07) <= not SR_SET(1);  XA(06) <= not SR_SET(0);

      XA(05) <= DSR(27);    XA(04) <= DSR(26);
      XA(03) <= DSR(25);    XA(02) <= DSR(24);

      XA(01) <= B(0);       XA(00) <= B(1);         -- fixes Bank Switching bug

      XD <= (others => 'Z');
      SRAM_WE <= '1';     FLASH_WE <= '1';

      if MLDL_SR = '0' then                         -- SR in SRAM
        SRAM_CE <= '0';     FLASH_CE <= '1';
        SRAM_OE <= '0';     FLASH_OE <= '1';
      else                                          -- SR in FLASH
        SRAM_CE <= '1';     FLASH_CE <= '0';
        SRAM_OE <= '1';     FLASH_OE <= '0';
      end if;

    when ROMRead =>
      XD <= (others => 'Z');
      XA(19) <= SR(07);     XA(18) <= SR(06);      
      XA(17) <= SR(05);     XA(16) <= SR(04);
      XA(15) <= SR(03);     XA(14) <= SR(02);
      XA(13) <= SR(01);     XA(12) <= SR(00);
      XA(11) <= ISR(11);    XA(10) <= ISR(10);
      XA(09) <= ISR(09);    XA(08) <= ISR(08);
      XA(07) <= ISR(07);    XA(06) <= ISR(06);
      XA(05) <= ISR(05);    XA(04) <= ISR(04);
      XA(03) <= ISR(03);    XA(02) <= ISR(02);
      XA(01) <= ISR(01);    XA(00) <= ISR(00);

      SRAM_WE <= '1';       FLASH_WE <= '1';

      if SR(08) = '0' then                          
        -- FLASH is addressed or I/O Bank mapped in FLASH
        SRAM_CE <= '1';       FLASH_CE <= CLK02;
        SRAM_OE <= '1';       FLASH_OE <= CLK02;
      else
        -- SRAM is addressed
        SRAM_OE  <= CLK02;    FLASH_OE <= '1';
        SRAM_CE  <= CLK02;    FLASH_CE <= '1';
      end if;

    when WROMWrite =>
      XD(15) <= '0';        XD(14) <= '0';
      XD(13) <= '0';        XD(12) <= '0';
      XD(11) <= '0';        XD(10) <= '0';

      XD(09) <= DSR(09);    XD(08) <= DSR(08);
      XD(07) <= DSR(07);    XD(06) <= DSR(06);
      XD(05) <= DSR(05);    XD(04) <= DSR(04);
      XD(03) <= DSR(03);    XD(02) <= DSR(02);
      XD(01) <= DSR(01);    XD(00) <= DSR(00);
      
      XA(19) <= SR(07);     XA(18) <= SR(06);
      XA(17) <= SR(05);     XA(16) <= SR(04);
      XA(15) <= SR(03);     XA(14) <= SR(02);
      XA(13) <= SR(01);     XA(12) <= SR(00);
      XA(11) <= DSR(23);    XA(10) <= DSR(22);
      XA(09) <= DSR(21);    XA(08) <= DSR(20);
      XA(07) <= DSR(19);    XA(06) <= DSR(18);
      XA(05) <= DSR(17);    XA(04) <= DSR(16);
      XA(03) <= DSR(15);    XA(02) <= DSR(14);
      XA(01) <= DSR(13);    XA(00) <= DSR(12);

      SRAM_OE <= '1';       FLASH_OE <= '1';       -- always in SRAM
      SRAM_CE <= CLK02;     FLASH_CE <= '1';
      SRAM_WE <= CLK02;     FLASH_WE <= '1';

    when MLDL_disable =>
      -- keep feeding the shift registers to the outputs for prototyping
      -- remove in next release, saves power?
      XA(19) <= DSR(29);    XA(18) <= DSR(28);
      XA(17) <= DSR(27);    XA(16) <= DSR(26);
      XA(15) <= DSR(25);    XA(14) <= DSR(24);
      XA(13) <= DSR(23);    XA(12) <= DSR(22);
      XA(11) <= DSR(21);    XA(10) <= DSR(20);
      XA(09) <= DSR(19);    XA(08) <= DSR(18);
      XA(07) <= DSR(17);    XA(06) <= DSR(16);
      XA(05) <= DSR(15);    XA(04) <= DSR(14);
      XA(03) <= DSR(13);    XA(02) <= DSR(12);
      XA(01) <= DSR(11);    XA(00) <= DSR(10);

      if (X_STROBE = '0') then
        -- Address lines are already driven from DSR[29..10]
        if DSR(09) = '0' then                       -- write command
          XD(15) <= DSR(07);    XD(14) <= DSR(06);
          XD(13) <= DSR(05);    XD(12) <= DSR(04);
          XD(11) <= DSR(03);    XD(10) <= DSR(02);

          XD(09) <= DSR(01);    XD(08) <= DSR(00);
          XD(07) <= XSR(07);    XD(06) <= XSR(06);
          XD(05) <= XSR(05);    XD(04) <= XSR(04);
          XD(03) <= XSR(03);    XD(02) <= XSR(02);
          XD(01) <= XSR(01);    XD(00) <= XSR(00);
          SRAM_OE <= '1';       FLASH_OE <= '1';

          if DSR(08) = '0' then                     -- We are talking to FLASH
            SRAM_CE <= '1';         FLASH_CE <= '0';
            SRAM_WE <= '1';         FLASH_WE <= not X_CLK;
           else                                     -- SRAM is addressed
            SRAM_CE <= '0';         FLASH_CE <= '1';
            SRAM_WE  <= not X_CLK;  FLASH_WE <= '1';
          end if;
        else    -- DSR(09) = '1'                    -- read command
          XD <= (others => 'Z');
          SRAM_WE <= '1';       FLASH_WE <= '1';
          if DSR(08) = '0' then                     -- We are talking to FLASH
            SRAM_CE <= '1';         FLASH_CE <= '0';
            SRAM_OE <= '1';         FLASH_OE <= '0';
          else                                      -- SRAM is addressed
            SRAM_CE <= '0';         FLASH_CE <= '1';
            SRAM_OE <= '0';         FLASH_OE <= '1';
          end if;
        end if;
      else                                          -- STROBE = '1'
        SRAM_CE <= '1';       FLASH_CE <= '1';
        SRAM_OE <= '1';       FLASH_OE <= '1';
        SRAM_WE <= '1';       FLASH_WE <= '1';
        XD <= (others => 'Z');
      end if;

    when others =>
      SRAM_CE <= '1';     FLASH_CE <= '1';
      SRAM_OE <= '1';     FLASH_OE <= '1';
      SRAM_WE <= '1';     FLASH_WE <= '1';
      XD <= (others => 'Z');
      XA <= (others => '0');

  end case;

end process;

-------------------------------------------------------------------------------
--             BLINGBLING DRIVER                                             --
-------------------------------------------------------------------------------

spare_signals: process(MLDLStat, CLK01, DSR, SPR)
-- driver for SPARE signals, update when doing MLDLStatWrite
begin
  if (CLK01'event and CLK01 = '1') then
    if (MLDLStat = MLDLStatWrite) then
      SPR(0) <= DSR(04);
      SPR(1) <= DSR(05);
      SPR(2) <= DSR(06);
      SPR(3) <= DSR(07);
    end if;
  end if;
end process;

-- SPARE0 <= SPR(0) xor X_CLK;
-- SPARE1 <= SPR(1) xor H_DAV;
-- SPARE2 <= SPR(2) xor H_BSY;
SPARE3 <= SPR(3);

-------------------------------------------------------------------------------
--             OTHER SIGNALS DRIVER                                          --
-------------------------------------------------------------------------------

  DATA_out <= 'Z';
  DATA_OE  <= 'Z';
  FI_out   <= 'Z';
  FI_OE    <= 'Z';
  SYNC_out <= 'Z';
  SYNC_OE  <= 'Z';
  SPARE0   <= 'Z';
  SPARE1   <= 'Z';
  SPARE2   <= 'Z';
  SPARE3   <= 'Z';

  MEM_RESET <= '1';           -- no reset, implement later????
  SRAM_CE1  <= SRAM_CE;
  SRAM_CE2  <= not SRAM_CE;

end Behavioral;