# HotChocolaBot - Wiring Diagram & Connection Guide

**Version**: 1.0
**Difficulty**: Intermediate
**Estimated Time**: 2-3 hours

⚠️ **SAFETY FIRST**: Disconnect all power before making connections. Never work on live circuits.

## Overview

This document describes the electrical connections for HotChocolaBot. The system uses:
- **5V GPIO signals** from Raspberry Pi to control relays
- **12V DC power** for peristaltic pumps
- **I2C bus** for temperature sensor and LCD display
- **Ground isolation** between Raspberry Pi and pump power

## System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Power Distribution                       │
│                                                              │
│  ┌──────────┐      ┌──────────┐      ┌──────────┐         │
│  │ 5V 3A    │      │ 12V 2A   │      │ Optional │         │
│  │ USB-C    │      │ DC       │      │ Buck     │         │
│  │ (RPi)    │      │ (Pumps)  │      │ Converter│         │
│  └────┬─────┘      └────┬─────┘      └────┬─────┘         │
│       │                 │                  │                │
│       ▼                 ▼                  ▼                │
│  ┌─────────────────────────────────────────────┐           │
│  │         Raspberry Pi 4                      │           │
│  │  ┌──────────────────────────────────────┐   │           │
│  │  │  GPIO Pins (BCM Numbering)           │   │           │
│  │  │  - GPIO 17: Cocoa Pump Relay         │   │           │
│  │  │  - GPIO 27: Milk Pump Relay          │   │           │
│  │  │  - GPIO 22: Sugar Pump Relay         │   │           │
│  │  │  - GPIO 23: Emergency Stop (Input)   │   │           │
│  │  │  - GPIO 24: Status LED (Output)      │   │           │
│  │  │                                       │   │           │
│  │  │  I2C Bus (Pins 3 & 5)                │   │           │
│  │  │  - SDA: GPIO 2 (Pin 3)               │   │           │
│  │  │  - SCL: GPIO 3 (Pin 5)               │   │           │
│  │  │  - Connected to LCD & Temp Sensor    │   │           │
│  │  └──────────────────────────────────────┘   │           │
│  └─────────────────────────────────────────────┘           │
│                                                              │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │ Relay Module │  │ I2C Devices  │  │ Indicators   │     │
│  │ (3-channel)  │  │              │  │              │     │
│  │              │  │ - TMP102     │  │ - E-Stop Btn │     │
│  │ Controls     │  │   (Temp)     │  │ - Status LED │     │
│  │ 12V pumps    │  │ - LCD1602    │  │              │     │
│  └──────┬───────┘  └──────────────┘  └──────────────┘     │
│         │                                                   │
│         ▼                                                   │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │ Cocoa Pump   │  │ Milk Pump    │  │ Sugar Pump   │     │
│  │ (12V)        │  │ (12V)        │  │ (12V)        │     │
│  └──────────────┘  └──────────────┘  └──────────────┘     │
└─────────────────────────────────────────────────────────────┘
```

## Pin Assignment Reference

### Raspberry Pi GPIO (BCM Numbering)

| BCM GPIO | Physical Pin | Function | Direction | Connected To |
|----------|--------------|----------|-----------|--------------|
| GPIO 2 (SDA) | Pin 3 | I2C Data | Bidirectional | LCD, Temp Sensor |
| GPIO 3 (SCL) | Pin 5 | I2C Clock | Output | LCD, Temp Sensor |
| GPIO 17 | Pin 11 | Cocoa Pump | Output | Relay 1 Signal |
| GPIO 27 | Pin 13 | Milk Pump | Output | Relay 2 Signal |
| GPIO 22 | Pin 15 | Sugar Pump | Output | Relay 3 Signal |
| GPIO 23 | Pin 16 | Emergency Stop | Input (Pull-up) | E-Stop Button |
| GPIO 24 | Pin 18 | Status LED | Output | LED + Resistor |
| GND | Pin 6, 9, 14, 20, 25, 30, 34, 39 | Ground | - | Common ground |
| 3.3V | Pin 1, 17 | Power | Output | I2C pull-ups (if needed) |
| 5V | Pin 2, 4 | Power | Output | Relay module VCC |

### I2C Device Addresses

| Device | Default I2C Address | Configurable? | Purpose |
|--------|---------------------|---------------|---------|
| TMP102 Temperature Sensor | 0x48 | Yes (0x48-0x4B) | Liquid temperature monitoring |
| LCD1602 with PCF8574 | 0x27 or 0x3F | Depends on backpack | Status display |

## Detailed Connection Instructions

### Section 1: Power Supply Setup

#### 1.1 Raspberry Pi Power (5V)

```
┌──────────────┐
│  5V 3A       │
│  USB-C PSU   │
└──────┬───────┘
       │
       ▼
┌──────────────────┐
│  Raspberry Pi 4  │
│  USB-C Port      │
└──────────────────┘
```

**Steps**:
1. Use official Raspberry Pi 5V 3A USB-C power supply
2. Connect directly to Raspberry Pi USB-C port
3. Do NOT power on yet

#### 1.2 Pump Power (12V)

```
┌──────────────┐
│  12V 2A      │
│  DC Adapter  │
└──────┬───────┘
       │
       ▼ (DC Barrel Jack)
┌──────────────────┐
│  Terminal Block  │
│  + and - rails   │
└──────┬───────────┘
       │
       ├─────────► Relay Module VCC (12V)
       │
       ├─────────► Common for all pump supplies
       │
       └─────────► Share GND with Raspberry Pi GND
                   (⚠️ Single ground point only!)
```

**Steps**:
1. Connect 12V PSU to DC barrel jack
2. Wire jack to terminal block (+ and -)
3. **IMPORTANT**: Connect 12V GND to ONE Raspberry Pi GND pin only
4. Do NOT power on yet

### Section 2: Relay Module Connections

The relay module switches 12V power to the pumps based on 5V GPIO signals.

#### 2.1 Relay Module Control Signals

```
Raspberry Pi GPIO          Relay Module
─────────────────         ─────────────

Pin 2 (5V)      ─────────► VCC (5V power for relay logic)
Pin 6 (GND)     ─────────► GND (common ground)

Pin 11 (GPIO17) ─────────► IN1 (Cocoa pump relay trigger)
Pin 13 (GPIO27) ─────────► IN2 (Milk pump relay trigger)
Pin 15 (GPIO22) ─────────► IN3 (Sugar pump relay trigger)
```

**Steps**:
1. Connect 5V from RPi Pin 2 to relay module VCC
2. Connect GND from RPi Pin 6 to relay module GND
3. Connect GPIO 17 (Pin 11) to relay IN1
4. Connect GPIO 27 (Pin 13) to relay IN2
5. Connect GPIO 22 (Pin 15) to relay IN3

#### 2.2 Relay Module High-Voltage Side (12V Pump Control)

Each relay has three terminals: COM (common), NO (normally open), NC (normally closed).
We use COM and NO for active-high switching.

```
Relay 1 (Cocoa Pump):
  12V+ ─────────► COM
  NO ───────────► Cocoa Pump +
  Pump - ───────► 12V GND

Relay 2 (Milk Pump):
  12V+ ─────────► COM
  NO ───────────► Milk Pump +
  Pump - ───────► 12V GND

Relay 3 (Sugar Pump):
  12V+ ─────────► COM
  NO ───────────► Sugar Pump +
  Pump - ───────► 12V GND
```

**Steps**:
1. Connect 12V+ to COM terminal of all 3 relays
2. Connect NO terminal of Relay 1 to Cocoa pump positive wire
3. Connect NO terminal of Relay 2 to Milk pump positive wire
4. Connect NO terminal of Relay 3 to Sugar pump positive wire
5. Connect all pump negative wires to 12V GND

### Section 3: I2C Devices

#### 3.1 I2C Bus Wiring

I2C is a two-wire bus (SDA and SCL) that allows multiple devices on the same lines.

```
Raspberry Pi              TMP102 Sensor           LCD1602 (I2C)
────────────              ─────────────           ─────────────

Pin 3 (SDA) ─────┬───────► SDA                ┌──► SDA
Pin 5 (SCL) ────┬┼───────► SCL                │┌─► SCL
Pin 1 (3.3V) ───┼┼───────► VCC (3.3V)         ││
Pin 6 (GND) ────┼┼───────► GND                ││
                ││                             ││
                │└──────────────────────────────┘│
                └────────────────────────────────┘
```

**IMPORTANT**:
- TMP102 uses **3.3V** (NOT 5V)
- LCD backpack typically uses **5V** (check your module)
- Both share same SDA/SCL lines
- Each device has unique I2C address

**Steps**:

**For TMP102 Temperature Sensor**:
1. Connect TMP102 VCC to RPi Pin 1 (3.3V)
2. Connect TMP102 GND to RPi Pin 6 (GND)
3. Connect TMP102 SDA to RPi Pin 3 (GPIO 2)
4. Connect TMP102 SCL to RPi Pin 5 (GPIO 3)

**For LCD1602 with I2C Backpack**:
1. Connect LCD VCC to RPi Pin 2 (5V) - or 3.3V if module supports it
2. Connect LCD GND to RPi Pin 9 (GND)
3. Connect LCD SDA to RPi Pin 3 (GPIO 2) - shared with TMP102
4. Connect LCD SCL to RPi Pin 5 (GPIO 3) - shared with TMP102

#### 3.2 I2C Address Configuration

Check I2C addresses after wiring:

```bash
sudo apt-get install i2c-tools
sudo i2cdetect -y 1
```

Expected output:
```
     0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
00:          -- -- -- -- -- -- -- -- -- -- -- -- --
10: -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
20: -- -- -- -- -- -- -- 27 -- -- -- -- -- -- -- --
30: -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 3f
40: -- -- -- -- -- -- -- -- 48 -- -- -- -- -- -- --
```

- `0x27` or `0x3F`: LCD display
- `0x48`: TMP102 temperature sensor

Update `config.toml` if addresses differ.

### Section 4: Emergency Stop Button

```
Raspberry Pi              Emergency Stop Button
────────────              ─────────────────────

Pin 16 (GPIO23) ─────────► Terminal 1 (NO)
Pin 14 (GND) ────────────► Terminal 2 (COM)
```

**Button Configuration**: Normally Open (NO)
- Unpressed: GPIO23 pulled HIGH (internal pull-up)
- Pressed: GPIO23 connected to GND → reads LOW

**Steps**:
1. Connect one terminal of E-Stop button to GPIO 23 (Pin 16)
2. Connect other terminal to GND (Pin 14)
3. Software will enable internal pull-up resistor

**Alternative**: Use Normally Closed (NC) for fail-safe behavior.

### Section 5: Status LED

```
Raspberry Pi              Status LED
────────────              ──────────

Pin 18 (GPIO24) ─────► 220Ω Resistor ─────► LED Anode (+)
Pin 20 (GND) ────────────────────────────► LED Cathode (-)
```

**Steps**:
1. Insert 220Ω resistor in series with LED anode (long leg)
2. Connect resistor to GPIO 24 (Pin 18)
3. Connect LED cathode (short leg) to GND (Pin 20)
4. LED will light when GPIO24 is HIGH

## Complete Wiring Checklist

### Power
- [ ] 5V USB-C PSU connected to Raspberry Pi (NOT powered on)
- [ ] 12V DC PSU connected to barrel jack → terminal block
- [ ] 12V GND connected to ONE Raspberry Pi GND pin
- [ ] All grounds connected to common point

### GPIO Outputs (Relays and LED)
- [ ] GPIO 17 (Pin 11) → Relay IN1 (Cocoa)
- [ ] GPIO 27 (Pin 13) → Relay IN2 (Milk)
- [ ] GPIO 22 (Pin 15) → Relay IN3 (Sugar)
- [ ] GPIO 24 (Pin 18) → 220Ω resistor → LED

### GPIO Inputs
- [ ] GPIO 23 (Pin 16) → Emergency Stop button

### I2C Bus
- [ ] GPIO 2 (Pin 3) → SDA (shared: TMP102, LCD)
- [ ] GPIO 3 (Pin 5) → SCL (shared: TMP102, LCD)
- [ ] TMP102 powered by 3.3V (Pin 1)
- [ ] LCD powered by 5V (Pin 2) - or 3.3V if compatible

### Relay High-Voltage Side
- [ ] 12V+ → All relay COM terminals
- [ ] Relay 1 NO → Cocoa pump +
- [ ] Relay 2 NO → Milk pump +
- [ ] Relay 3 NO → Sugar pump +
- [ ] All pump - → 12V GND

### Pumps
- [ ] Cocoa pump wired to Relay 1
- [ ] Milk pump wired to Relay 2
- [ ] Sugar pump wired to Relay 3
- [ ] All pumps have tubing connected
- [ ] Check valves installed (prevent backflow)

## Testing Procedure

### Pre-Power Checks

1. **Visual Inspection**:
   - No loose wires
   - No shorts between power and ground
   - Correct polarity on all connections
   - Relays in correct orientation

2. **Continuity Testing** (with multimeter, power OFF):
   - Check GPIO to relay signal continuity
   - Verify ground connections
   - Check I2C line continuity

### Initial Power-Up

1. **Connect 12V pump supply** (Raspberry Pi still OFF)
   - Check voltage at terminal block: should read ~12V
   - Relays should be OFF (pumps not running)

2. **Power on Raspberry Pi**
   - Boot and verify OS loads
   - Check I2C devices detected: `sudo i2cdetect -y 1`
   - Verify GPIO pins not triggering relays (should be LOW/OFF initially)

3. **Software Test**:
   ```bash
   cd ~/the-hotchocolabot
   cargo run
   ```
   - Watch for successful hardware initialization
   - LCD should display "HotChocolaBot Ready!"
   - Status LED should blink

### Component Testing

Test each component individually before full operation:

```bash
# Test GPIO outputs (relays)
# (Software should provide test mode)

# Read temperature sensor
# (Check temperature reading in logs)

# Test LCD display
# (Verify messages appear)

# Test emergency stop
# (Press button, verify system stops)
```

## Troubleshooting

### Issue: Relays clicking but pumps not running

- **Check**: 12V power supply voltage
- **Check**: Pump connections to relay NO terminals
- **Check**: Pump ground connections

### Issue: I2C devices not detected

- **Check**: SDA/SCL wiring
- **Check**: Device power (3.3V for TMP102, 5V for LCD)
- **Check**: I2C enabled in `raspi-config`
- **Run**: `sudo i2cdetect -y 1` to scan bus

### Issue: GPIO pins not controlling relays

- **Check**: 5V power to relay module
- **Check**: GPIO signal wiring
- **Check**: Relay trigger voltage (some need 3.3V, some need 5V)

### Issue: Emergency stop not triggering

- **Check**: Button wiring
- **Check**: Internal pull-up enabled in software
- **Check**: Button type (NO vs NC)

## Safety Notes

⚠️ **Critical Safety Requirements**:

1. **Electrical**:
   - Never work on powered circuits
   - Use insulated tools
   - Keep liquids away from electronics
   - Ensure proper grounding

2. **Mechanical**:
   - Secure all components to prevent movement
   - Use cable ties for strain relief
   - Label all connections

3. **Operational**:
   - Emergency stop must be accessible
   - Do not operate unattended
   - Supervise all student interactions

## Next Steps

After completing wiring:
1. Review `assembly_instructions.md` for mechanical assembly
2. Follow software setup in main `README.md`
3. Test each subsystem individually
4. Perform full system integration test
5. Calibrate pump timings in `config.toml`

## References

- Raspberry Pi GPIO Pinout: https://pinout.xyz
- I2C Device Addresses: Check component datasheets
- Relay Module Documentation: Varies by manufacturer
