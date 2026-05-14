# HotChocolaBot - Assembly Instructions

**Version**: 1.0
**Difficulty**: Intermediate
**Estimated Time**: 3-5 hours (first build)
**Team Size**: 1-2 people

## Before You Begin

### Required Tools

- [ ] Screwdriver set (Phillips and flat)
- [ ] Wire strippers
- [ ] Wire cutters
- [ ] Multimeter (for testing)
- [ ] Soldering iron (optional, for permanent connections)
- [ ] Heat gun or lighter (for heat shrink)
- [ ] Drill with bits (if modifying enclosure)
- [ ] Marker/label maker
- [ ] Safety glasses

### Required Parts

Refer to `hardware/bom/parts_list.md` for complete component list.

### Safety Precautions

⚠️ **IMPORTANT**:
- Wear safety glasses when cutting/drilling
- Work in well-ventilated area if soldering
- Keep liquids away from electronics during assembly
- Disconnect all power before making changes
- Use insulated tools near live circuits

### Workspace Setup

- **Clean, dry work surface** with good lighting
- **Anti-static mat** (recommended for electronics)
- **Component organizer** (tackle box or compartmented tray)
- **Cable management supplies** (cable ties, velcro)

## Assembly Process Overview

```
Phase 1: Prepare Enclosure (30-45 min)
   ↓
Phase 2: Mount Fixed Components (45-60 min)
   ↓
Phase 3: Electrical Wiring (90-120 min)
   ↓
Phase 4: Plumbing Setup (30-45 min)
   ↓
Phase 5: Testing & Calibration (60-90 min)
   ↓
Phase 6: Final Assembly & Documentation (30 min)
```

---

## Phase 1: Prepare Enclosure (30-45 min)

### 1.1 Select and Modify Enclosure

**Recommended**: Clear acrylic A4 storage box (educational visibility)

#### Holes to Drill:

1. **Front Panel**:
   - Emergency stop button (22mm hole)
   - Status LED (5mm hole)
   - LCD display cutout (71mm × 25mm rectangle)

2. **Side Panels**:
   - 3× Pump tube pass-throughs (6mm holes)
   - Power cable entry (10mm grommet)

3. **Top/Rear Panel**:
   - Ventilation holes (optional, 6-8 × 5mm holes)

#### Procedure:

```
Step 1: Mark hole positions with marker
Step 2: Tape over marking to prevent cracking
Step 3: Start with pilot hole (2mm bit)
Step 4: Gradually increase bit size to final diameter
Step 5: Smooth edges with file or sandpaper
Step 6: Clean enclosure of plastic shavings
```

### 1.2 Install Mounting Hardware

1. **Raspberry Pi Standoffs**:
   - Use M3 × 6mm standoffs
   - Position in lower-left area of enclosure base
   - Mark and drill M3 mounting holes
   - Secure with M3 screws

2. **Relay Module Mounting**:
   - Use velcro dots or M3 standoffs
   - Position near Raspberry Pi
   - Ensure relay switch side accessible

3. **Component Positioning Guide**:
   ```
   ┌─────────────────────────────────────┐
   │ Enclosure Top View                  │
   │                                     │
   │  [Containers]   [Containers]        │
   │     Milk         Cocoa    Sugar     │
   │      ▼            ▼        ▼        │
   │    ┌────┐      ┌────┐   ┌────┐     │
   │    │Pump│      │Pump│   │Pump│     │
   │    └────┘      └────┘   └────┘     │
   │                                     │
   │    ┌─────────┐  ┌──────────┐       │
   │    │ Relays  │  │   RPi 4  │       │
   │    └─────────┘  └──────────┘       │
   │                                     │
   │    ┌──────┐    ┌───────────┐       │
   │    │ 12V  │    │   Temp    │       │
   │    │ PSU  │    │  Sensor   │       │
   │    └──────┘    └───────────┘       │
   │                                     │
   └─────────────────────────────────────┘
   ```

---

## Phase 2: Mount Fixed Components (45-60 min)

### 2.1 Mount Raspberry Pi

1. Attach Raspberry Pi to standoffs using M3 × 6mm screws
2. Ensure Pi is level and secure
3. Orient with GPIO pins accessible for wiring
4. Do NOT insert SD card or power yet

### 2.2 Install Emergency Stop Button

1. Insert button through front panel hole
2. Secure with retaining nut from inside
3. Attach wire leads to NO (Normally Open) terminals
4. Label wires: "GPIO23" and "GND"

### 2.3 Mount Status LED

1. Insert LED through 5mm hole in front panel
2. Secure with hot glue or LED holder
3. Attach resistor to anode (positive, long leg)
4. Label wires: "GPIO24" and "GND"

### 2.4 Install LCD Display

**Method 1** (Flush mount):
1. Remove LCD from PCB if possible
2. Mount LCD in rectangular cutout
3. Secure PCB inside enclosure
4. Connect with ribbon cable or wires

**Method 2** (Simple mount):
1. Mount entire LCD module to inside of front panel
2. Align with cutout for visibility
3. Secure with M3 screws or hot glue

4. Label I2C connections: "SDA", "SCL", "5V", "GND"

### 2.5 Mount Relay Module

1. Attach relay module using velcro or standoffs
2. Position with terminals accessible
3. Orient so IN1/IN2/IN3 labels are visible
4. Leave space for 12V power wiring

### 2.6 Install Temperature Sensor

**If using TMP102 breakout board**:
1. Position sensor near liquid path (but not in contact)
2. Mount with velcro or small standoffs
3. Ensure I2C wires can reach Raspberry Pi
4. Label connections: "SDA", "SCL", "3.3V", "GND"

---

## Phase 3: Electrical Wiring (90-120 min)

**IMPORTANT**: Follow `wiring_diagram.md` for detailed connection guide.

### 3.1 Prepare Wires

Cut wires to appropriate lengths:
- **GPIO to relays**: 10-15 cm
- **I2C connections**: 15-20 cm
- **Power lines**: As needed for routing
- **Pump power**: 20-30 cm

Strip 5mm from each wire end. Use heat shrink for insulation.

### 3.2 Wire Raspberry Pi GPIO Outputs

Using male-to-female jumper wires:

1. **Relay Control**:
   ```
   GPIO 17 (Pin 11) → Relay IN1 (red wire)
   GPIO 27 (Pin 13) → Relay IN2 (blue wire)
   GPIO 22 (Pin 15) → Relay IN3 (green wire)
   ```

2. **Status LED**:
   ```
   GPIO 24 (Pin 18) → 220Ω resistor → LED anode
   GND (Pin 20) → LED cathode
   ```

3. **Emergency Stop**:
   ```
   GPIO 23 (Pin 16) → E-Stop terminal 1
   GND (Pin 14) → E-Stop terminal 2
   ```

### 3.3 Wire I2C Bus

**TMP102 Temperature Sensor**:
```
RPi Pin 1 (3.3V) → TMP102 VCC
RPi Pin 3 (SDA)  → TMP102 SDA
RPi Pin 5 (SCL)  → TMP102 SCL
RPi Pin 6 (GND)  → TMP102 GND
```

**LCD Display**:
```
RPi Pin 2 (5V)   → LCD VCC (or 3.3V if compatible)
RPi Pin 3 (SDA)  → LCD SDA (shared with TMP102)
RPi Pin 5 (SCL)  → LCD SCL (shared with TMP102)
RPi Pin 9 (GND)  → LCD GND
```

### 3.4 Wire Relay Module

**Low-voltage side** (control signals):
```
RPi Pin 2 (5V)   → Relay VCC
RPi Pin 6 (GND)  → Relay GND
GPIO 17          → Relay IN1 (already connected in 3.2)
GPIO 27          → Relay IN2
GPIO 22          → Relay IN3
```

**High-voltage side** (pump power) - see Phase 3.5

### 3.5 Wire Power Distribution

#### 12V Power Supply Setup:

1. **Connect 12V PSU to terminal block**:
   ```
   12V PSU + → Terminal block + rail (red)
   12V PSU - → Terminal block - rail (black)
   ```

2. **Connect 12V to Relays**:
   ```
   Terminal + → Relay 1 COM
   Terminal + → Relay 2 COM
   Terminal + → Relay 3 COM
   ```

3. **Connect Relays to Pumps**:
   ```
   Relay 1 NO → Cocoa Pump + (red wire)
   Relay 2 NO → Milk Pump + (blue wire)
   Relay 3 NO → Sugar Pump + (green wire)
   ```

4. **Connect Pump Grounds**:
   ```
   Cocoa Pump - → Terminal - (black wire)
   Milk Pump -  → Terminal - (black wire)
   Sugar Pump - → Terminal - (black wire)
   ```

5. **Common Ground Connection**:
   ```
   Terminal - (12V GND) → RPi Pin 25 (GND)
   ```
   ⚠️ **Use ONLY ONE ground connection between 12V and RPi**

### 3.6 Cable Management

1. Bundle related wires with cable ties
2. Use different colored wires or labels:
   - **Red**: +12V, +5V, +3.3V
   - **Black**: GND
   - **Colored**: GPIO signals (use different colors per pump)
3. Leave slack for maintenance
4. Route wires away from moving parts (pumps)
5. Secure bundles with velcro to enclosure walls

---

## Phase 4: Plumbing Setup (30-45 min)

### 4.1 Prepare Ingredient Containers

1. **Clean thoroughly** with hot soapy water
2. **Drill lid** for tubing pass-through (5mm hole)
3. **Insert tubing** through lid (~5cm into liquid)
4. **Seal** hole with hot glue or grommet
5. **Label** each container: "COCOA", "MILK", "SUGAR"

### 4.2 Install Peristaltic Pumps

1. **Mount pumps** to enclosure base with velcro or screws
2. **Thread tubing** through pump mechanism:
   - Follow pump manufacturer's instructions
   - Ensure tubing is fully seated in rollers
   - Direction matters! Check pump rotation vs. flow direction

3. **Connect tubing**:
   ```
   Container → Inlet tubing → Pump → Outlet tubing → Dispense nozzle
   ```

4. **Install check valves** (prevent backflow):
   - Place on outlet side of each pump
   - Ensure arrow points toward dispense end

### 4.3 Dispense Nozzle Assembly

**Option 1** (Simple):
- Route all three outlet tubes to common exit point
- Use cable ties to bundle
- Cut tubes to same length for simultaneous dispensing

**Option 2** (Sequenced):
- Route tubes to different heights
- Allows layered dispensing (e.g., milk → cocoa → sugar)

### 4.4 Tubing Organization

1. Measure and cut tubing to appropriate lengths:
   - **Inlet**: Container to pump (~15-20 cm)
   - **Outlet**: Pump to nozzle (~25-30 cm)

2. Use cable ties to prevent kinking
3. Ensure no sharp bends that restrict flow
4. Label each tube at both ends

---

## Phase 5: Testing & Calibration (60-90 min)

### 5.1 Pre-Power Visual Inspection

- [ ] All connections secure
- [ ] No exposed wire touching metal/other wires
- [ ] Correct polarity on all power connections
- [ ] Emergency stop accessible
- [ ] Pumps correctly oriented

### 5.2 Continuity Testing (Power OFF)

Using multimeter in continuity mode:
1. **GPIO to relay**: Beep between GPIO pin and relay INx
2. **I2C lines**: Verify SDA/SCL continuity
3. **Ground**: Check all GND points connected
4. **No shorts**: Verify no beep between +5V and GND, +12V and GND

### 5.3 Initial Power-Up

#### Step 1: Power the 12V supply ONLY

1. Plug in 12V PSU
2. Measure voltage at terminal block: should read ~12V
3. Relays should be OFF (pumps silent)
4. If anything unexpected happens, IMMEDIATELY UNPLUG

#### Step 2: Power Raspberry Pi

1. Insert SD card with Raspbian OS
2. Connect 5V USB-C power
3. Watch boot sequence
4. Wait for desktop or SSH access

#### Step 3: Verify I2C Devices

```bash
# Enable I2C in raspi-config if not already enabled
sudo raspi-config
# Navigate to: Interfacing Options → I2C → Enable

# Install I2C tools
sudo apt-get update
sudo apt-get install -y i2c-tools

# Scan I2C bus
sudo i2cdetect -y 1
```

Expected output shows addresses `0x27` (or `0x3F`) and `0x48`.

### 5.4 Software Installation & Testing

```bash
# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source $HOME/.cargo/env

# Clone repository
git clone https://github.com/Hyperpolymath/the-hotchocolabot.git
cd the-hotchocolabot

# Copy and edit configuration
cp config.toml.example config.toml
nano config.toml  # Adjust I2C addresses if different

# Run tests
cargo test

# Run in test mode (without dispensing)
RUST_LOG=debug cargo run
```

Watch for:
- [x] "Temperature sensors operational" ✓
- [x] "All pumps connected and responsive" ✓
- [x] LCD displays "HotChocolaBot Ready!" ✓

### 5.5 Component Testing

Test each component individually:

#### Test 1: Emergency Stop
1. Run software
2. Press emergency stop button
3. Verify system logs "EMERGENCY STOP TRIGGERED"
4. Release button, reset in software

#### Test 2: Status LED
1. LED should blink on startup
2. Verify solid on when ready
3. Test emergency stop triggers LED pattern

#### Test 3: Temperature Sensor
1. Check temperature reading in logs
2. Should show room temperature (~18-25°C)
3. Touch sensor to warm - reading should increase

#### Test 4: LCD Display
1. Verify boot message appears
2. Check character clarity
3. If garbled, adjust I2C address or contrast pot

#### Test 5: Individual Pump Test

**IMPORTANT**: Use water for initial testing, NOT actual ingredients.

```bash
# Edit config.toml to enable test mode
[education]
challenge_mode = false
show_internals = true
```

Manual pump test:
1. Fill one container with water
2. Run software in test mode
3. Observe pump activation
4. Measure dispensed volume
5. Repeat for each pump

### 5.6 Calibration

#### Pump Flow Rate Calibration:

1. **Measure actual flow**:
   - Run pump for 1000ms
   - Measure dispensed volume (use graduated cylinder)
   - Calculate: mL/second

2. **Adjust config.toml**:
   ```toml
   [recipes.standard]
   cocoa_ms = 2000  # Adjust based on desired volume
   milk_ms = 5000
   sugar_ms = 1000
   ```

3. **Test standard recipe**:
   - Dispense using water
   - Measure total volumes:
     - Cocoa: ~20-30 mL
     - Milk: ~100-150 mL
     - Sugar: ~10-15 mL
   - Adjust timings until satisfied

#### Temperature Calibration:

Compare TMP102 reading to known thermometer:
- If offset exists, note in documentation
- Or apply software compensation in code

---

## Phase 6: Final Assembly & Documentation (30 min)

### 6.1 Secure All Components

1. Tighten all screws
2. Apply final cable ties
3. Add labels to all connections
4. Close enclosure (but keep accessible for maintenance)

### 6.2 Create Component Map

Draw or photograph final layout. Label:
- Each pump (Cocoa, Milk, Sugar)
- Power supplies
- Raspberry Pi
- Relay module
- All wire colors and destinations

Tape this inside enclosure lid for future reference.

### 6.3 Create Maintenance Checklist

Document regular maintenance tasks:
- [ ] Weekly: Check tube connections for leaks
- [ ] Weekly: Clean dispensing nozzle
- [ ] Monthly: Flush pump tubing
- [ ] Monthly: Inspect electrical connections
- [ ] Per workshop: Refill ingredient containers

### 6.4 Safety Label

Create and attach safety label:

```
┌──────────────────────────────────────┐
│        ⚠️  SAFETY NOTICE  ⚠️           │
│                                      │
│ • Emergency stop button: [LOCATION]  │
│ • 12V electrical hazard inside       │
│ • Do not operate unattended          │
│ • Supervise students at all times    │
│ • Use food-safe ingredients only     │
│                                      │
│ In emergency: PRESS E-STOP BUTTON    │
│ For issues: [CONTACT INFO]           │
└──────────────────────────────────────┘
```

---

## Troubleshooting Guide

### Problem: Pump doesn't run

**Check**:
- Relay clicking? (If yes, check 12V supply to pump)
- GPIO signal reaching relay? (Use multimeter or LED)
- Pump polarity correct?
- Tubing installed correctly in pump?

### Problem: Pump runs but no flow

**Check**:
- Tubing kinked or blocked?
- Check valve orientation correct?
- Air bubbles in line? (Prime pump)
- Inlet tube submerged in liquid?

### Problem: LCD display blank

**Check**:
- Power connected? (Measure voltage at VCC pin)
- I2C address correct? (Run `i2cdetect`)
- Contrast adjustment? (Trim pot on backpack)
- SDA/SCL swapped?

### Problem: Temperature reading incorrect

**Check**:
- I2C address conflict?
- 3.3V power (NOT 5V)?
- Sensor type matches code?
- Wiring correct?

### Problem: Emergency stop doesn't trigger

**Check**:
- Button wiring (NO vs NC terminals)
- GPIO23 pull-up enabled in software?
- Button functional? (Test with multimeter)

---

## Next Steps

1. **Perform full system test** with water
2. **Calibrate pump timings** for desired recipe
3. **Prepare ingredients** (cocoa powder, milk, sugar)
4. **Test with real ingredients** (small batch)
5. **Clean system** after testing
6. **Prepare for workshop delivery**

---

## Resources

- Wiring diagram: `hardware/schematics/wiring_diagram.md`
- Parts list: `hardware/bom/parts_list.md`
- Software setup: `README.md`
- Configuration guide: `config.toml.example`

---

## Appendix: Quick Reference

### GPIO Pin Summary (BCM)

| Function | GPIO | Physical Pin |
|----------|------|--------------|
| Cocoa Pump | 17 | 11 |
| Milk Pump | 27 | 13 |
| Sugar Pump | 22 | 15 |
| E-Stop Input | 23 | 16 |
| Status LED | 24 | 18 |
| I2C SDA | 2 | 3 |
| I2C SCL | 3 | 5 |

### I2C Addresses

- TMP102: 0x48
- LCD: 0x27 or 0x3F

### Power Requirements

- Raspberry Pi: 5V 3A (15W)
- Pumps (3×): 12V 2A total (24W)
- **Total**: ~40W maximum

---

**Assembly Complete!**

Congratulations! Your HotChocolaBot is now assembled. Proceed to software configuration and workshop preparation.

Questions? Issues? See main README.md or open an issue on GitHub.
