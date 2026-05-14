# HotChocolaBot - Bill of Materials (BOM)

**Version**: 1.0
**Last Updated**: November 2024
**Target Budget**: £500-750

## Core Components

### Computing & Control

| Component | Specification | Quantity | Unit Price (£) | Supplier | Part Number | Notes |
|-----------|--------------|----------|----------------|----------|-------------|-------|
| Raspberry Pi 4 | 4GB RAM | 1 | £55-65 | Pimoroni, The Pi Hut | RPI4-MODBP-4GB | 2GB sufficient but 4GB recommended |
| MicroSD Card | 32GB Class 10 | 1 | £8-12 | Amazon, Pimoroni | SanDisk Ultra 32GB | For OS and software |
| Power Supply | 5V 3A USB-C | 1 | £8 | Pimoroni | PSU-USBC-UKP-5V3A | Official RPi power supply |

**Subtotal**: £71-85

### Pumps & Liquid Handling

| Component | Specification | Quantity | Unit Price (£) | Supplier | Part Number | Notes |
|-----------|--------------|----------|----------------|----------|-------------|-------|
| Peristaltic Pump | 12V DC, Food-grade | 3 | £15-25 | Amazon, eBay | Generic 12V Peristaltic | Search "12V food-grade peristaltic pump" |
| Food-Grade Tubing | Silicone, 4mm ID | 3m | £8-12 | Amazon | Food-grade silicone tube | Compatible with pump |
| Container/Reservoir | Food-safe plastic | 3 | £5-10 | IKEA, Amazon | IKEA KORKEN jars | For cocoa, milk, sugar |
| Check Valves | One-way valve, 4mm | 3 | £2-4 | Amazon | Aquarium check valve | Prevent backflow |

**Subtotal**: £66-117 (for 3 ingredient lines)

### Electronics & Control

| Component | Specification | Quantity | Unit Price (£) | Supplier | Part Number | Notes |
|-----------|--------------|----------|----------------|----------|-------------|-------|
| Relay Module | 3-channel 5V relay | 1 | £8-12 | Amazon, Pimoroni | SainSmart 3CH Relay | Or 3× single relays |
| 12V Power Supply | 12V 2A DC adapter | 1 | £8-12 | Amazon | 12V 2A UK plug PSU | For pumps |
| DC Barrel Jack | Female, PCB mount | 1 | £1-2 | Pimoroni, Amazon | Generic 5.5×2.1mm | For 12V input |
| Buck Converter | 12V to 5V (optional) | 1 | £3-5 | Amazon | LM2596 module | If powering RPi from 12V |

**Subtotal**: £20-31

### Sensors & Display

| Component | Specification | Quantity | Unit Price (£) | Supplier | Part Number | Notes |
|-----------|--------------|----------|----------------|----------|-------------|-------|
| Temperature Sensor | TMP102 or DS18B20 | 1 | £3-6 | Pimoroni, Adafruit | TMP102 breakout | I2C interface preferred |
| LCD Display | 16×2 with I2C backpack | 1 | £5-8 | Amazon, Pimoroni | LCD1602 I2C | Blue or green backlight |
| Emergency Stop Button | Mushroom head, NO/NC | 1 | £4-8 | Amazon, RS Components | Red mushroom e-stop | Momentary or latching |
| Status LED | 5mm red LED | 1 | £0.20 | Pimoroni, Amazon | Generic red LED | With 220Ω resistor |
| LED Resistor | 220Ω 1/4W | 1 | £0.10 | Pimoroni, Amazon | Generic resistor | For status LED |

**Subtotal**: £12.30-22.20

### Wiring & Connectors

| Component | Specification | Quantity | Unit Price (£) | Supplier | Part Number | Notes |
|-----------|--------------|----------|----------------|----------|-------------|-------|
| Jumper Wires | M-M, M-F, F-F pack | 1 pack | £4-6 | Amazon, Pimoroni | 120pc jumper wire set | Various lengths |
| Breadboard | 830 tie-points | 1 | £4-6 | Amazon, Pimoroni | Generic breadboard | Or use PCB for final |
| Terminal Blocks | Screw terminal 2-pin | 5 | £0.50-1 | Amazon, Pimoroni | 5.08mm pitch | For pump connections |
| Heat Shrink Tubing | Assorted sizes | 1 pack | £3-5 | Amazon | Heat shrink kit | For wire protection |
| Wire | 22 AWG solid core | 1 spool | £5-8 | Amazon, Pimoroni | Hookup wire spool | Red and black |

**Subtotal**: £16.50-26

### Enclosure & Structure

| Component | Specification | Quantity | Unit Price (£) | Supplier | Part Number | Notes |
|-----------|--------------|----------|----------------|----------|-------------|-------|
| Enclosure Box | Clear acrylic/plastic | 1 | £15-30 | Amazon, Hobbycraft | A4 storage box | Educational visibility |
| Mounting Hardware | M3 screws/standoffs | 1 pack | £5-8 | Amazon, Pimoroni | M3 kit | For RPi and components |
| Cable Ties | Various sizes | 1 pack | £3-5 | Amazon, Screwfix | Cable tie assortment | Cable management |
| Velcro Strips | Self-adhesive | 1 pack | £3-5 | Amazon | Velcro dots/strips | For component mounting |

**Subtotal**: £26-48

### Optional Components (Enhanced Educational Features)

| Component | Specification | Quantity | Unit Price (£) | Supplier | Part Number | Notes |
|-----------|--------------|----------|----------------|----------|-------------|-------|
| Flow Sensors | Liquid flow meter | 3 | £5-8 | Amazon | YF-S201 flow sensor | Measure actual volumes |
| Pressure Sensor | BMP280 I2C | 1 | £3-5 | Pimoroni | BMP280 breakout | Environmental monitoring |
| RGB Status LED | WS2812 or NeoPixel | 1 | £2-4 | Pimoroni, Adafruit | NeoPixel stick | Better visual feedback |
| Buzzer | 5V active buzzer | 1 | £1-2 | Amazon, Pimoroni | Active buzzer 5V | Audio feedback |
| Real-Time Clock | DS3231 I2C RTC | 1 | £3-5 | Amazon, Pimoroni | DS3231 RTC module | Log timestamps |

**Subtotal (Optional)**: £14-24

## UK Supplier Directory

### Primary Suppliers

1. **Pimoroni** (https://shop.pimoroni.com)
   - UK-based, excellent for Raspberry Pi and breakout boards
   - Fast shipping, educational discounts available
   - High-quality components

2. **The Pi Hut** (https://thepihut.com)
   - Raspberry Pi official reseller
   - Large electronics selection
   - UK warehouse

3. **Amazon UK** (https://amazon.co.uk)
   - Fast Prime shipping
   - Wide component selection
   - Variable quality - check reviews

4. **RS Components** (https://uk.rs-online.com)
   - Professional components
   - Educational accounts available
   - Bulk discounts

5. **CPC Farnell** (https://cpc.farnell.com)
   - Educational supplier
   - Wide electronics range
   - School accounts

### Specialty Suppliers

- **Hobbycraft** - Enclosures, craft materials
- **IKEA** - Food-safe containers
- **Screwfix** - Hardware, cable management

## Total Cost Breakdown

| Category | Minimum (£) | Maximum (£) | Notes |
|----------|-------------|-------------|-------|
| Core Components | 71 | 85 | RPi, SD, PSU |
| Pumps & Liquids | 66 | 117 | 3× pumps, tubing, containers |
| Electronics | 20 | 31 | Relays, power supplies |
| Sensors & Display | 12 | 22 | Temp sensor, LCD, buttons |
| Wiring & Connectors | 17 | 26 | Breadboard, wires, terminals |
| Enclosure | 26 | 48 | Box, mounting hardware |
| **TOTAL (Basic)** | **£212** | **£329** | Minimum viable prototype |
| Optional Components | 14 | 24 | Enhanced features |
| **TOTAL (Enhanced)** | **£226** | **£353** | With optional sensors |

### Budget Notes

- **Prototype Budget**: £212-329 (basic functional system)
- **Enhanced Budget**: £226-353 (with optional features)
- **Educational Bulk**: Additional 10-20% for multiple units
- **Original Estimate**: £500-750 includes contingency, shipping, consumables

### Cost Reduction Strategies

1. **Use Generic Components**: Amazon generics vs. branded (save 20-30%)
2. **Educational Discounts**: Pimoroni, RS, CPC offer school/education pricing
3. **Bulk Orders**: Buy multiple units together
4. **Reuse Hardware**: Existing Raspberry Pi, SD cards
5. **3D Print Enclosure**: Instead of buying (if printer available)

## Shopping List Template

**Quick Copy-Paste List for Ordering:**

```
Core:
[ ] Raspberry Pi 4 (4GB) - Pimoroni
[ ] 32GB MicroSD - Amazon
[ ] RPi Power Supply - Pimoroni

Pumps:
[ ] 3× 12V Peristaltic Pumps - Amazon/eBay
[ ] 3m Silicone Tubing (4mm) - Amazon
[ ] 3× Food-safe containers - IKEA

Electronics:
[ ] 3-Channel Relay Module - Amazon
[ ] 12V 2A Power Supply - Amazon
[ ] TMP102 Temp Sensor - Pimoroni
[ ] 16×2 LCD with I2C - Amazon
[ ] Emergency Stop Button - Amazon
[ ] Red LED + 220Ω resistor - Pimoroni

Wiring:
[ ] Jumper wire set - Amazon
[ ] Breadboard - Pimoroni
[ ] Terminal blocks (5×) - Amazon
[ ] Heat shrink kit - Amazon
[ ] 22 AWG wire (red/black) - Amazon

Structure:
[ ] Clear storage box - Amazon/Hobbycraft
[ ] M3 hardware kit - Amazon
[ ] Cable ties - Screwfix
[ ] Velcro strips - Amazon
```

## Assembly Time Estimate

- **Electrical Assembly**: 2-3 hours
- **Mechanical Assembly**: 1-2 hours
- **Software Setup**: 1-2 hours
- **Testing & Calibration**: 2-3 hours
- **Total**: 6-10 hours for first unit

## Next Steps

1. Review BOM and adjust based on available hardware
2. Create purchase order (see shopping list above)
3. Review `wiring_diagram.md` for assembly guidance
4. Follow `assembly_instructions.md` for step-by-step build
5. Configure software per `config.toml.example`

## Notes for Workshops

If building multiple units for workshops:

- Order 10% extra components (spares)
- Pre-cut wires to standard lengths
- Pre-program SD cards with OS
- Label all components in individual kits
- Prepare assembly guide handouts

## Sustainability Considerations

- **Food-Safe Materials**: Ensure all liquid-contact parts are food-safe
- **Reusability**: Design for disassembly and component reuse
- **E-Waste**: Plan for responsible disposal/recycling
- **Energy**: Calculate power consumption, use efficient PSUs
