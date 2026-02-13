# HotChocolaBot Workshop Curriculum

**Program**: Educational Robotics for Systems Thinking
**Duration**: 2.5 hours per session
**Target Audience**: Ages 12-18 (Junior: 12-14, Senior: 15-18)
**Class Size**: 8-15 students
**Instructor Ratio**: 1 instructor, 1 assistant (recommended)

## Learning Objectives

By the end of this workshop, students will be able to:

### Knowledge
- Identify components of an embedded system (sensors, actuators, controllers)
- Explain the concept of abstraction layers in software
- Describe safety-critical system design principles
- Understand state machines and their role in system control

### Skills
- Apply reverse engineering methodology to unknown systems
- Diagram system architecture and component interactions
- Propose and test system modifications
- Debug simple hardware/software integration issues

### Attitudes
- Appreciate complexity in everyday automated systems
- Develop curiosity-driven investigation habits
- Collaborate in technical problem-solving
- Think critically about system design trade-offs

## Workshop Structure

### Session Format (2.5 hours)

```
00:00-00:15  Introduction & Ice Breaker (15 min)
00:15-00:30  Mystery Box Challenge (15 min)
00:30-01:15  Guided Exploration (45 min)
01:15-01:30  BREAK (15 min)
01:30-02:15  Deep Dive Investigation (45 min)
02:15-02:30  Reflection & Discussion (15 min)
```

---

## Detailed Session Plan

### Part 1: Introduction & Ice Breaker (15 min)

#### Goals
- Build rapport with students
- Assess prior knowledge
- Set expectations for hands-on learning

#### Activities

**1. Welcome Circle** (5 min)
- Instructor introduces self and workshop goals
- Each student shares: Name + "One machine you use daily"
- Note common themes (phones, computers, appliances)

**2. Systems Thinking Warm-Up** (10 min)

*Activity: "Coffee Machine Reverse Engineering"*

Ask students:
> "If you found a coffee machine with no labels or instructions, how would you figure out how it works?"

Collect answers on whiteboard:
- Look for buttons/switches
- Try pressing things
- Look inside
- Check for wires/connections
- Test with water

Instructor summarizes: **"This is reverse engineering! Today we'll use these exact skills on our hot chocolate bot."**

---

### Part 2: Mystery Box Challenge (15 min)

#### Goals
- Activate curiosity and hypothesis generation
- Practice observation without touching
- Develop initial mental model

#### Setup
- HotChocolaBot covered with cloth
- "DO NOT TOUCH" sign visible
- Only external wires/tubes visible

#### Activities

**1. Observation Round** (5 min)

Students circulate around covered bot. They can:
- Look at visible parts (tubes, wires, power cables)
- Listen (is it making sounds?)
- Smell (are ingredients detectable?)
- **NOT touch or lift cover**

**2. Hypothesis Generation** (5 min)

In small groups (3-4 students), discuss:
1. What do you think this machine does?
2. What components might be inside?
3. How might they work together?

Each group records predictions on worksheet (Appendix A).

**3. Reveal & First Demo** (5 min)

- Remove cover: *"Meet HotChocolaBot!"*
- Explain basic function: "Makes hot chocolate automatically"
- Run **one** complete dispense cycle
- Students observe, take notes

---

### Part 3: Guided Exploration (45 min)

#### Goals
- Identify and document system components
- Understand component functions
- Map system architecture

#### Activities

**1. Hardware Scavenger Hunt** (15 min)

Distribute "Component Detective Sheet" (Appendix B).

Students work in pairs to find and identify:
- [ ] The "brain" (Raspberry Pi)
- [ ] The pumps (3 peristaltic pumps)
- [ ] The switches (relays)
- [ ] The sensor (temperature)
- [ ] The display (LCD)
- [ ] The emergency stop button
- [ ] The power supplies

For each component, record:
- What it looks like
- Where it's located
- What you think it does
- How it connects to other parts

**2. Component Function Discussion** (10 min)

Gather class. For each component found:

*Example: Pumps*
- Instructor: "What did you find that moves the liquids?"
- Students: "The pumps!"
- Instructor: "How do you think they work?"
- Demonstrate pump action (manual rotation if possible)
- Explain: Peristaltic pumps squeeze tubes to push liquid

Repeat for each major component.

**3. System Mapping Activity** (20 min)

Students create **system diagrams** showing:
- All components (boxes)
- Connections between them (arrows)
- Flow of information (dotted lines)
- Flow of power (solid lines)
- Flow of ingredients (colored lines)

Provide large paper and colored markers.

*Instructor circulates, asks questions:*
- "Why does the Raspberry Pi connect to the pumps?"
- "What tells the pump when to start?"
- "Where does the power come from?"

**Example student diagram:**
```
[Container] ---liquid---> [Pump] ---controlled by---> [Relay]
                                                         ^
                                                         |
                                              [Raspberry Pi]
                                                    ^
                                                    |
                                            [Temperature Sensor]
                                            [Emergency Stop]
                                            [Display]
```

---

### Part 4: BREAK (15 min)

- Snacks and drinks (ideally hot chocolate made by the bot!)
- Informal discussion about robotics, engineering careers
- Students can sketch ideas for improvements

---

### Part 5: Deep Dive Investigation (45 min)

#### Goals
- Understand software control logic
- Explore safety systems
- Investigate design decisions

#### Station Rotation (3 stations × 15 min each)

Divide class into 3 groups. Rotate through stations.

#### **Station A: Software Exploration**

*Focus: How does code control hardware?*

**Activities:**
1. View simplified code snippets (pseudo-code):
   ```
   FUNCTION make_hot_chocolate():
       CHECK temperature is safe
       DISPLAY "Adding milk..."
       RUN milk_pump FOR 5 seconds
       DISPLAY "Adding cocoa..."
       RUN cocoa_pump FOR 2 seconds
       DISPLAY "Adding sugar..."
       RUN sugar_pump FOR 1 second
       DISPLAY "Enjoy!"
   ```

2. Students trace execution step-by-step
3. Discussion questions:
   - Why check temperature first?
   - What happens if we swap the order?
   - How does the code "talk" to the pump?

**Challenge**: Modify recipe (change timings) on paper

#### **Station B: Safety Systems**

*Focus: Why over-engineered? Safety!*

**Activities:**
1. Identify safety features:
   - Emergency stop button
   - Temperature limits
   - Maximum pump runtime
   - State machine (explain with diagram)

2. "What could go wrong?" brainstorm:
   - Pump runs too long → overflow
   - Temperature too high → burns
   - Emergency stop → immediate shutdown

3. Test emergency stop:
   - Instructor runs dispense cycle
   - Student presses E-stop
   - Observe immediate pump shutdown
   - Discuss why this matters

**Challenge**: Design a new safety check (e.g., liquid level sensor)

#### **Station C: Engineering Decisions**

*Focus: Why is it built this way?*

**Activities:**
1. Present design alternatives:

   *Question: Why peristaltic pumps instead of syringes?*
   - Discuss pros/cons of each
   - Precision vs. cost vs. food safety

   *Question: Why Raspberry Pi instead of Arduino?*
   - Computing power
   - Ease of programming
   - Cost

   *Question: Why 3 separate pumps instead of pre-mixing?*
   - Flexibility (different recipes)
   - Maintenance (clean one ingredient line)
   - Educational value (observe each step)

2. Students debate trade-offs

**Challenge**: Propose one design change and justify it

---

### Part 6: Reflection & Discussion (15 min)

#### Goals
- Consolidate learning
- Connect to broader concepts
- Inspire further investigation

#### Activities

**1. Gallery Walk** (5 min)
- Display all student system diagrams
- Students circulate, add sticky notes with:
  - ⭐ "I learned..."
  - ❓ "I wonder..."
  - 💡 "Idea for improvement..."

**2. Group Reflection** (5 min)

Discuss as class:
- "What surprised you most about HotChocolaBot?"
- "Where else do you see similar systems?" (vending machines, automated factories, cars)
- "What would you change if you built version 2?"

**3. Closing Challenge** (5 min)

*Optional homework/extension:*
- Research one component (e.g., "How do relays work?")
- Find a machine at home and reverse-engineer it (toaster, microwave, etc.)
- Design your own automated system on paper

Distribute post-workshop survey (Appendix C).

---

## Materials Required

### Per Workshop

- [ ] 1× HotChocolaBot (fully assembled and tested)
- [ ] Ingredients (cocoa, milk, sugar for ~20 servings)
- [ ] Power: 2× outlets (5V + 12V)
- [ ] Large paper (A3 or flip chart) for diagramming
- [ ] Colored markers (red, blue, black, green)
- [ ] Printed worksheets (Appendices A, B, C)
- [ ] Laptop with code repository (for Station A)
- [ ] Camera/phone for documentation
- [ ] Cleaning supplies (spills happen!)

### Per Student

- [ ] Pencil/pen
- [ ] Notebook or worksheet packet
- [ ] Safety glasses (if allowing close inspection)
- [ ] Name tag

---

## Differentiation Strategies

### For Younger Students (12-14)

- Use more analogies (pumps = "squeezy bottles")
- Shorter investigation time, more guided questions
- Simplify code examples (flowcharts instead of pseudo-code)
- Focus on concrete observations over abstract concepts

### For Older Students (15-18)

- Introduce formal concepts (state machines, abstraction layers)
- Show actual Rust code snippets
- Discuss safety verification and formal methods
- Challenge: "How would you write tests for this system?"

### For Mixed Abilities

- Pair stronger students with those needing support
- Offer tiered challenges (basic, intermediate, advanced)
- Allow multiple expression modes (drawing, writing, building)

---

## Assessment

### Formative Assessment (During Workshop)

**Observe:**
- Are students asking questions?
- Can they identify component functions?
- Do diagrams show understanding of connections?
- Are they engaging in design discussions?

**Listen for:**
- Use of technical vocabulary (sensor, actuator, controller)
- Causal reasoning ("The pump runs BECAUSE the Pi sends a signal")
- Systems thinking ("If we change X, then Y will happen")

### Summative Assessment (End of Workshop)

**Pre/Post Survey** (Appendix C):
- Knowledge questions (multiple choice)
- Attitude questions (Likert scale)
- Open-ended reflection

**Portfolio Artifacts:**
- System diagram
- Component detective sheet
- Design proposal sketch

---

## Instructor Notes

### Preparation (1 week before)

- [ ] Test HotChocolaBot thoroughly
- [ ] Prepare ingredients (check expiry dates)
- [ ] Print all worksheets (1 per student + extras)
- [ ] Set up station materials
- [ ] Charge any devices
- [ ] Confirm venue logistics (tables, chairs, power)

### Day-of Setup (30 min before)

- [ ] Arrive early, test bot one more time
- [ ] Arrange tables for group work
- [ ] Set up 3 station areas
- [ ] Display welcome slide/poster
- [ ] Prepare emergency contact info
- [ ] Brew coffee (for instructors!)

### During Workshop

**Do:**
- Encourage questions, even "silly" ones
- Validate multiple approaches to problems
- Share enthusiasm for engineering
- Take photos (with permission) for documentation
- Note any technical issues for improvement

**Don't:**
- Give away answers too quickly - guide with questions
- Assume prior knowledge - define technical terms
- Rush - let students struggle productively
- Ignore safety concerns - model safe practices

### After Workshop

- [ ] Clean and store bot
- [ ] Collect and review student work
- [ ] Send follow-up resources to students (optional)
- [ ] Document lessons learned
- [ ] Update curriculum based on feedback

---

## Troubleshooting

### "Students aren't engaging"

- Make it competitive: "Which group can find all components first?"
- Add storytelling: "This bot has a secret mission..."
- Let them press buttons/trigger actions
- Break into smaller groups

### "Bot malfunctions during demo"

- Use it as a learning opportunity: "What do we do when systems fail?"
- Demonstrate debugging process
- Have backup video of working bot
- Pivot to diagram-based exploration

### "Too easy/too hard"

- Adjust on the fly: skip sections or go deeper
- Offer extension challenges to fast finishers
- Provide more scaffolding to struggling students
- Revisit learning objectives - are we meeting them?

### "Running out of time"

- Skip one rotation station (combine B & C)
- Shorten break to 10 minutes
- Send reflection survey home as homework
- Prioritize Parts 2-3 (exploration) over Part 5 (deep dive)

---

## Extension Activities

### For Follow-Up Workshops

1. **Build Your Own** (Advanced, 6-8 weeks):
   - Students assemble their own simplified bots
   - Teach soldering, wiring, basic programming
   - Culminates in demo day

2. **Programming Workshop**:
   - Teach Rust basics
   - Students modify HotChocolaBot code
   - Add new features (custom recipes, LED patterns)

3. **Design Thinking Sprint**:
   - Students redesign bot for different use case
   - Prototype with cardboard and markers
   - Present to peers

### Independent Learning

- GitHub repository exploration
- Online courses (Raspberry Pi, embedded systems)
- Science fair project based on automated systems
- Mentorship with MechCC members

---

## Appendices

### Appendix A: Mystery Box Prediction Worksheet

```
HOTCHOCOLABOT MYSTERY BOX

Your Name: ________________  Partner: ________________

WITHOUT TOUCHING THE BOT, OBSERVE:

What do you SEE?
□ Wires (what color?): _________________________________
□ Tubes (how many?): __________________________________
□ Buttons/Lights: _____________________________________
□ Other: ______________________________________________

What do you HEAR?
_______________________________________________________

What do you SMELL?
_______________________________________________________

PREDICTIONS:

1. What does this machine do?
_______________________________________________________

2. What's inside the box? (Draw or list)
_______________________________________________________

3. How does it work? (Your theory)
_______________________________________________________
_______________________________________________________

AFTER THE REVEAL:

Were your predictions correct? What surprised you?
_______________________________________________________
```

### Appendix B: Component Detective Sheet

```
COMPONENT DETECTIVE SHEET

Find each component and fill in the details!

┌──────────────────────────────────────────────────┐
│ THE BRAIN                                        │
│ Name: _________________ (Hint: Raspberry _____)  │
│ What it does: _________________________________  │
│ Connected to: _________________________________  │
│ Draw it:                                         │
│                                                  │
└──────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────┐
│ THE PUMPS (How many? ____)                       │
│ What they do: __________________________________  │
│ How they work: _________________________________  │
│ What controls them: ____________________________  │
└──────────────────────────────────────────────────┘

[Continue for: SENSORS, DISPLAY, EMERGENCY STOP, POWER SUPPLY]

BONUS CHALLENGE:
Draw arrows showing how information flows between components!
```

### Appendix C: Pre/Post Workshop Survey

See separate file: `education/assessments/workshop_survey.md`

---

## Contact & Support

**Workshop Facilitators:**
- Primary Contact: [Insert instructor email]
- Technical Support: MechCC team

**Resources:**
- Code Repository: https://github.com/Hyperpolymath/the-hotchocolabot
- Issue Tracker: [Report problems or suggestions]
- MechCC Website: [Link to UAL Creative Communities]

---

**Version History:**
- v1.0 (2024-11): Initial curriculum design
- Future: Incorporate feedback from pilot workshops

**License:** CC BY-SA 4.0 (Share and adapt with attribution)
